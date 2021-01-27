
{-# LANGUAGE
  ForeignFunctionInterface, TypeSynonymInstances,
  ScopedTypeVariables, FlexibleInstances, FlexibleContexts,
  UndecidableInstances
#-}

{- |

Interact with embedded Perl interpreter.

= Interpreter instance

Pretty much any function in this module will only operated correctly if a
properly initialized /interpreter instance/ exists  -- that is,
the function 'perl5_init' has been called. You don't
have to /pass/ the resulting 'Interpreter' to the functions, typically --
rather, calling 'perl5_init' has the side effect
of initializing various global variables needed by Perl.
Normally, only one interpreter instance can exist at a time
(unless your Perl library has been specially compiled to allow for multiple
instances -- see <https://perldoc.perl.org/perlembed#Maintaining-multiple-interpreter-instances perlembed>).

For convenience, a 'bracket'-like function is provided, 'withPerl5', which creates
an interpreter using 'perl5_init', cleans up afterwards using
'perl_destruct', and runs your 'IO' actions in between.

Calling 'withPerl5' creates an 'Interpreter' instance that is
equivalent to running

@
perl -e ""
@

at the command-line.

-}



module Language.Perl5
    (
      Context(..)
    , ToSV(..)
    , FromSV(..)
    , withPerl5
    , callSub,      (.:), (.!)
    , callMethod,   (.$), (.$!)
    , eval
    , eval_
    , SV
    , use
    )
    where


import Control.Exception (bracket, throwIO, Exception(..))

import Data.Dynamic (toDyn)
import Data.Int
import Data.List (intersperse, intercalate)

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Language.Perl5.Internal
import Language.Perl5.Internal.Types

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----
-- safely running Perl things

-- | Run a computation within the context of a Perl 5 interpreter.
withPerl5 :: IO a -> IO a
withPerl5 f =
    withCString "-e" $ \prog -> withCString "" $ \cstr ->
        withArray [prog, prog, cstr] $ \argv ->
            bracket (perl5_init 3 argv) (\interp -> do
                perl_destruct interp
                perl_free interp) (const f)


-----
-- marshalling to and from Perl

-- | Data types that can be cast to a Perl 5 value (SV).
class ToSV a where
    toSV :: a -> IO SV
-- TODO: shift 'primitive marshalling' into its own module.

-- | Data types that can be cast from a Perl 5 value (SV).
class FromSV a where
    fromSV :: SV -> IO a

instance ToSV SV where toSV = return
instance FromSV SV where fromSV = return

instance ToSV () where
    toSV _ = perl5_sv_undef
instance FromSV () where
    fromSV x = seq x (return ())

instance ToSV String where
    toSV str = withCStringLen str $ \(cstr, len) ->
        perl5_newSVpvn cstr (toEnum len)

instance FromSV String where
    fromSV sv = do
        cstr <- perl5_SvPV sv
        peekCString cstr

-- | For convenience, a 'ToSV' instance is provided for 'Int's.
-- However, it's lossy: actually, a Perl 'SV' will only fit
-- an 'Int32'.
instance ToSV Int where
    toSV = perl5_newSViv . toEnum

instance FromSV Int where
    fromSV = fmap fromEnum . perl5_SvIV

instance ToSV Int32 where
    toSV = toSV . toInt
      where
        toInt :: Int32 -> Int
        toInt = fromIntegral

instance FromSV Int32 where
    fromSV = fmap fromInt . fromSV
      where
        fromInt :: Int -> Int32
        fromInt = fromIntegral

instance ToSV Double where
    toSV = perl5_newSVnv . realToFrac

instance FromSV Double where
    fromSV = fmap realToFrac . perl5_SvNV

instance FromSV Bool where
    fromSV = perl5_SvTRUE

instance ToSV Bool where
    toSV True = perl5_sv_yes
    toSV False = perl5_sv_no


-- -- ---
--  CVs -- Code Values


class ToCV a where
    toCV :: a -> Int -> IO SV

instance {-# OVERLAPS #-} ToSV a => ToCV a where
    toCV x _ = toSV x

----------
-- Arg conversion

class ToArgs a where
    toArgs :: a -> IO [SV]

class FromArgs a where
    fromArgs :: [SV] -> IO a
    contextOf :: a -> Context
    contextOf _ = ScalarCtx

instance ToArgs [String] where
    toArgs = mapM toSV

instance FromArgs [String] where
    fromArgs = mapM fromSV

instance {- OVERLAPS -} FromArgs () where
    fromArgs _ = return ()
    contextOf _ = VoidCtx

instance ToArgs () where
    toArgs _ = return []

instance {-# OVERLAPS #-} ToSV a => ToArgs a where
    toArgs = fmap (:[]) . toSV

instance (ToSV a, ToSV b) => ToArgs (a, b) where
    toArgs (x, y) = do
        x' <- toSV x
        y' <- toSV y
        return [x', y']

instance {-# OVERLAPS #-} FromSV a => FromArgs a where
    fromArgs [] = error "Can't convert an empty return list!"
    fromArgs (x:_) = fromSV x
    contextOf _ = ScalarCtx

instance (FromSV a, FromSV b) => FromArgs (a, b) where
    fromArgs [] = error "Can't convert an empty return list!"
    fromArgs [_] = error "Can't convert a single  return list!"
    fromArgs (x:y:_) = do
        x' <- fromSV x
        y' <- fromSV y
        return (x', y')
    contextOf _ = ListCtx



instance ToArgs [SV] where
    toArgs = return
instance FromArgs [SV] where
    fromArgs = return

instance ToArgs a => ToSV (IO a) where
  toSV f = do
      sp <- newStablePtr $ \_ _ -> do
        svs <- toArgs =<< f
        mkSVList svs
      perl5_make_cv sp

instance {-# OVERLAPS #-} (ToArgs a, FromArgs r) => ToSV (r -> IO a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            args'   <- fromArgs =<< asSVList args
            svs     <- toArgs =<< f args'
            mkSVList svs
        perl5_make_cv sp

instance (ToArgs a, FromArgs (r1, r2)) => ToSV (r1 -> r2 -> IO a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            (a1, a2)    <- fromArgs =<< asSVList args
            svs         <- toArgs =<< f a1 a2
            mkSVList svs
        perl5_make_cv sp

instance {-# OVERLAPS #-} (ToArgs a, FromArgs r) => ToSV (r -> a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            args'   <- fromArgs =<< asSVList args
            svs     <- toArgs $ f args'
            mkSVList svs
        perl5_make_cv sp

instance (ToArgs a, FromArgs (r1, r2)) => ToSV (r1 -> r2 -> a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            (a1, a2)    <- fromArgs =<< asSVList args
            svs         <- toArgs $ f a1 a2
            mkSVList svs
        perl5_make_cv sp




-- un-befunge the result of calling one of our eval/apply
-- functions. i.e., any functions whose return value is
-- ultimately given to us by @perl5_return_conv@ from
-- @cbits/p5embed.c@.
returnPerl5 :: forall a. FromArgs a => Ptr SV -> IO a
returnPerl5 rv = do
    res  <- svEither rv
    case res of
      Left [err]   -> throwIO (toException $ toDyn err)
      Left (_:x:_) -> error =<< fromSV x
      Right r      -> fromArgs r

------
-- --- eval funcs


-- | Evaluate a snippet of Perl 5 code.
eval :: forall a. FromArgs a => String -> IO a
eval str = withCStringLen str $ \(cstr, len) -> do
    rv  <- perl5_eval cstr (toEnum len) (numContext $ contextOf (undefined :: a))
    returnPerl5 rv

-- | Same as 'eval' but always in void context.
eval_ :: String -> IO ()
eval_ str = eval str

-- | Call a Perl 5 subroutine.
callSub :: forall s a r. (ToCV s, ToArgs a, FromArgs r) => s -> a -> IO r
callSub sub args = do
    args'   <- toArgs args
    sub'    <- toCV sub (length args')
    rv      <- withSVArray args' $ \argsPtr ->
        perl5_apply sub' (SV nullPtr) argsPtr (numContext $ contextOf (undefined :: r))
    returnPerl5 rv

-- | Call a Perl 5 method.
callMethod :: forall i m a r. (ToSV i, ToSV m, ToArgs a, FromArgs r) => i -> m -> a -> IO r
callMethod inv meth args = do
    inv'    <- toSV inv
    args'   <- toArgs args
    sub'    <- toSV meth
    rv      <- withSVArray args' $ \argsPtr ->
        perl5_apply sub' inv' argsPtr (numContext $ contextOf (undefined :: r))
    returnPerl5 rv

-- aliases for callSub and callMethod

(.:) :: (ToCV sub, ToArgs args, FromArgs ret) => sub -> args -> IO ret
(.:) = callSub

(.!) :: (ToCV sub, ToArgs args) => sub -> args -> IO ()
(.!) = callSub


(.$) :: (ToSV meth, ToArgs args, FromArgs ret) => SV -> meth -> args -> IO ret
(.$) = callMethod

(.$!) :: (ToSV meth, ToArgs args) => SV -> meth -> args -> IO ()
(.$!) = callMethod

-- utility functions


-- | Use a module.  Returns a prototype object representing the module.
use :: String -> IO SV
use m = eval $ "use " ++ m ++ "; q[" ++ m ++ "]"

-- instances that call (indirectly) eval

instance FromArgs r => FromSV (IO r) where
    -- Callback code.
    fromSV x =
        return $ callSub x ()

instance (ToArgs a, FromArgs r) => FromSV (a -> IO r) where
    -- Callback code.
    fromSV x =
        return $ callSub x

instance (ToArgs a, ToArgs b, FromArgs r) => FromSV (a -> b -> IO r) where
    -- Callback code.
    fromSV x =
        -- First we obtain x as a CV
        return $ \arg1 arg2 -> do
            as1  <- toArgs arg1
            as2  <- toArgs arg2
            callSub x (as1 ++ as2)

instance {-# OVERLAPS #-} ToCV String where
  toCV  sub count = do
      cv <- withCString sub perl5_get_cv
      if unSV cv /= nullPtr then return cv else do
           let prms = map (\i -> "$_[" ++ show i ++ "]") [0 .. count-1]
           eval ("sub { " ++ sub ++ "(" ++ intercalate ", " prms ++ ") }")

-- hsPerl5Apply -- a function we expose from Haskell
-- to C. (used in cbits/p5embed.c)

hsPerl5Apply :: StablePtr Callback -> Ptr SV -> CInt -> IO (Ptr SV)
hsPerl5Apply ptr args cxt = do
    f <- deRefStablePtr ptr
    f args cxt

foreign export ccall "hsPerl5Apply"
    hsPerl5Apply :: StablePtr Callback -> Ptr SV -> CInt -> IO (Ptr SV)



