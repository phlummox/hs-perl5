
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
the function 'hsperl_init' has been called. You don't
have to /pass/ the resulting 'Interpreter' to the functions, typically --
rather, calling 'hsperl_init' has the side effect
of initializing various global variables needed by Perl.
Normally, only one interpreter instance can exist at a time
(unless your Perl library has been specially compiled to allow for multiple
instances -- see <https://perldoc.perl.org/perlembed#Maintaining-multiple-interpreter-instances perlembed>).

For convenience, a 'bracket'-like function is provided, 'withPerl', which creates
an interpreter using 'hsperl_init', cleans up afterwards using
'perl_destruct', and runs your 'IO' actions in between.

Calling 'withPerl' creates an 'Interpreter' instance that is
equivalent to running

@
perl -e ""
@

at the command-line.

-}



module Language.Perl
    (
    -- * Perl calling context
      Context(..)
    -- * Major types
    , SV
    -- * Marshal values between Haskell and Perl
    , ToSV(..)
    , FromSV(..)
    -- * Safely run Perl things
    , withPerl
    -- * evaluate in a Perl context
    , callSub,      (.:), (.!)
    , callMethod,   (.$), (.$!)
    , eval
    , eval_
    -- * utility functions
    , use
    )
    where

import Control.Concurrent
import Control.Exception (bracket, throwIO, Exception(..))
import Control.Monad

import qualified Data.ByteString as BS
import           Data.Dynamic (toDyn)
import           Data.Int
import           Data.List ( intercalate)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Language.Perl.Internal
import Language.Perl.Internal.Types

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

----
-- safely running Perl things

-- | Run a computation within the context of a Perl 5 interpreter.
withPerl :: IO a -> IO a
withPerl f =
    withCString "-e" $ \prog -> withCString "" $ \cstr ->
        withArray [prog, prog, cstr] $ \argv ->
            bracket (acquire argv) release between
  where
    acquire argv   = hsperl_init 3 argv
    release interp = do -- currently, interpreter leaves data lying around
                         -- after destruction -- "set_destruct_level"
                         -- ensures it is cleared/deallocated
                         hsperl_set_destruct_level
                         void $ perl_destruct interp
                         perl_free interp
    -- we should be running in a bound thread, since interpreter
    -- is not thread-safe
    between _interp = (if rtsSupportsBoundThreads then runInBoundThread else id)
                         f

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
    toSV _ = hsperl_sv_undef
instance FromSV () where
    fromSV x = seq x (return ())

instance ToSV String where
    toSV str = withCStringLen str $ \(cstr, len) ->
        hsperl_newSVpvn cstr (toEnum len)

instance FromSV String where
    fromSV sv = do
        cstr <- hsperl_SvPV sv
        peekCString cstr

instance ToSV Text where
  toSV txt = BS.useAsCStringLen (T.encodeUtf8 txt) $ \(cstr, len) -> do
               sv <- hsperl_newSVpvn cstr (toEnum len)
               hsperl_SvUTF8_on sv
               return sv

instance FromSV Text where
    fromSV sv =
          alloca $ \(lenPtr :: Ptr CSize) -> do
            cStr <- hsperl_sv_2pvutf8 sv lenPtr
            len  <- peek lenPtr
            -- to do: unsafePackCStringLen is presumably faster
            T.decodeUtf8  <$> BS.packCStringLen (cStr, fromIntegral len)


-- | For convenience, a 'ToSV' instance is provided for 'Int's.
-- However, a Haskell 'Int' on your platform might not be the same size 
-- as Perl's integral type - for an exact
-- conversion, see the instance for 'IV'.
instance ToSV Int where
    toSV = hsperl_newSViv . toEnum

instance FromSV Int where
    fromSV = fmap fromEnum . hsperl_SvIV

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
    toSV = hsperl_newSVnv . realToFrac

instance FromSV Double where
    fromSV = fmap realToFrac . hsperl_SvNV

instance FromSV Bool where
    fromSV = hsperl_SvTRUE

instance ToSV Bool where
    toSV True = hsperl_sv_yes
    toSV False = hsperl_sv_no


-- -- ---
--  CVs -- Code Values

-- | convert to code value
class ToCV a where
    toCV :: a -> Int -> IO SV

instance {-# OVERLAPS #-} ToSV a => ToCV a where
    toCV x _ = toSV x

----------
-- Arg conversion

-- | argument conversion
class ToArgs a where
    toArgs :: a -> IO [SV]

-- | argument conversion
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
      hsperl_make_cv sp

instance {-# OVERLAPS #-} (ToArgs a, FromArgs r) => ToSV (r -> IO a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            args'   <- fromArgs =<< asSVList args
            svs     <- toArgs =<< f args'
            mkSVList svs
        hsperl_make_cv sp

instance (ToArgs a, FromArgs (r1, r2)) => ToSV (r1 -> r2 -> IO a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            (a1, a2)    <- fromArgs =<< asSVList args
            svs         <- toArgs =<< f a1 a2
            mkSVList svs
        hsperl_make_cv sp

instance {-# OVERLAPS #-} (ToArgs a, FromArgs r) => ToSV (r -> a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            args'   <- fromArgs =<< asSVList args
            svs     <- toArgs $ f args'
            mkSVList svs
        hsperl_make_cv sp

instance (ToArgs a, FromArgs (r1, r2)) => ToSV (r1 -> r2 -> a) where
  toSV f = do
        sp <- newStablePtr $ \args _ -> do
            (a1, a2)    <- fromArgs =<< asSVList args
            svs         <- toArgs $ f a1 a2
            mkSVList svs
        hsperl_make_cv sp




-- un-befunge the result of calling one of our eval/apply
-- functions. i.e., any functions whose return value is
-- ultimately given to us by @hsperl_return_conv@ from
-- @cbits/p5embed.c@.
returnPerl :: forall a. FromArgs a => Ptr SV -> IO a
returnPerl rv = do
    res  <- svEither rv
    case res of
      Left [err]   -> throwIO (toException $ toDyn err)
      Left (_:x:_) -> error =<< fromSV x
      Right r      -> fromArgs r
      _            -> error "unexpected return value"

------
-- --- eval funcs


-- | Evaluate a snippet of Perl 5 code.
eval :: forall a. FromArgs a => String -> IO a
eval str = withCStringLen str $ \(cstr, len) ->
  hsperl_eval cstr (toEnum len) (numContext $ contextOf (undefined :: a)) returnPerl

-- | Same as 'eval' but always in void context.
eval_ :: String -> IO ()
eval_ str = eval str

-- | Call a Perl 5 subroutine.
callSub :: forall s a r. (ToCV s, ToArgs a, FromArgs r) => s -> a -> IO r
callSub sub args = do
    args'   <- toArgs args
    sub'    <- toCV sub (length args')
    withSVArray args' $ \argsPtr ->
      hsperl_apply sub' (SV nullPtr) argsPtr (numContext $ contextOf (undefined :: r)) returnPerl

-- | Call a Perl 5 method.
callMethod :: forall i m a r. (ToSV i, ToSV m, ToArgs a, FromArgs r) => i -> m -> a -> IO r
callMethod inv meth args = do
    inv'    <- toSV inv
    args'   <- toArgs args
    sub'    <- toSV meth
    withSVArray args' $ \argsPtr ->
      hsperl_apply sub' inv' argsPtr (numContext $ contextOf (undefined :: r)) returnPerl

-- aliases for callSub and callMethod

-- | alias for 'callSub'
(.:) :: (ToCV sub, ToArgs args, FromArgs ret) => sub -> args -> IO ret
(.:) = callSub

-- | version of 'callSub' that returns no result
(.!) :: (ToCV sub, ToArgs args) => sub -> args -> IO ()
(.!) = callSub

-- | alias for 'callMethod'
(.$) :: (ToSV meth, ToArgs args, FromArgs ret) => SV -> meth -> args -> IO ret
(.$) = callMethod

-- | version of 'callMethod' that returns no result
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

-- NB: weird casting of CV to SV
instance {-# OVERLAPS #-} ToCV String where
  toCV  sub count = do
      cv <- withCString sub hsperl_get_cv
      if unSV cv /= nullPtr then return cv else do
           let prms = map (\i -> "$_[" ++ show i ++ "]") [0 .. count-1]
           eval ("sub { " ++ sub ++ "(" ++ intercalate ", " prms ++ ") }")

-- hsPerlApply -- a function we expose from Haskell
-- to C. (used in cbits/p5embed.c)

hsPerlApply :: StablePtr Callback -> Ptr SV -> CInt -> IO (Ptr SV)
hsPerlApply ptr args cxt = do
    f <- deRefStablePtr ptr
    f args cxt

foreign export ccall "hsPerlApply"
    hsPerlApply :: StablePtr Callback -> Ptr SV -> CInt -> IO (Ptr SV)



