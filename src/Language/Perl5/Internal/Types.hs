

{- |

Types for interfacing with an embedded Perl interpreter.

-}

module Language.Perl5.Internal.Types
  where

import Foreign
import Foreign.C.Types

import Language.Perl5.Internal.Constants


{-# ANN module ("HLint: ignore Missing NOINLINE pragma" :: String) #-}

-- | (pointer to a) Perl interpreter instance.
newtype Interpreter = Interpreter { unInterpreter :: Ptr Interpreter } deriving (Show, Eq)
-- | (pointer to a) scalar value.
newtype SV          = SV          { unSV          :: Ptr SV }          deriving (Show, Eq)


-- | type of a callback from Perl into Haskell.
type Callback = Ptr SV -> CInt -> IO (Ptr SV)

-- the downside of the "newtype X = X (Ptr X)"
-- approach is that working with *arrays* of ptrs-to-X becomes
-- more fiddly.
--
-- so we define a few helper functions for working with
-- arrays-of-ptrs-to-SVs.

-- | convert a NULL terminated "array of pointers" (ptr-to-ptr-to-SV)
-- to a list of SVs.
--
-- wrapper around "@peekArray nullPtr@"
asSVList :: Ptr SV -> IO [SV]
asSVList ptrs = do
  let ptrs' :: Ptr (Ptr SV)
      ptrs' = castPtr ptrs
  map SV <$> peekArray0 nullPtr ptrs'
  -- ... could generalize this, if we were
  -- willing to call unsafeCoerce instead of SV.

-- | advance one el, and get the "tail" of an "array of pointers".
--
-- wrapper around advancePtr 1 fed into peekArray0 nullPtr
svTail :: Ptr a -> IO [SV]
svTail ptrs = do
  let ptrs' :: Ptr (Ptr SV)
      ptrs' = castPtr ptrs
      ptrsTail = ptrs' `advancePtr` 1
  map SV <$> peekArray0 nullPtr ptrsTail

-- | return either the error list, or the result
-- list from an array of pointers.
svEither :: Ptr SV -> IO (Either [SV] [SV])
svEither ptrs = do
  errList <- asSVList ptrs
  if null errList
  then Right <$> svTail   ptrs
  else return $ Left errList

-- | Make a NULL-terminated array of SVs.
--
-- Wrapper around 'newArray0 nullPtr'
mkSVList :: [SV] -> IO (Ptr SV)
mkSVList svs = do
        arr <- newArray0 nullPtr (map unSV svs)
        let arr' :: Ptr SV
            arr' = castPtr arr
        return arr'

-- | temporarily marshal a list of SVs into a
-- NULL-terminated array, and perform some action with
-- them.
--
-- Wrapper around withArray0.
withSVArray :: [SV] -> (Ptr SV -> IO b) -> IO b
withSVArray svs f = withArray0 nullPtr svs' (f . castPtr)
  where
    svs' = map unSV svs


-- | Perl's calling context.
data Context = VoidCtx | ScalarCtx | ListCtx
  deriving (Eq, Show)

instance Enum Context where
  fromEnum = numContext
  toEnum n = case n of
    G_VOID    -> VoidCtx
    G_SCALAR  -> ScalarCtx
    G_ARRAY   -> ListCtx
    _         -> error "not a context"

-- | Convert a 'Context' to an integral
-- value that can be passed to the C API.
--
-- Used by the various "call_..." and "eval_..." Perl functions (which then
-- pass it on to whatever code is being called).
numContext :: (Eq p, Num p) => Context -> p
numContext VoidCtx   = G_VOID
numContext ScalarCtx = G_SCALAR
numContext ListCtx   = G_ARRAY

