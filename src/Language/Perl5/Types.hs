

{- |

Types for interfacing with an embedded Perl interpreter.

-}

module Language.Perl5.Types
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe

{-# ANN module ("HLint: ignore Missing NOINLINE pragma" :: String) #-}

type Interpreter = Ptr ()
type SV = Ptr ()
type Callback = Ptr SV -> CInt -> IO (Ptr SV)


-- | Perl 5's calling context.
data Context = Void | Item | List

-- | Convert a 'Context' to an integral
-- value that can be passed to the C API.
--
-- Used by the various "call_..." and "eval_..." Perl functions to (which then
-- pass it on to whatever code is being called).
enumContext :: (Num a) => Context -> a
enumContext Void = fromIntegral perl5_G_VOID
enumContext Item = fromIntegral perl5_G_SCALAR
enumContext List = fromIntegral perl5_G_ARRAY

perl5_G_VOID :: CInt
perl5_G_VOID = unsafePerformIO $ peek perl5_G_VOID_

perl5_G_SCALAR :: CInt
perl5_G_SCALAR = unsafePerformIO $ peek perl5_G_SCALAR_

perl5_G_ARRAY :: CInt
perl5_G_ARRAY = unsafePerformIO $ peek perl5_G_ARRAY_



-- constants to be bitwise OR'd with other call-context flags,
-- to give something we can pass to eval_... and call_... Perl API functions.
-- TODO: 'import' these using hsc2hs, rather than 'import a ptr and unsafely deref it'.

foreign import ccall unsafe "& perl5_G_VOID"
    perl5_G_VOID_ :: Ptr CInt

foreign import ccall unsafe "& perl5_G_SCALAR"
    perl5_G_SCALAR_ :: Ptr CInt

foreign import ccall unsafe "& perl5_G_ARRAY"
    perl5_G_ARRAY_ :: Ptr CInt
