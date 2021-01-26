

{- |

Types for interfacing with an embedded Perl interpreter.

-}

module Language.Perl5.Internal.Types
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Language.Perl5.Internal.Constants


{-# ANN module ("HLint: ignore Missing NOINLINE pragma" :: String) #-}

type Interpreter = Ptr ()
type SV = Ptr ()
type Callback = Ptr SV -> CInt -> IO (Ptr SV)


-- | Perl's calling context.
data Context = VoidCtx | ScalarCtx | ListCtx
  deriving (Eq, Show)

instance Enum Context where
  fromEnum = numContext
  toEnum n = case n of
    G_VOID    -> VoidCtx
    G_SCALAR  -> ScalarCtx
    G_ARRAY   -> ListCtx

-- | Convert a 'Context' to an integral
-- value that can be passed to the C API.
--
-- Used by the various "call_..." and "eval_..." Perl functions (which then
-- pass it on to whatever code is being called).
numContext :: (Eq p, Num p) => Context -> p
numContext VoidCtx   = G_VOID
numContext ScalarCtx = G_SCALAR
numContext ListCtx   = G_ARRAY

