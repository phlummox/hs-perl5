

{- |

Types for interfacing with an embedded Perl interpreter.

-}

module Language.Perl.Types
  (
  -- * Basic types
    SV
  , AV
  , CV
  , Interpreter
  , Callback
  -- * Calling context
  , Context(..)
  , numContext
  -- * Underlying C types
  , IV
  , NV
  )
  where

import Language.Perl.Internal.Types



