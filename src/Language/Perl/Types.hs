

{- |

Types for interfacing with an embedded Perl interpreter.

-}

module Language.Perl.Types
  (
  -- * Basic types
    SV
  , Interpreter
  , Callback
  -- * Calling context
  , Context(..)
  , numContext
  )
  where


import Language.Perl.Internal.Types



