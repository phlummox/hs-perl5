
{-# LANGUAGE PatternSynonyms #-}

{- |

Constants and stuff extracted from header files

-}

module Language.Perl.Internal.Constants
  (
  -- * Flag values for 'call' functions
  -- See <https://perldoc.perl.org/perlcall#FLAG-VALUES perlcall> for more
  -- detail.
    pattern G_VOID
  , pattern G_SCALAR
  , pattern G_ARRAY
  , pattern G_DISCARD
  , pattern G_NOARGS
  , pattern G_EVAL
  , pattern G_KEEPERR
  -- * Underlying C types
  , IV
  , NV
  )
  where

#include "EXTERN.h"
#include "perl.h"
#include "embed.h"

import Data.Int

-- | void context
pattern G_VOID :: (Num a, Eq a) => a
pattern G_VOID   = #const G_VOID

-- | scalar context
pattern G_SCALAR :: (Num a, Eq a) => a
pattern G_SCALAR   = #const G_SCALAR

-- | list context (yes - it IS called list context
-- in the Perl docco, despite the name)
pattern G_ARRAY :: (Num a, Eq a) => a
pattern G_ARRAY   = #const G_ARRAY

-- | discard results
pattern G_DISCARD :: (Num a, Eq a) => a
pattern G_DISCARD   = #const G_DISCARD

-- | no args -- use with caution
pattern G_NOARGS :: (Num a, Eq a) => a
pattern G_NOARGS   = #const G_NOARGS

-- | catch exceptions
pattern G_EVAL :: (Num a, Eq a) => a
pattern G_EVAL   = #const G_EVAL

-- | do ... something with errors.
-- see <https://perldoc.perl.org/perlcall#FLAG-VALUES perlcall>
pattern G_KEEPERR :: (Num a, Eq a) => a
pattern G_KEEPERR   = #const G_KEEPERR



-- | Underlying C integer type used by Perl. (\"IV" = "Integer value".)
type IV = #type IV

-- | Underlying C floating-point type used by Perl. (\"NV" = "Numeric value".)
type NV = #type NV

