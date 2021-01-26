

{- |

Types for interfacing with an embedded Perl interpreter.

-}

module Language.Perl5.Types
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String


-- | Perl 5's calling context.
data Context = Void | Item | List

enumContext :: (Num a) => Context -> a
enumContext Void = 128
enumContext Item = 0
enumContext List = 1

type Interpreter = Ptr ()
type SV = Ptr ()

type Callback = Ptr SV -> CInt -> IO (Ptr SV)
