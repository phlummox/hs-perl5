{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE KindSignatures, RoleAnnotations #-}
module MO.Run where
import Data.Typeable
type role AnyResponder nominal
data AnyResponder (m :: * -> *)
emptyResponder :: (Typeable1 m, Monad m) => AnyResponder m

