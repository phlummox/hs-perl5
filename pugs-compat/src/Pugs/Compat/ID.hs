{-# OPTIONS_GHC -fno-full-laziness -fno-cse -cpp #-}

module Pugs.Compat.ID (
    ID, bufToID, hashNew,
    __, (+++), nullID, idKey, idBuf, AtomMap, AtomSet
) where

import StringTable.Atom
import StringTable.AtomMap (AtomMap)
import StringTable.AtomSet (AtomSet)
import Pugs.Compat.Cast
import Data.Int
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashTable.IO as H

type ID = Atom

bufToID :: ByteString -> ID
bufToID = toAtom

idBuf :: ID -> ByteString
idBuf = fromAtom

idKey :: ID -> Int
idKey = fromAtom

{-# INLINE hashNew #-}
hashNew :: IO (H.BasicHashTable ByteString a)
hashNew = H.new

{-
-- XXX - Under GHCI, our global _BufToID table could be refreshed into
--       nonexistence, so we need to compare IDs based on the actual buffer,
--       not its unique key.
data ID = MkID
#ifdef PUGS_UNDER_GHCI
    { idBuf :: !ByteString, idKey :: !Int }
#else
    { idKey :: !Int, idBuf :: !ByteString }
#endif
    deriving (Typeable, Data)

instance Eq ID where
    MkID x _ == MkID y _ = x == y
    MkID x _ /= MkID y _ = x /= y

instance Ord ID where
    compare (MkID x _) (MkID y _) = compare x y
    MkID x _ <= MkID y _ = x <= y
    MkID x _ >= MkID y _ = x >= y
    MkID x _ < MkID y _ = x < y
    MkID x _ > MkID y _ = x > y

instance Show ID where
    showsPrec x MkID{ idBuf = buf } = showsPrec x buf

instance Read ID where
    readsPrec p s = [ (unsafePerformIO (bufToID (UTF8.pack x)), y) | (x, y) <- readsPrec p s]
-}

{-# NOINLINE nullID #-}
nullID :: ID
nullID = _cast ""

{-# INLINE __ #-}
__ :: String -> ByteString
__ = UTF8.fromString

{-# INLINE (+++) #-}
(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append

instance ((:>:) ID) String where
    cast = toAtom

instance ((:>:) String) ID where
    cast = fromAtom

instance ((:<:) String) ID where
    castBack = toAtom

instance ((:<:) ID) ByteString where
    castBack = fromAtom

instance ((:<:) ByteString) ID where
    castBack = toAtom
