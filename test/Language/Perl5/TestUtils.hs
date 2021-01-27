
{-

Helper funcs and instances for testing.

-}

module Language.Perl5.TestUtils
  where

import Data.Char
import Test.QuickCheck


-- | generate abitrary ASCII string w/ no NUL characters.
noNulsASCIIString :: Gen String
noNulsASCIIString = filter (/= '\x00') . getASCIIString <$> (arbitrary :: Gen ASCIIString)

-- | an ASCII string that can be reproduced in Perl just by putting single quotes around it.
-- No NUL chars and no control characters, no backslashes, no single qotes.
-- Just letters, safe punctuation and digits.
quotableASCIIString :: Gen String
quotableASCIIString = filter isOK . getASCIIString <$> (arbitrary :: Gen ASCIIString)
  where
    isOK :: Char -> Bool
    isOK c = c `notElem` "'\\" && (isAlphaNum c || isPunctuation c)

