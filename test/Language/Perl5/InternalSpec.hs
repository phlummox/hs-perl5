
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Perl5.InternalSpec
    -- (
    --   main
    -- , spec
    -- )
  where

import Test.Hspec
import Test.QuickCheck

import            Data.Char
import            Data.Int
import qualified  Data.Text as T
import            Data.Text (Text)
import            Data.Text.Arbitrary ()
import            Data.Word

import Foreign
import Foreign.C

import            System.IO.Temp
import            System.IO

import            Text.InterpolatedString.Perl6 (qc)

import Language.Perl5 (withPerl5) -- shift withPerl5 out?
import Language.Perl5.Types
import Language.Perl5.Internal

import qualified Language.Perl5.Internal as I

import Data.Proxy
import GHC.Exts
import Debug.Trace
import Data.Monoid

import Language.Perl5.TestUtils

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

default(String)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


prop_perlInt32_roundtrips :: Int32 -> Expectation
prop_perlInt32_roundtrips n = withPerl5 $ do
  sv  <- perl5_newSViv (fromIntegral n)
  res <- perl5_SvIV sv
  fromIntegral res `shouldBe` n

prop_perlDouble_roundtrips :: CDouble -> Expectation
prop_perlDouble_roundtrips f = withPerl5 $ do
  sv  <- perl5_newSVnv f
  res <- perl5_SvNV sv
  res `shouldBe` f

prop_perlString_roundtrips :: Property
prop_perlString_roundtrips = forAll noNulsASCIIString $ \str ->
  withCStringLen str $ \(cStr, len) ->
    withPerl5 $ do
      sv  <- perl5_newSVpvn cStr (fromIntegral len)
      res <- perl5_SvPV sv >>= peekCString
      res `shouldBe` str

evalTest :: String -> Context -> IO (Ptr SV)
evalTest str ctx =
  withCStringLen str $ \(cStr, len) ->
    withPerl5 $
      perl5_eval cStr (fromIntegral len) (numContext ctx)


-- |
-- evaluating a Perl fragment like "2", "99", "-10" etc.,
-- should result in us getting an array of ptrs that looks like:
-- { NULL, ptr-to-sole-result, NULL}.
prop_eval_cint_as_scalar_gives_single_result :: CInt -> Expectation
prop_eval_cint_as_scalar_gives_single_result n = do
  res <- evalTest (show n) ScalarCtx
  let res' :: Ptr SV
      res' = castPtr res
  zerothEl  <- peek res'
  firstEl   <- peek (advancePtr res' 1)
  secondEl  <- peek (advancePtr res' 2)
  -- !_ <- traceM $ "hs. res: " <> show res <> ", zerothEl: " <> show zerothEl
  ("zerothEl",zerothEl)   `shouldBe`    ("zerothEl",nullPtr)
  ("firstEl",firstEl)     `shouldNotBe` ("firstEl",nullPtr)
  ("secondEl",secondEl)   `shouldBe`    ("secondEl",nullPtr)

prop_eval_cint_as_scalar_roundtrips :: CInt -> Expectation
prop_eval_cint_as_scalar_roundtrips n = do
  res <- evalTest (show n) ScalarCtx
  let res' :: Ptr SV
      res' = castPtr res
  firstEl  <- peek (advancePtr res' 1) >>= perl5_SvIV
  firstEl `shouldBe` n

-- evaluate a simple (ASCII, no control characters, no NUls) string --
-- equivalent of  perl -e '"mystring"'
prop_eval_simplestring_as_scalar_roundtrips :: Property
prop_eval_simplestring_as_scalar_roundtrips =
  forAll quotableASCIIString $ \str -> do
    res <- evalTest ("'" <> str <> "'") ScalarCtx
    let res' :: Ptr SV
        res' = castPtr res
    firstEl  <- peek (advancePtr res' 1) >>= perl5_SvPV >>= peekCString
    firstEl `shouldBe` str

-- | evaluate the expression:
-- @
--  split ' ' "astring1 bstring2"
-- @
prop_eval_splitstring_as_array_roundtrips :: Property
prop_eval_splitstring_as_array_roundtrips =
  forAll quotableASCIIString $ \str1 ->
    forAll quotableASCIIString $ \str2 -> do
      let str1_ = filter (/= ' ') str1
          str2_ = filter (/= ' ') str2
          fragment :: String
          fragment = [qc| my @res = split ' ', 'a{str1_} b{str2_}'; return @res;
                      |]
      -- hPutStrLn stderr $ "fragment = " <> fragment
      res <- evalTest fragment ListCtx
      let res' :: Ptr SV
          res' = castPtr res
      zerothEl   <- peek res'
      ("zerothEl",zerothEl)   `shouldBe`    ("zerothEl",nullPtr)
      firstEl  <- peek (advancePtr res' 1)
      ("firstEl",firstEl) `shouldNotBe` ("firstEl",nullPtr)
      firstElStr <- perl5_SvPV (firstEl) >>= peekCString
      ("firstElStr",firstElStr) `shouldBe` ("firstElStr", "a" <> str1_)
      secondEl   <- peek (advancePtr res' 2)
      ("secondEl",secondEl) `shouldNotBe` ("secondEl",nullPtr)
      secondElStr <- perl5_SvPV (secondEl) >>= peekCString
      ("secondElStr",secondElStr) `shouldBe` ("secondElStr", "b" <> str2_)
      thirdEl   <- peek (advancePtr res' 3)
      ("thirdEl",thirdEl)   `shouldBe`    ("thirdEl",nullPtr)

eval_die :: String -> IO (Ptr SV)
eval_die msg = do
    res <- evalTest perlString VoidCtx
    let res' :: Ptr SV
        res' = castPtr res
    return res'
  where
    perlString :: String
    perlString =
      [qc|die '{msg}';|]

-- when we die(0, the (NULL-terminated) error list is
-- non-zero-len
prop_die_returns_error :: Property
prop_die_returns_error =
    forAll quotableASCIIString $ \str -> do
      res' <- eval_die str
      zerothEl   <- peek res'
      errList    <- peekArray0 nullPtr res'
      ("zerothEl",zerothEl) `shouldNotBe` ("zerothEl",nullPtr)
      ("errListLen",length errList) `shouldNotBe` ("errListLen",1)

---- the error we get out is the error we put in
--prop_die_roundtrips_error :: Property
--prop_die_roundtrips_error =
--    forAll quotableASCIIString $ \str -> do
--      res' <- eval_die ("xx" <> str)
--      zerothEl   <- peek res'
--      errList    <- peekArray0 nullPtr res'
--      errMesg    <- fromSV (head errList)
--      let zz = errMesg :: Text
--          expectedMesgPrefix :: Text
--          expectedMesgPrefix = "xx" <> T.pack str <> " at (eval"
--      ("zerothEl",zerothEl) `shouldNotBe` ("zerothEl",nullPtr)
--      ("errMesg",errMesg)   `shouldSatisfy` (\x -> expectedMesgPrefix `T.isPrefixOf` snd x)


spec :: Spec
spec = do
  describe "perl5_newSViv (Int32)" $ do
    it "should roundtrip OK" $
        property prop_perlInt32_roundtrips
  describe "perl5_newSVnv (CDouble)" $ do
    it "should roundtrip OK" $
        property prop_perlDouble_roundtrips
  describe "perl5_newSVpvn (CStringLen)" $ do
    it "should roundtrip OK"
        prop_perlString_roundtrips
  describe "perl5_eval" $ do
    describe "when evaluating an int" $ do
      it "should give 1 result" $
        property prop_eval_cint_as_scalar_gives_single_result
      it "should roundtrip" $
        property prop_eval_cint_as_scalar_roundtrips
    describe "when evaluating simple, single-quote-quotable string" $
      it "should roundtrip"
        prop_eval_simplestring_as_scalar_roundtrips
    describe "when evaluating expressions of form: split \" \" \"str1 str2\"" $
      it "should give 2 results"
        prop_eval_splitstring_as_array_roundtrips
    describe "when we die()" $ do
      it "should result in a non-zero-len error list"
        prop_die_returns_error
      it "that contains the error we put in" $
        pendingWith "consider where ToSV and FromSV classes should go"
        --property prop_die_roundtrips_error


