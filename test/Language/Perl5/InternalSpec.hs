
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
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import            Control.Monad
import            Data.Either.Compat
import qualified  Data.List as L
import            Data.Monoid ( (<>) )
import            Data.Text.Arbitrary ()
import            Foreign
import            Foreign.C
import            Text.InterpolatedString.Perl6 (qc)

import Language.Perl5 (withPerl5) -- shift withPerl5 out?
import Language.Perl5.Internal.Types
import Language.Perl5.Internal

import Language.Perl5.TestUtils

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Redundant return" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

default(String)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

forceEither :: Either a b -> b
forceEither Left{}    = error "bad forceEither"
forceEither (Right r) = r

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

evalTest :: String -> Context -> IO (Either [SV] [SV])
evalTest str ctx =
  withCStringLen str $ \(cStr, len) ->
      svEither =<< perl5_eval cStr (fromIntegral len) (numContext ctx)

-- |
-- evaluating a Perl fragment like "2", "99", "-10" etc.,
-- should result in us getting an array of ptrs that looks like:
-- { NULL, ptr-to-sole-result, NULL}.
prop_eval_cint_as_scalar_gives_single_result :: CInt -> Expectation
prop_eval_cint_as_scalar_gives_single_result n = do
  res   <- withPerl5 $ evalTest (show n) ScalarCtx
  res   `shouldSatisfy` (\x -> isRight x && length (forceEither x) == 1)

prop_eval_cint_as_scalar_roundtrips :: CInt -> Expectation
prop_eval_cint_as_scalar_roundtrips n = do
  withPerl5 $ do
    retVal  <- evalTest (show n) ScalarCtx
    retVal `shouldSatisfy` isRight
    res <- mapM perl5_SvIV (forceEither retVal)
    ("res",res) `shouldBe` ("res",[n])

-- evaluate a simple (ASCII, no control characters, no NUls) string --
-- equivalent of  perl -e '"mystring"'
prop_eval_simplestring_as_scalar_roundtrips :: Property
prop_eval_simplestring_as_scalar_roundtrips =
  forAll quotableASCIIString $ \str -> withPerl5 $ do
    retVal  <- evalTest ("'" <> str <> "'") ScalarCtx
    retVal `shouldSatisfy` isRight
    res <- mapM (peekCString <=< perl5_SvPV) (forceEither retVal)
    ("res",res) `shouldBe` ("res",[str])

-- | evaluate the expression:
-- @
--  split ' ' "astring1 bstring2"
-- @
prop_eval_splitstring_as_array_roundtrips :: Property
prop_eval_splitstring_as_array_roundtrips =
  forAll quotableASCIIString $ \str1 ->
    forAll quotableASCIIString $ \str2 -> withPerl5 $ do
      let str1_ = filter (/= ' ') str1
          str2_ = filter (/= ' ') str2
          fragment :: String
          fragment = [qc| my @res = split ' ', 'a{str1_} b{str2_}'; return @res;
                      |]
      -- hPutStrLn stderr $ "fragment = " <> fragment
      retVal <- evalTest fragment ListCtx
      retVal `shouldSatisfy` isRight
      res    <- mapM (peekCString <=< perl5_SvPV) (forceEither retVal)
      ("res",res) `shouldBe` ("res",["a" <> str1_, "b" <> str2_])


eval_die :: String -> IO (Either [SV] [SV])
eval_die msg = do
    evalTest perlString VoidCtx
  where
    perlString :: String
    perlString =
      [qc|die '{msg}';|]

-- when we die(0, the (NULL-terminated) error list is
-- non-zero-len and contains the expected error
prop_die_returns_error :: Property
prop_die_returns_error =
    forAll quotableASCIIString $ \str -> withPerl5 $ do
      retVal <- eval_die ("xx" <> str)
      retVal `shouldSatisfy` isLeft
      errList <- mapM (peekCString <=< perl5_SvPV) $ fromLeft (error "ack!") retVal
      errList `shouldSatisfy` (not . null)
      let errMesg  = head errList
          expectedMesgPrefix = "xx" <> str <> " at (eval"
      ("errMesg",errMesg)  `shouldSatisfy` (\x -> expectedMesgPrefix `L.isPrefixOf` snd x)



spec :: Spec
spec = do
  describe "perl5_newSViv (Int32)" $ do
    modifyMaxSuccess (const 500) $
      it "should roundtrip OK" $
          property prop_perlInt32_roundtrips
  describe "perl5_newSVnv (CDouble)" $ do
    modifyMaxSuccess (const 500) $
      it "should roundtrip OK" $
          property prop_perlDouble_roundtrips
  describe "perl5_newSVpvn (CStringLen)" $ do
    modifyMaxSuccess (const 500) $
      it "should roundtrip OK"
          prop_perlString_roundtrips
  describe "perl5_eval" $ do
    describe "when evaluating an int" $ do
      modifyMaxSuccess (const 500) $ do
        it "should give 1 result" $
          property prop_eval_cint_as_scalar_gives_single_result
        it "should roundtrip" $
          property prop_eval_cint_as_scalar_roundtrips
    describe "when evaluating simple, single-quote-quotable string" $
      modifyMaxSuccess (const 500) $
        it "should roundtrip"
          prop_eval_simplestring_as_scalar_roundtrips
    describe "when evaluating expressions of form: split \" \" \"str1 str2\"" $
      modifyMaxSuccess (const 500) $
        it "should give 2 results"
          prop_eval_splitstring_as_array_roundtrips
    describe "when we die()" $
      modifyMaxSuccess (const 500) $
        it "the error list contains the error message we put in"
          prop_die_returns_error


