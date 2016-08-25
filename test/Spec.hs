{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck
import Control.Applicative
import Control.Exception (evaluate)
import Data.Attoparsec.Text
import JavaParser
import qualified Data.Text as T

instance Arbitrary T.Text where
    arbitrary = T.pack <$> (arbitrary :: Gen String)

main :: IO ()
main = hspec $ do
  describe "Lib.parseSuppression:" $ do
    it "should not parse the empty string" $ do
         suppressionParser `shouldFailOn` T.empty
    it "should parse a @SuppressWarnings annotation" $ do
       ("@SuppressWarnings(\"foo\") //bar baz\n" :: T.Text) ~>
         suppressionParser `shouldParse` Justified "foo" "bar baz"
    it "should parse a @SuppressFBWarnings annotation" $ do
       ("@SuppressFBWarnings(\"foo\") //bar baz\n" :: T.Text) ~>
         suppressionParser `shouldParse` Justified "foo" "bar baz"
    it "should ignore spaces while parsing @SuppressWarnings annotation" $ do
       (" @SuppressWarnings  ( \"foo\" )  //  bar baz\n" :: T.Text) ~>
         suppressionParser `shouldParse` Justified "foo" "bar baz"
    it "should parse @SupressWarnings with no justification" $ do
      (" @SuppressWarnings ( \"foo\" )\n" :: T.Text) ~>
        suppressionParser `shouldParse` Unjustified "foo"
    it "should parse @SupressWarnings with no justification" $ do
      (" @SuppressWarnings ( \"foo\" ) \n" :: T.Text) ~>
        suppressionParser `shouldParse` Unjustified "foo"
  describe "Lib.javaLineParser:" $ do
    it "should parse code fragments as code" $ do
      ("    class Foo\n" :: T.Text) ~>
        javaLineParser `shouldParse` Code
    it "should parse code fragments as code" $ do
      ("    int foo(int baz, double bar)\n" :: T.Text) ~>
        javaLineParser `shouldParse` Code
    it "should parse code fragments as code" $ do
      ("    @Test\n" :: T.Text) ~>
        javaLineParser `shouldParse` Code
    it "should parse justifed as justified" $ do
      (" @SuppressWarnings  ( \"foo\" )  //  bar baz\n" :: T.Text) ~>
        javaLineParser `shouldParse` Justified "foo" "bar baz"
    it "should parse unjustifed as unjustified" $ do
      (" @SuppressWarnings  ( \"foo\" )\n" :: T.Text) ~>
        javaLineParser `shouldParse` Unjustified "foo"
  
