{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T

parseChar = skipSpace >> char '@' >> string "SuppressWarnings"

parseStr = skipSpace >> string "@SuppressWarnings"

main = defaultMain [
  bgroup "parse char first" [
     bench "sw"  $ nf (parseOnly parseChar) "   @SuppressWarnings(\"foo\")",
     bench "soa"  $ nf (parseOnly parseChar) "   @SomeOtherAnnotation()",
     bench "if"  $ nf  (parseOnly parseChar) "   if (this.isSomeCode())"
     ],
  bgroup "parse string" [
     bench "sw"  $ nf (parseOnly parseStr) "   @SuppressWarnings(\"foo\")",
     bench "soa"  $ nf (parseOnly parseStr) "   @SomeOtherAnnotation()",
     bench "if"  $ nf  (parseOnly parseStr) "   if (this.isSomeCode())"
     ]
  ]
  
