{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Arrow
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import JavaParser
import System.Directory.Tree
import System.Environment
import System.FilePath
import Text.Hastache
import Text.Hastache.Context
import Data.Data
import Data.Generics
import Data.Functor.Identity

main :: IO ()
main = do
  args <- getArgs
  handleArgs args

-- | simple CL args handling before main entry point
handleArgs :: [String] -> IO ()
handleArgs [templatePath, outputPath, sourceDir] = walkFiles templatePath outputPath sourceDir
handleArgs _ = putStrLn "Usage: suppr <template-path> <output-path> <source-dir>"

-- | the real main entry point after handling CL args
walkFiles :: FilePath -> FilePath -> FilePath -> IO ()
walkFiles templatePath outputPath sourceDir = do
  tree <- readDirectoryWith TIO.readFile sourceDir
  testTemplate <- readFile templatePath
  temp <- render testTemplate . getResults $ tree
  TLIO.writeFile outputPath temp
  where
    getResults = F.foldr processFile (Tables [] []) . zipPaths .  filterAnchoredTree isJavaDirTree

-- | data structures that mainly exist to make rendering the template easy cos of deriving Data, Typeable
data Tables = Tables {unjust :: [Table], just :: [Table]} deriving (Show, Data, Typeable)
data Table = Table {path :: FilePath, suppressions :: [Line] } deriving (Show, Data, Typeable)

-- | render the Tables to a hastache template
render :: String -> Tables -> IO TL.Text
render template = hastacheStr defaultConfig (encodeStr template) . mkGenericContext

-- | Given a source file build the actual tables of unjust and just suppressions
processFile :: (FilePath, T.Text) -> Tables -> Tables
processFile (source, content) (Tables unjust just) =
  makeTables <<< addTable source unjust *** addTable source just $ getSuppressions content

-- | push out data into the record structure that hastache can understand
makeTables :: ([Table],[Table]) -> Tables
makeTables (unjust, just) = Tables unjust just

-- | Add new table to list of tables i.e. add the lines for a single file
addTable :: FilePath -> [Table] -> [Line] -> [Table]
addTable source table [] = table
addTable source table lines = Table source lines:table

-- | Predicate to filter out parts of the DirTree we dont care about i.e. non java files
isJavaDirTree :: DirTree a -> Bool
isJavaDirTree (File path _) = takeExtension path == ".java"
isJavaDirTree (Dir _ _) = True
isJavaDirTree _ = False

-- | Apply filter to dire tree preserving anchor
filterAnchoredTree :: (DirTree a -> Bool) -> AnchoredDirTree a -> AnchoredDirTree a
filterAnchoredTree f (b:/a)  = b :/  filterDir f a
