module Validation
    ( getValidationTests
    )
where

import System.Exit (exitFailure)
import System.IO
import System.FilePath
import System.Directory
import Control.Monad
import Data.List

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.AST ( srcFile )
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Handlers
import Graphics.Vty.Widgets.Builder.Reader
import Graphics.Vty.Widgets.Builder.Reader.XML

data ExpectedResult = Failure [String]
                    | Success
                      deriving (Eq, Show)

data ValidationTest =
    ValidationTest { inputDocument :: String
                   , description :: String
                   , expectedResult :: ExpectedResult
                   , testFilename :: FilePath
                   }
    deriving (Eq, Show)

testCaseDelim :: String
testCaseDelim = "---"

getValidationTestDir :: IO FilePath
getValidationTestDir = do
  cur <- getCurrentDirectory
  canonicalizePath $ cur </> "tests" </> "validation"

getValidationTestFilenames :: IO [FilePath]
getValidationTestFilenames = do
  files <- getDirectoryContents =<< getValidationTestDir
  return [ f | f <- files
         , ".test" `isSuffixOf` f
         , not $ "." `isPrefixOf` f
         ]

partitionBy :: (Eq a) => [a] -> a -> [[a]]
partitionBy stuff d = partitionBy' stuff d []
    where
      partitionBy' [] _ cur = if null cur
                              then []
                              else [cur]
      partitionBy' (e:es) delim cur =
          if e == delim
          then [cur] ++ partitionBy' es delim []
          else partitionBy' es delim (cur ++ [e])

cleanup :: [String] -> [String]
cleanup theLines = filter (not . ("#" `isPrefixOf`)) theLines

parseExpectedResult :: [String] -> Maybe ExpectedResult
parseExpectedResult [] = Nothing
parseExpectedResult ["success"] = Just Success
parseExpectedResult ls = Just $ Failure ls

parseValidationTest :: FilePath -> String -> Maybe ValidationTest
parseValidationTest filename input = do
  let theLines = lines input
      sections = partitionBy (cleanup theLines) testCaseDelim
  case sections of
    [desc, inputDoc, expected] ->
        do
          r <- parseExpectedResult expected
          return $ ValidationTest { description = intercalate " " desc
                                  , inputDocument = unlines inputDoc
                                  , expectedResult = r
                                  , testFilename = filename
                                  }
    _ -> Nothing

readValidationTest :: FilePath -> IO ValidationTest
readValidationTest filename = do
  contents <- readFile filename
  case parseValidationTest filename contents of
    Nothing -> error $ "Cannot parse test case " ++ filename
    Just t -> return t

runValidationTest :: ValidationTest -> IO ()
runValidationTest tc = do
  tmpdir <- getTemporaryDirectory
  (filename, handle) <- openTempFile tmpdir "testcase.tmp"
  hPutStrLn handle (inputDocument tc)
  hClose handle

  docResult <- readDocument xmlReader filename `catch`
               \e -> do
                 putStrLn $ "Error opening " ++ filename ++ ":"
                 print e
                 exitFailure

  case (docResult, expectedResult tc) of
    (Left es, Success) ->
        do
          assertFailure $ concat [ "validation failed but should have succeeded.\n"
                                 , intercalate "\n" (map fst es)
                                 ]
    (Right doc, Failure es) ->
        do
          result <- generateSourceForDocument defaultConfig doc coreSpecHandlers
          case result of
            Left generationErrors ->
                do let actualMsgs = map (\(Error loc msg) -> show (loc { srcFile = "-"}) ++ ": " ++ msg) generationErrors
                   when (not $ es == actualMsgs) $
                        assertFailure $ concat [ "Error: validation failed as expected, but with the wrong specifics.\n"
                                               , "Expected:\n"
                                               , intercalate "\n" es
                                               , "\nActual:\n"
                                               , intercalate "\n" actualMsgs
                                               ]
            Right _ -> assertFailure $ concat [ "validation succeeded but should have failed with the following errors:\n"
                                              , intercalate "\n" es
                                              ]
    (Left actualEs, Failure expectedEs) ->
        do
          let actualMsgs = map (\(msg, loc) -> show (loc { srcFile = "-"}) ++ ": " ++ msg) actualEs
          when (not $ expectedEs == actualMsgs) $
               assertFailure $ concat [ "Error: validation failed as expected, but with the wrong specifics.\n"
                                      , "Expected:\n"
                                      , intercalate "\n" expectedEs
                                      , "\nActual:\n"
                                      , intercalate "\n" actualMsgs
                                      ]
    (Right doc, Success) ->
        do
          result <- generateSourceForDocument defaultConfig doc coreSpecHandlers
          case result of
            Left generationErrors ->
                do let actualMsgs = map (\(Error loc msg) -> show (loc { srcFile = "-"}) ++ ": " ++ msg) generationErrors
                   assertFailure $ concat [ "Error: validation should have succeeded but failed.\n"
                                          , intercalate "\n" actualMsgs
                                          ]
            Right _ -> return ()

getValidationTests :: IO Test
getValidationTests = do
  base <- getValidationTestDir
  caseFiles <- getValidationTestFilenames
  tests <- forM caseFiles $
           \filename -> readValidationTest (base </> filename)

  let mkTestCase tc = testCase (takeFileName $ testFilename tc ++ " (" ++ description tc ++ ")")
                      (runValidationTest tc)

  return $ testGroup "document-validation" (map mkTestCase tests)
