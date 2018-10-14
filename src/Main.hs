{-# LANGUAGE OverloadedStrings #-}


module Main where


import Data.Function
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Error (Error(FlagsError, FileError))
import qualified Error
import qualified File
import Flags (Flags)
import qualified Flags
import Prelude.Extra (List, andThen)
import qualified System.Environment as Sys
import Result (Result(Ok, Err))
import qualified Result


main :: IO ()
main = 
    (fmap toFlags Sys.getArgs)
        >>= runFromFlags


toFlags :: List String -> Result Error Flags
toFlags 
    = Result.mapError FlagsError
    . Flags.fromList
    . List.map T.pack


runFromFlags :: Result Error Flags -> IO ()
runFromFlags result =
    case result of
        Ok flags ->
            flags
                & readWscFile 
                & andThen (fromFile flags)

        Err error ->
            printError error


fromFile :: Flags -> Text -> IO ()
fromFile flags fileText =
    case transpile fileText of
        Ok file ->
            write (Flags.outputFile flags) file

        Err error ->
            printError error


transpile :: Text -> Result Error Text
transpile 
    = Result.mapError FileError
    . Result.map File.write
    . File.parse
            

printError :: Error -> IO ()
printError error =
    [ T.center 60 ' ' ""
    , T.center 60 '=' ""
    , T.center 60 ' ' "White Space C Transpilation Error"
    , T.center 60 '=' ""
    , Error.throw error
    , T.center 60 '=' ""
    ]
        & T.unlines
        & T.unpack
        & putStrLn


readWscFile :: Flags -> IO Text
readWscFile 
    = fmap T.pack
    . Prelude.readFile
    . T.unpack
    . Flags.srcFile


write :: Text -> Text -> IO ()
write fn file =
    writeFile (T.unpack fn) (T.unpack file)
        >> putStrLn "Success!"

