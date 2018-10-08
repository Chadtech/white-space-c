{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Error (Error(FlagsError, FileError))
import qualified Error
import qualified File
import Flags (Flags)
import qualified Flags
import Flow
import Prelude.Extra (List, andThen)
import qualified System.Environment as Sys
import Result (Result(Ok, Err))
import qualified Result


main :: IO ()
main = 
    Sys.getArgs
        |> fmap toFlags
        |> andThen runFromFlags


toFlags :: List String -> Result Error Flags
toFlags args =
    args
        |> List.map T.pack
        |> Flags.fromList
        |> Result.mapError FlagsError 


runFromFlags :: Result Error Flags -> IO ()
runFromFlags result =
    case result of
        Ok flags ->
            flags
                |> readWscFile 
                |> andThen (fromFile flags)

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
transpile fileText =
    fileText
        |> File.parse
        |> Result.map File.write
        |> Result.mapError FileError
            

printError :: Error -> IO ()
printError error =
    [ T.center 60 ' ' ""
    , T.center 60 '=' ""
    , T.center 60 ' ' "White Space C Transpilation Error"
    , T.center 60 '=' ""
    , Error.throw error
    , T.center 60 '=' ""
    ]
        |> T.unlines
        |> T.unpack
        |> putStrLn


readWscFile :: Flags -> IO Text
readWscFile flags =
    flags
        |> Flags.srcFile
        |> T.unpack
        |> Prelude.readFile
        |> fmap T.pack


write :: Text -> Text -> IO ()
write fn file =
    writeFile (T.unpack fn) (T.unpack file)
        >> putStrLn "Success!"

