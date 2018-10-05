{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Error (Error(FlagsError))
import qualified Error
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
                |> andThen 
                    (write (Flags.outputFile flags))

        Err error ->
            [ T.center 60 ' ' ""
            , T.center 60 '=' ""
            , T.center 60 ' ' "White Space C Transpilation Error"
            , T.center 60 '=' ""
            , Error.throw error
            , T.center 60 '=' ""
            ]
                |> T.unlines
                |> finish


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


finish :: Text -> IO ()
finish =
    putStrLn . T.unpack

