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
    case Result.map readWscFile result of
        Ok reader ->
            reader
                |> fmap T.unpack
                |> andThen putStrLn

        Err error ->
            finish (Error.throw error)


readWscFile :: Flags -> IO Text
readWscFile flags =
    flags
        |> Flags.targetFile
        |> T.unpack
        |> Prelude.readFile
        |> fmap T.pack


finish :: Text -> IO ()
finish =
    putStrLn . T.unpack