{-# LANGUAGE OverloadedStrings #-}


module Flags
    ( Flags
    , Error
    , Flags.fromList
    , throw
    , srcFile
    , outputFile
    ) where


import Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Flags.Option (Option(Key, KeyValue))
import qualified Flags.Option as Option
import Flags.Args (Args)
import qualified Flags.Args as Args
import Flow
import qualified Parse
import Parse (Parser)
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result


data Flags
    = Flags 
        { srcFile :: Text 
        , deliberateOutputFile :: Maybe Text
        }

            
fromList :: List Text -> Result Error Flags
fromList argTxts =
    case Args.fromList argTxts of
        Ok args ->
            Ok Flags
                |> applySrcFile (Args.files args)
                |> applyOutputFile args
        
        Err err ->
            Err (ArgsError err)


applySrcFile :: List Text -> Parser Error Text b
applySrcFile files ctorResult =
    case files of
        first : _ ->
            Parse.construct first ctorResult

        [] ->
            Err NoSrcFile


applyOutputFile :: Args -> Parser Error (Maybe Text) b
applyOutputFile args ctorResult =
    case Option.get "output" (Args.options args) of
        Just (KeyValue _ value) ->
            Parse.construct (Just value) ctorResult

        Just (Key k) ->
            Err (MalformedOutputOption k)

        Nothing ->
            Parse.construct Nothing ctorResult


outputFile :: Flags -> Text
outputFile flags =
    case deliberateOutputFile flags of
        Just d ->
            d

        Nothing ->
            flags
                |> srcFile
                |> removeFileExtension
                |> addCExtension


removeFileExtension :: Text -> Text
removeFileExtension text =
    case List.reverse (T.splitOn ".wsc" text) of
        "" : rest ->
            rest
                |> List.reverse
                |> T.concat

        _ ->
            text


addCExtension :: Text -> Text
addCExtension text =
    T.append text ".c"


-- ERROR --


data Error
    = NoSrcFile
    | ArgsError Args.Error
    | MalformedOutputOption Text 


throw :: Error -> Text
throw error =
    case error of
        NoSrcFile ->
            "You didnt enter a file to compile, write something like \n\n\
            \    wscc main.wsc\n"

        ArgsError argsError ->
            Args.throw argsError

        MalformedOutputOption optionTxt ->
            [ "For the file output, you entered..\n\n   --"
            , optionTxt 
            , "\n\nBut the option needs a value like..\n\n    \
              \--output=src/main.c\n"
            ]
                |> T.concat


