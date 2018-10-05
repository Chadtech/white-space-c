{-# LANGUAGE OverloadedStrings #-}


module Flags.Args
    ( Args
    , options
    , files
    , fromList
    , Error
    , throw
    ) where


import Data.Text (Text)
import qualified Data.Text as T
import Flags.Option (Option)
import qualified Flags.Option as Option
import Flow
import Prelude.Extra (List, mapSecond)
import Result (Result(Ok, Err))
import qualified Result

import qualified Data.List as List


data Args
    = Args
        { files :: List Text
        , options :: List Option
        }

        
addFile :: Text -> Args -> Args
addFile file args = 
    args { files = file : (files args) }


addOption :: Option -> Args -> Args
addOption option args =
    args { options = option : (options args) }


fromList :: List Text -> Result Error Args
fromList argTxts =
    []
        |> gatherFiles argTxts
        |> mapSecond (gatherOptions [])
        |> fromTuple


fromTuple :: (List Text, Result Error (List Option)) -> Result Error Args
fromTuple (files, optionsResult) =
    case optionsResult of
        Ok options ->
            Args 
                { files = files
                , options = options
                }
                |> Ok

        Err err ->
            Err err


gatherFiles :: List Text -> List Text -> (List Text, List Text)
gatherFiles argTxts gathered =
    case argTxts of
        first : rest ->
            if T.take 2 first == "--" then
                (gathered, argTxts)

            else
                gatherFiles rest (first : gathered)

        [] ->
            (gathered, [])


gatherOptions :: List Option -> List Text -> Result Error (List Option)
gatherOptions gathered argTxts  =
    case argTxts of
        first : rest ->
            if T.take 2 first == "--" then
                case Option.parse (T.drop 2 first) of
                    Ok option ->
                        gatherOptions (option : gathered) rest

                    Err err ->
                        Err (OptionError err)
                        
            else
                Err FilesInterMixedWithOptions

        [] ->
            Ok gathered


-- ERROR --


data Error
    = FilesInterMixedWithOptions
    | OptionError Option.Error


throw :: Error -> Text
throw error =
    case error of
        FilesInterMixedWithOptions ->
            "There was something wrong with the arguments you \
            \gave wscc. It should look something like this..\n\n\
            \    wscc <..files> <..options>\n\n\
            \where options are either..\n\n\
            \    --key\n\n\
            \or..\n\n\
            \    --key=value\n\n"

        OptionError optionError ->
            Option.throw optionError
