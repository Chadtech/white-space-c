{-# LANGUAGE OverloadedStrings #-}


module Flags.Option
    ( Option(..)
    , parse
    , get
    , Error
    , throw
    ) where


import Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Prelude.Extra (List, mapMaybe)
import Result (Result(Ok, Err))
import qualified Result


data Option
    = KeyValue Text Text
    | Key Text

    
parse :: Text -> Result Error Option
parse optionTxt =
    case  (T.splitOn "=" optionTxt) of
        k : [] ->
            Ok (Key k)

        k : v : [] ->
            Ok (KeyValue k v)

        _ ->
            Err (MalformedOption optionTxt)


get :: Text -> List Option -> Maybe Option
get queryKey options =
    case options of
        first : rest ->
            if optionKey first == queryKey then
                Just first

            else 
                get queryKey rest

        [] ->
            Nothing
    

optionKey :: Option -> Text
optionKey option =
    case option of
        KeyValue key _ ->
            key

        Key key ->
            key


-- ERROR --


data Error 
    = MalformedOption Text


throw :: Error -> Text
throw error =
    case error of
        MalformedOption optionTxt ->
            [ "You used the following option..\n\n    --"
            , optionTxt
            , "\n\nBut I need something that looks like this..\n\n    "
            , "--key=value\n\n"
            , "Or this..\n\n     "
            , "--key\n"
            ]
                |> T.concat