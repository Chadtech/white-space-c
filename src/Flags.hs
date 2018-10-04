{-# LANGUAGE OverloadedStrings #-}


module Flags
    ( Flags
    , Error
    , fromList
    , throw
    , targetFile
    ) where


import Data.Text (Text)
import qualified Data.Text as T
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result


data Flags
    = Flags { targetFile :: Text }    


fromList :: List Text -> Result Error Flags
fromList options =
    case options of
        first : rest ->
            Ok (Flags { targetFile = first })

        [] ->
            Err NoTargetFile


-- ERROR --


data Error
    = NoTargetFile


throw :: Error -> Text
throw error =
    case error of
        NoTargetFile ->
            "You didnt enter a file to compile, write something like \n\n\
            \    wscc main.wsc\n"