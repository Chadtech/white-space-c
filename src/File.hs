{-# LANGUAGE OverloadedStrings #-}


module File
    ( parse
    , write
    , Error
    , throw
    ) where


import Data.Function
import Data.List as List
import Data.List.Index as LI
import Data.Text (Text)
import qualified Data.Text as T
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result


data Canonical
    = Include Text


parse :: Text -> Result Error (List Canonical)
parse 
    = toCanonicals
    . LI.indexed
    . List.map T.stripEnd
    . T.lines


toCanonicals :: List (Int, Text) -> Result Error (List Canonical)
toCanonicals lines =
    toCanonicalsAccum lines []


toCanonicalsAccum :: List (Int, Text) -> List Canonical -> Result Error (List Canonical)
toCanonicalsAccum lines canonicals =
    case lines of
        first : rest ->
            case lineToCanonical lines first rest of
                Ok (Just newCanonical, remainingLines) ->
                    toCanonicalsAccum 
                        remainingLines 
                        (newCanonical : canonicals)

                Ok (Nothing, remainingLines) ->
                    toCanonicalsAccum
                        remainingLines
                        canonicals

                Err err ->
                    Err err


        [] ->
            Ok (List.reverse canonicals)


data Context
    = Context 
        { allLines :: List (Int, Text)
        , thisIndex :: Int
        , thisLine :: Text
        , linesAfterThisLine :: List (Int, Text)
        }


lineToCanonical :: 
    List (Int, Text) 
    -> (Int, Text) 
    -> List (Int, Text) 
    -> Result Error (Maybe Canonical, List (Int, Text))
lineToCanonical allLines (index, line) rest =
    let
        context :: Context
        context =
            Context
                { allLines = allLines
                , thisIndex = index
                , thisLine = line
                , linesAfterThisLine = rest
                }
    in
    case T.words line of
        "include" : remainingWords ->
            parseInclude context remainingWords

        -- "int" : name : "=" : 

        "" : [] ->
            Ok (Nothing, rest)

        _ ->
            -- commented out for development, should error
            Ok (Nothing, rest)


parseInclude :: Context ->  List Text -> Result Error (Maybe Canonical, List (Int, Text))
parseInclude context wordsAfterInclude =
    case wordsAfterInclude of
        includeFile : [] ->
            ( Just $ Include includeFile
            , linesAfterThisLine context
            )
                & Ok

        _ ->
            context
                & IncludeHasTooManyWords
                & Err


write :: List Canonical -> Text
write canonicals =
    canonicals
        & List.map writeCanonical 
        & T.concat


writeCanonical :: Canonical -> Text
writeCanonical canonical =
    case canonical of
        Include importFile ->
            [ "#include <" 
            , importFile
            , ">\n"
            ]
                & T.concat


-- ERROR --


data Error 
    = IncludeHasTooManyWords Context


throw :: Error -> Text
throw error =
    case error of
        IncludeHasTooManyWords context ->
            [ context 
                & thisIndex
                & show
                & T.pack
            , "|"
            , thisLine context
            , "\n          "
            , T.replicate 
                (T.length (thisLine context) - 8)
                "^"
            ]
                & T.concat