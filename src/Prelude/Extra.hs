{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( head
    , listMap2
    , List
    , (<<)
    , debugLog
    , mapFirst
    , mapSecond
    , mapMaybe
    , andThen
    ) where


import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (head)
import qualified Debug.Trace as Debug


andThen :: Monad m => (a -> m b) -> m a -> m b
andThen f m =
    m >>= f


head :: List a -> Maybe a
head list =
    case list of
        [] ->
            Nothing

        first : rest ->
            Just first


mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f maybe =
    case maybe of
        Just v ->
            Just (f v)

        Nothing ->
            Nothing


listMap2 :: List a -> List b -> (a -> b -> c) -> List c
listMap2 xs ys f =
    listMap2Accumulate xs ys f []


listMap2Accumulate :: List a -> List b -> (a -> b -> c) -> List c -> List c
listMap2Accumulate xs ys f output =
    case (xs, ys) of
        (x : restXs, y : restYs) ->
            listMap2Accumulate 
                restXs 
                restYs 
                f 
                (f x y : output)

        _ ->
            List.reverse output


type List a = [ a ]


infixl 0 <<
(<<) = leftCompose


leftCompose :: (b -> c) -> (a -> b) -> a -> c
leftCompose f g v =
    f (g v)


debugLog :: String -> (a -> String) -> a -> a
debugLog msg toString x =
    Debug.trace (msg ++ " : " ++ (toString x)) x


mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) =
    (f a, b)


mapSecond :: (a -> b) -> (c, a) -> (c, b)
mapSecond f (a, b) =
    (a, f b)