module Error
    ( Error(..)
    , throw
    ) where


import Data.Text (Text)
import qualified Data.Text as T
import qualified Flags


data Error
    = FlagsError Flags.Error


throw :: Error -> Text
throw error =
    case error of
        FlagsError flagsError ->
            Flags.throw flagsError