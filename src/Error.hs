module Error
    ( Error(..)
    , throw
    ) where


import Data.Text (Text)
import qualified Data.Text as T
import qualified Flags
import qualified File


data Error
    = FlagsError Flags.Error
    | FileError File.Error


throw :: Error -> Text
throw error =
    case error of
        FlagsError flagsError ->
            Flags.throw flagsError

        FileError fileError ->
            File.throw fileError