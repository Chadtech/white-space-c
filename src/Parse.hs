module Parse
    ( Parser
    , construct
    ) where


import Flow
import Result (Result)
import qualified Result


type Parser error a b =
    Result error (a -> b) -> Result error b


construct :: a -> Result error (a -> b) -> Result error b
construct x =
    Result.map ((|>) x)