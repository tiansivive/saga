module Saga.Parser.ParsingInfo where 


data ParseError = ParseError { col:: Int, line:: Int }


class (Show a) => ParsingInfo a where
    (<->) :: a -> a -> a
    details :: a -> ParseError
