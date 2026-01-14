module Professor (Professor(..)) where

data Professor = Professor {
    nomeProf :: String,
    departamento :: String,
    formacaoProf :: String
} deriving (Show)