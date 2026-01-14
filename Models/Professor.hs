module Models.Professor where

data Professor = Professor
  { cpf :: String, -- Arrumar um identificador melhor
    nome :: String
  }
  deriving (Show)
