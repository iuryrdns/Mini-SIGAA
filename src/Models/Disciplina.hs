module Models.Disciplina(Disciplina, getCodigoDisciplina, getNomeDisciplina, getRequisitosDisciplina, criarDisciplina) where

data Disciplina = Disciplina{ 
  _codigo :: String,
  _nome :: String,
  _preRequisitos :: [String]
  } deriving (Show, Read, Eq)

criarDisciplina :: String -> String -> [String] -> Disciplina
criarDisciplina codigo nome preRequisitos = Disciplina {
  _codigo = codigo,
  _nome = nome,
  _preRequisitos = preRequisitos
}

getCodigoDisciplina :: Disciplina -> String
getCodigoDisciplina = _codigo

getNomeDisciplina :: Disciplina -> String
getNomeDisciplina = _nome

getRequisitosDisciplina :: Disciplina -> [String]
getRequisitosDisciplina = _preRequisitos

