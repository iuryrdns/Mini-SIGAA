module Models.Disciplina(Disciplina, getCodigoDisciplina, getNomeDisciplina, getRequisitosDisciplina, criarDisciplina) where

data Disciplina = Disciplina{ 
  _codigo :: Int,
  _nome :: String,
  _preRequisitos :: [Disciplina]
  } deriving (Show, Read, Eq)

criarDisciplina :: Int -> String -> [Disciplina] -> Disciplina
criarDisciplina codigo nome preRequisitos = Disciplina {
  _codigo = codigo,
  _nome = nome,
  _preRequisitos = preRequisitos
}

getCodigoDisciplina :: Disciplina -> Int
getCodigoDisciplina = _codigo

getNomeDisciplina :: Disciplina -> String
getNomeDisciplina = _nome

getRequisitosDisciplina :: Disciplina -> [Disciplina]
getRequisitosDisciplina = _preRequisitos

