module Models.Disciplina (Disciplina(..), alterarProfessor, alterarLocal) where

import Models.Professor ( Professor )

data Disciplina = Disciplina {
    nomeDisc :: String,
    local :: String,
    professor :: Professor
} deriving (Show)

alterarProfessor :: Disciplina -> Professor -> Disciplina
alterarProfessor disciplina novoProfessor = disciplina {professor = novoProfessor}

alterarLocal :: Disciplina -> String -> Disciplina
alterarLocal disciplina novoLocal = disciplina {local = novoLocal}
