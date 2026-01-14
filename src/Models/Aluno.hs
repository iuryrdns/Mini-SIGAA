module Models.Aluno (Aluno(..), adicionarDisciplina) where

import Models.Disciplina ( Disciplina )

data Aluno = Aluno {
    nomeAluno :: String,
    matricula :: Int,
    curso :: String,
    cra :: Float,
    disciplinas :: [Disciplina]
} deriving (Show)

adicionarDisciplina :: Aluno -> Disciplina -> Aluno
adicionarDisciplina aluno novaDisciplina = aluno {disciplinas = disciplinas aluno ++ [novaDisciplina]}