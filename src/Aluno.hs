module Aluno (Aluno(..), adicionarDisciplina) where

import Disciplina

data Aluno = Aluno {
    nomeAluno :: String,
    matricula :: Int,
    curso :: String,
    cra :: Float,
    disciplinas :: [Disciplina]
} deriving (Show)

adicionarDisciplina :: Aluno -> Disciplina -> Aluno
adicionarDisciplina aluno novaDisciplina = aluno {disciplinas = disciplinas aluno ++ [novaDisciplina]}