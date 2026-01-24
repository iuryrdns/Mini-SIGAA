
module Models.PerfilAcademico
  ( PerfilAcademico(..)
  ) where

data PerfilAcademico
  = Blocado
  | ProvavelConcluinte
  | Desblocado
  | Adiantado
  | Regular
  deriving (Eq, Ord, Show)

{-

import Models.Aluno (Aluno, periodoAtual, disciplinasConcluidas)
import Models.Disciplina (Disciplina, periodoDisciplina, creditos)

import qualified Data.Map as M



calcularPerfil :: Aluno -> Disciplina -> PerfilAcademico
calcularPerfil aluno disc
    | estaNoBloco aluno disc     = Blocado
    | ehProvavelConcluinte aluno = ProvavelConcluinte
    | estaDesblocado aluno disc  = Desblocado
    | estaAdiantado aluno disc   = Adiantado
    | otherwise                  = Regular

periodoAluno = periodoAtual

estaNoBloco :: Aluno -> Disciplina -> Bool
estaNoBloco aluno disc = periodoAluno aluno == periodoDisciplina disc

estaDesblocado :: Aluno -> Disciplina -> Bool
estaDesblocado aluno disc = periodoAluno aluno > periodoDisciplina disc

estaAdiantado :: Aluno -> Disciplina -> Bool
estaAdiantado aluno disc = periodoAluno aluno < periodoDisciplina disc
-}