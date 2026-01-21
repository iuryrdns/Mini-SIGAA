{-# LANGUAGE TemplateHaskell #-}
module BrickMenu.Tipos where

import Brick
import Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import Brick.Focus (FocusRing)
import Lens.Micro.TH (makeLenses)
import qualified Data.Map as M

import Models.Aluno (Aluno)
import Sistema (Sistema)
import Models.Professor (Professor)
import Models.Matricula (Matricula)
import Brick.Widgets.List (List)

data Name = MenuPrincipal 
          | EditNomeAluno | EditMatricula | EditCurso | EditCRA 
          | EditMatriculaProfessor | EditNomeProfessor | EditDepto | EditFormacao 
          | EditCodigoDisciplina | EditNomeDisciplina 
          | EditCodTurma | EditProfTurma | EditDiscTurma | EditHorarioTurma | EditMaxAlunosTurma
          | ListaAlunos
          | ListaProfessores
          | EditMatAluno | EditMatTurma
          | EditHoraTurma | EditDiaTurma
          | ListaSolicitacoes
          deriving (Ord, Show, Eq)

data Tela = TelaMenu | TelaCadAluno | TelaCadProfessor | TelaCadDisciplina | TelaCadTurma | TelaListaAlunos | TelaListaProfessores | TelaMatriculas
          | TelaAgenda | TelaCadSolicitacao | TelaListaSolicitacoes
    deriving (Eq, Show)

data AppState = AppState
  { _sistema     :: Sistema
  , _listaMenu   :: L.List Name String
  , _listaMenuAlunos :: L.List Name Aluno
  , _listaMenuProfessores :: L.List Name Professor
  , _listaMenuSolicitacoes :: L.List Name Matricula
  , _telaAtiva   :: Tela
  , _mensagemErro :: Maybe String
  , _formularios :: M.Map Name (Editor String Name)
  , _foco        :: FocusRing Name
  }

makeLenses ''AppState