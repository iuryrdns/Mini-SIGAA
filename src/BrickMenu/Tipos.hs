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

-- Adicione aqui todos os identificadores de campos que existirem no sistema
data Name = MenuPrincipal 
          | EditNomeAluno | EditMatricula | EditCurso | EditCRA 
          | EditMatriculaProfessor | EditNomeProfessor | EditDepto | EditFormacao 
          | EditCodigoDisciplina | EditNomeDisciplina 
          | EditCodTurma | EditProfTurma | EditDiscTurma | EditHorarioTurma | EditMaxAlunosTurma
          | ListaAlunos
          deriving (Ord, Show, Eq)

data Tela = TelaMenu | TelaCadAluno | TelaCadProfessor | TelaCadDisciplina | TelaCadTurma | TelaListaAlunos

data AppState = AppState
  { _sistema     :: Sistema
  , _listaMenu   :: L.List Name String
  , _listaMenuAlunos :: L.List Name Aluno
  , _telaAtiva   :: Tela
  , _formularios :: M.Map Name (Editor String Name)
  , _foco        :: FocusRing Name
  }

makeLenses ''AppState