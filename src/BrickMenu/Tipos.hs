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
import Models.Types (Matricula, Nome, Codigo, Curso)
import Brick.Widgets.List (List)
import Models.Turma (Turma)
import Models.Disciplina (Disciplina)

data Name = MenuPrincipal 
          | EditNomeAluno | EditMatricula | EditCurso | EditCRA 
          | EditMatriculaProfessor | EditNomeProfessor | EditDepto | EditFormacao 
          | EditCodigoDisciplina | EditNomeDisciplina | EditRequisitos | EditCursosPermitidos
          | EditCodTurma | EditProfTurma | EditDiscTurma | EditHorario | EditSala | EditMaxAlunosTurma
          | ListaAlunos
          | ListaProfessores
          | ListaTurmas
          | ListaDisciplinas
          | EditMatAluno | EditMatTurma
          | ListaSolicitacoes
          | ScrollRelatorio
          | BtnSim | BtnNao
          deriving (Ord, Show, Eq)

data Tela = TelaInicial | TelaMenu | TelaCadAluno | TelaCadProfessor | TelaCadDisciplina | TelaCadTurma 
          | TelaListaAlunos | TelaListaProfessores | TelaListaTurmas | TelaListaDisciplinas
          | TelaMatriculas | TelaAgenda | TelaCadSolicitacao | TelaListaSolicitacoes
          | TelaRelatorio | TelaConfirmacao | TelaConfirmacaoFimSemestre
    deriving (Eq, Show)

data AppState = AppState
  { _sistema     :: Sistema
  , _listaMenu   :: L.List Name String
  , _listaMenuAlunos :: L.List Name Aluno
  , _listaMenuProfessores :: L.List Name Professor
  , _listaMenuTurmas :: L.List Name Turma
  , _listaMenuDisciplinas :: L.List Name Disciplina
  , _listaMenuSolicitacoes :: L.List Name (Matricula, Int)
  , _textoRelatorio :: String
  , _telaAtiva   :: Tela
  , _mensagemErro :: Maybe String
  , _formularios :: M.Map Name (Editor String Name)
  , _foco        :: FocusRing Name
  }

makeLenses ''AppState