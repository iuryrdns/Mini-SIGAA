{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Menu.Tipos
Description : Definições de tipos centrais e estado da aplicação.

Este módulo centraliza os tipos de dados usados na interface Brick,
incluindo o 'AppState', os identificadores de widgets ('Name') e as
definições das telas disponíveis no sistema.
-}

module Menu.Tipos where

-- Bibliotecas Externas
import Brick.Widgets.Edit (Editor)
import Brick.Focus (FocusRing)
import Lens.Micro.TH (makeLenses)
import qualified Brick.Widgets.List as L
import qualified Data.Map as M

-- Módulos de Negócio
import Sistema (Sistema)
import Models.Aluno (Aluno, NotasDisciplina)
import Models.Professor (Professor)
import Models.Disciplina (Disciplina)
import Models.Types (Solicitacao(..), ResultadoProcessamento(..))

-------------------------------------------------------------------------------
-- Identificadores (Names)
-------------------------------------------------------------------------------

-- | Identificadores únicos para os widgets da interface.
-- Essencial para o controle de foco e captura de eventos de editores e listas.
data Name = MenuPrincipal 
          --- Campos Aluno
          | EditNomeAluno | EditMatricula | EditCurso | EditCRA 
          --- Campos Professor
          | EditMatriculaProfessor | EditNomeProfessor | EditDepto | EditFormacao
          --- Campos Disciplina
          | EditCodigoDisciplina | EditNomeDisciplina | EditPreRequisitosDisciplina 
          | EditPeriodoDisciplina 
          --- Campos Turma
          | EditCodTurma | EditProfTurma | EditDiscTurma | EditHorarioTurma 
          | EditMaxAlunosTurma | EditSalaTurma
          -- Identificadores de Listas
          | ListaAlunos
          | ListaProfessores
          | ListaDisciplinas
          | ListaSolicitacoes
          | ListaResultados
          --- Campos Matrícula
          | EditMatAluno | EditMatTurma
          | EditConsultaMatricula
          --- Campos Notas
          | EditMatriculaNota | EditTurmaNota | EditNota1 | EditNota2 | EditNota3 | EditNotaFinal
          | EditConsultaNotaMatricula | EditListaNotas
          -- Viewport para Agenda 
          | AgendaViewport
          deriving (Ord, Show, Eq)

-------------------------------------------------------------------------------
-- Navegação
-------------------------------------------------------------------------------

-- | Representa as diferentes telas/estados visuais da aplicação.
data Tela = TelaMenu 
          | TelaCadAluno 
          | TelaCadProfessor 
          | TelaCadDisciplina 
          | TelaCadTurma 
          | TelaListaAlunos 
          | TelaListaProfessores 
          | TelaListaDisciplinas 
          | TelaAgenda
          | TelaMatriculas 
          | TelaCadSolicitacao 
          | TelaListaSolicitacoes 
          | TelaMenuNotas 
          | TelaListaResultados
          | TelaInserirNotas 
          | TelaConsultarNotas
          | TelaExibirNotasAluno
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Estado Global (AppState)
-------------------------------------------------------------------------------

-- | O estado consolidado da aplicação.
-- Contém os dados de negócio (Sistema) e o estado da interface (Listas, Foco, Campos).
data AppState = AppState
  { _sistema              :: Sistema              -- ^ Dados lógicos do SIGAA
  , _listaMenu            :: L.List Name String   -- ^ Menu principal de navegação
  , _listaMenuAlunos      :: L.List Name Aluno    -- ^ Visualização de alunos
  , _listaMenuProfessores :: L.List Name Professor-- ^ Visualização de professores
  , _listaMenuDisciplinas :: L.List Name Disciplina -- ^ Visualização de disciplinas
  , _listaMenuSolicitacoes:: L.List Name Solicitacao -- ^ Visualização de matrículas
  , _listaMenuResultados  :: L.List Name ResultadoProcessamento -- ^ Visualização de resultados de matrículas
  , _listaMenuNotas       :: L.List Name (Int, NotasDisciplina) -- ^ Visualização de notas
  , _telaAtiva            :: Tela                 -- ^ Controle de navegação atual
  , _mensagemErro         :: Maybe String         -- ^ Feedback para o usuário
  , _formularios          :: M.Map Name (Editor String Name) -- ^ Coleção de campos de texto
  , _foco                 :: FocusRing Name       -- ^ Gerenciador de foco (TAB)
  }

-- Geração automática de Lenses para os campos do AppState
makeLenses ''AppState