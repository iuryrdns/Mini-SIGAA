{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Ponto de entrada principal do Mini-SIGAA.

Este módulo inicializa o estado da aplicação, define o mapa de atributos
visuais (estilos) e inicia o loop de eventos da biblioteca Brick.
-}

module Main (main) where

-- Bibliotecas Externas
import Brick
import Brick.Widgets.Edit (editor)
import Brick.Focus (focusRing, focusRingCursor)
import Lens.Micro ((^.))
import Control.Monad (void)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Map as M

-- Módulos Internos (Biblioteca Mini-SIGAA)
import Sistema (getFase)
import Menu.Tipos
import Menu.UI.UI (drawUI)
import Menu.Eventos.Events (handleEvent)
import Utils.Database (carregarSistema)

-------------------------------------------------------------------------------
-- Configuração da Aplicação
-------------------------------------------------------------------------------

-- | Definição do objeto 'App' do Brick.
-- Une o desenho da UI, controle de foco e tratamento de eventos.
app :: App AppState e Name
app = App 
    { appDraw         = drawUI
    , appChooseCursor = focusRingCursor (^.foco)
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const styles
    }

-- | Mapa de atributos (estilos) para a interface.
styles :: AttrMap
styles = attrMap V.defAttr 
    [ (attrName "erro",                fg V.red `V.withStyle` V.bold)
    , (attrName "sucesso",             fg V.green `V.withStyle` V.bold)
    , (attrName "border",              fg V.cyan) 
    , (attrName "logo",                fg V.yellow `V.withStyle` V.bold)
    , (attrName "periodo1",            fg V.cyan)
    , (attrName "periodo2",            fg V.yellow)
    , (L.listSelectedAttr,             V.black `on` V.yellow)
    , (L.listSelectedFocusedAttr,      V.black `on` V.cyan)
    ]

-------------------------------------------------------------------------------
-- Inicialização e Main
-------------------------------------------------------------------------------

-- | Função principal que prepara o sistema e inicia a interface.
main :: IO ()
main = do
    -- Carrega os dados persistentes (JSON)
    sis <- carregarSistema
    
    let faseAtual = getFase sis
    
    -- 1. Define a Tela Inicial e as Opções do Menu baseadas na Fase do Sistema
    let (telaInicial, opcoesMenu) = case faseAtual of
            1 -> ( TelaMatriculas
                 , ["Cadastrar Matrícula", "Mostrar Solicitações", "Encerrar Período de Matrículas", "Sair"] 
                 )
            2 -> ( TelaMenuNotas
                 , ["Inserir Nota", "Consultar Notas", "Finalizar Semestre", "Sair"] 
                 )
            _ -> ( TelaMenu
                 , [ "Cadastrar Aluno", "Cadastrar Professor", "Cadastrar Disciplina"
                   , "Cadastrar Turma", "Listar Alunos", "Listar Professores"
                   , "Listar Turmas",  "Listar Disciplinas", "Período de Matrículas", "Sair"
                   ] 
                 )

    -- 2. Inicializa o Map de formulários com editores vazios
    let initialForms = M.fromList [ (n, editor n (Just 1) "") | n <- todosOsCampos ]
    
    -- 3. Monta o estado inicial da aplicação
    let initialState = AppState
          { _sistema              = sis
          , _listaMenu            = L.list MenuPrincipal (Vec.fromList opcoesMenu) 1
          , _listaMenuAlunos      = L.list ListaAlunos Vec.empty 1
          , _listaMenuProfessores = L.list ListaProfessores Vec.empty 1
          , _listaMenuDisciplinas  = L.list ListaDisciplinas Vec.empty 1
          , _listaMenuSolicitacoes = L.list ListaSolicitacoes Vec.empty 1
          , _listaMenuNotas        = L.list EditListaNotas Vec.empty 1
          , _telaAtiva            = telaInicial
          , _mensagemErro         = Nothing
          , _formularios          = initialForms
          , _foco                 = focusRing [] 
          }


    void $ defaultMain app initialState

-- | Lista exaustiva de todos os campos de edição do sistema para inicialização.
todosOsCampos :: [Name]
todosOsCampos = 
    [ EditNomeAluno, EditMatricula, EditCurso, EditCRA
    , EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao
    , EditCodigoDisciplina, EditNomeDisciplina, EditPreRequisitosDisciplina, EditPeriodoDisciplina
    , EditCodTurma, EditProfTurma, EditDiscTurma, EditHorarioTurma, EditMaxAlunosTurma
    , EditMatAluno, EditMatTurma
    , EditMatriculaNota, EditTurmaNota, EditNota1, EditNota2, EditNota3, EditConsultaNotaMatricula, EditListaNotas
    ]