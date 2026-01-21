{-# LANGUAGE OverloadedStrings #-}
module Main where

import Models.Aluno (criarAluno)
import Models.Disciplina (criarDisciplina)
import Models.Professor (criarProfessor)
import Models.Matricula (criarMatricula)
import Sistema
    ( Sistema(_matriculas),
      abrirPeriodoMatriculas,
      cadastrarAluno,
      cadastrarDisciplina,
      cadastrarProfessor,
      getAlunos,
      getFase,
      getProfessores,
      sistemaVazio,
      cadastrarTurma,
      carregarSistema )
import System.IO (hFlush, stdout)
import Models.Turma (criarTurma)

import Brick
import Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Map as M
import Brick.Focus (focusRing, focusRingCursor)
import Lens.Micro ((^.))
import Brick.AttrMap (attrName)

import BrickMenu.Tipos
import BrickMenu.UI (drawUI)
import BrickMenu.Events (handleEvent)
import BrickMenu.Tipos (AppState(_listaMenu))
import qualified GHC.Generics as Models

app :: App AppState e Name
app = App { appDraw = drawUI
          , appChooseCursor = focusRingCursor (^.foco)
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const $ attrMap V.defAttr 
              [ (attrName "erro", fg V.red `V.withStyle` V.bold)
              , (attrName "sucesso", fg V.green `V.withStyle` V.bold)
              -- Cor das Bordas
              , (attrName "border", fg V.cyan) 
              -- Cor do Logo/Cabeçalhos
              , (attrName "logo", fg V.yellow `V.withStyle` V.bold)
              -- Cores do Menu (Item selecionado)
              , (L.listSelectedAttr, V.black `on` V.yellow)
              , (L.listSelectedFocusedAttr, V.black `on` V.cyan)
              ]
          }

main :: IO ()
main = do
    sistema <- carregarSistema
    
    let faseAtual = getFase sistema
    let telaInicial = if faseAtual == 1 then TelaMatriculas else TelaMenu
    
    let opcoesMenu = if faseAtual == 1
                     then ["Cadastrar Matrícula", "Mostrar Solicitações", "Sair"]
                     else ["Cadastrar Aluno", "Cadastrar Professor", "Cadastrar Disciplina", "Cadastrar Turma", "Listar Alunos", "Listar Professores", "Listar Turmas", "Período de Matrículas"]

    let todosOsCampos = [ EditNomeAluno, EditMatricula, EditCurso, EditCRA, EditMatriculaProfessor, EditNomeProfessor
                        , EditDepto, EditFormacao, EditCodigoDisciplina, EditNomeDisciplina, EditCodTurma
                        , EditProfTurma, EditDiscTurma, EditDiaTurma, EditHoraTurma, EditMaxAlunosTurma, EditMatAluno, EditMatTurma
                        ]
    
    let initialForms = M.fromList [ (n, editor n (Just 1) "") | n <- todosOsCampos ]

    let initialState = AppState
          { _sistema              = sistema
          , _listaMenu            = L.list MenuPrincipal (Vec.fromList opcoesMenu) 1
          , _listaMenuAlunos      = L.list ListaAlunos Vec.empty 1
          , _listaMenuProfessores = L.list ListaProfessores Vec.empty 1
          , _listaMenuSolicitacoes = L.list ListaSolicitacoes Vec.empty 1
          , _telaAtiva            = telaInicial
          , _mensagemErro         = Nothing
          , _formularios          = initialForms
          , _foco                 = focusRing [] 
          }
    _ <- defaultMain app initialState
    return ()