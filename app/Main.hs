{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Map as M
import Brick.Focus (focusRing, focusRingCursor)
import Lens.Micro ((^.))
import Brick.AttrMap (attrMap, attrName)
import Brick.Util (fg, on)
import Brick.Main (App(..), defaultMain)
import Brick.Widgets.Edit (editor)

import Sistema (Sistema, getFase)
import Utils.Database (carregarSistema)
import BrickMenu.Tipos
import BrickMenu.UI (drawUI)
import BrickMenu.Events (handleEvent)

app :: App AppState e Name
app = App { appDraw = drawUI
          , appChooseCursor = focusRingCursor (^.foco)
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const $ attrMap V.defAttr 
              [ (attrName "erro", fg V.red `V.withStyle` V.bold)
              , (attrName "sucesso", fg V.green `V.withStyle` V.bold)
              , (attrName "border", fg V.cyan) 
              , (attrName "logo", fg V.yellow `V.withStyle` V.bold)
              , (L.listSelectedAttr, V.black `on` V.yellow)
              , (L.listSelectedFocusedAttr, V.black `on` V.cyan)
              ]
          }

main :: IO ()
main = do
    (sistema, msgCarregamento) <- carregarSistema
    
    let faseAtual = getFase sistema
    let telaInicial = TelaInicial 
    
    let opcoesMenu = case faseAtual of
                     0 -> [ "Cadastrar Aluno", "Cadastrar Professor", "Cadastrar Disciplina", "Cadastrar Turma"
                          , "Listar Alunos", "Listar Professores", "Listar Turmas", "Listar Disciplinas"
                          , "Visualizar Relatório Geral", "Iniciar Matrículas", "Sair"
                          ]
                     1 -> ["Cadastrar Matrícula", "Mostrar Solicitações", "Encerrar Matrículas", "Sair"]
                     2 -> ["Visualizar Relatório Geral", "Iniciar Novo Semestre", "Sair"]
                     _ -> ["Sair"]

    let todosOsCampos = [ EditNomeAluno, EditMatricula, EditCurso, EditCRA
                        , EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao
                        , EditCodigoDisciplina, EditNomeDisciplina, EditRequisitos, EditCursosPermitidos
                        , EditCodTurma, EditProfTurma, EditDiscTurma, EditHorario, EditSala, EditMaxAlunosTurma
                        , EditMatAluno, EditMatTurma
                        ]
    
    let initialForms = M.fromList [ (n, editor n (Just 1) "") | n <- todosOsCampos ]

    let initialState = AppState
          { _sistema              = sistema
          , _listaMenu            = L.list MenuPrincipal (Vec.fromList opcoesMenu) 1
          , _listaMenuAlunos      = L.list ListaAlunos Vec.empty 1
          , _listaMenuProfessores = L.list ListaProfessores Vec.empty 1
          , _listaMenuTurmas      = L.list ListaTurmas Vec.empty 1
          , _listaMenuDisciplinas = L.list ListaDisciplinas Vec.empty 1
          , _listaMenuSolicitacoes = L.list ListaSolicitacoes Vec.empty 1
          , _textoRelatorio       = ""
          , _telaAtiva            = telaInicial
          , _mensagemErro         = Just msgCarregamento
          , _formularios          = initialForms
          , _foco                 = focusRing [] 
          }
    _ <- defaultMain app initialState
    return ()
