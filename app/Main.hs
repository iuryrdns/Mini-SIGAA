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

import Sistema (Sistema)
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
    (sistema, msgLog) <- carregarSistema
    
    let todosOsCampos = [ EditNomeAluno, EditMatricula, EditCurso, EditCRA
                        , EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao
                        , EditCodigoDisciplina, EditNomeDisciplina, EditRequisitos, EditCursosPermitidos
                        , EditCodTurma, EditProfTurma, EditDiscTurma, EditHorario, EditSala, EditMaxAlunosTurma
                        , EditMatAluno, EditMatTurma
                        , EditNotaMatricula, EditNotaCodDisc, EditNotaValor
                        , EditPendenteSala, EditPendenteHorario
                        ]
    
    let initialForms = M.fromList [ (n, editor n (Just 1) "") | n <- todosOsCampos ]

    let initialState = AppState
          { _sistema              = sistema
          , _listaMenu            = L.list MenuPrincipal Vec.empty 1
          , _listaMenuAlunos      = L.list ListaAlunos Vec.empty 1
          , _listaMenuProfessores = L.list ListaProfessores Vec.empty 1
          , _listaMenuTurmas      = L.list ListaTurmas Vec.empty 1
          , _listaMenuDisciplinas = L.list ListaDisciplinas Vec.empty 1
          , _listaMenuSolicitacoes = L.list ListaSolicitacoes Vec.empty 1
          , _listaConflitos       = L.list ListaConflitos Vec.empty 1
          , _turmaEmEdicao        = Nothing
          , _textoRelatorio       = ""
          , _telaAtiva            = TelaInicial
          , _mensagemErro         = Just (msgLog ++ " Pressione Enter.")
          , _formularios          = initialForms
          , _foco                 = focusRing [] 
          }
    
    _ <- defaultMain app initialState
    return ()