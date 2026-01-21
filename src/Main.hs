{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import Sistema (carregarSistema)
import Foreign.C (eDEADLK)

app :: App AppState e Name
app = App { appDraw = drawUI
          , appChooseCursor = focusRingCursor (^.foco)
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const $ attrMap V.defAttr [ (L.listSelectedAttr, V.black `on` V.cyan)   
              , (attrName "logo", fg V.brightBlue `V.withStyle` V.bold)
              ]
          }

main :: IO ()
main = do
    sistema <- carregarSistema
    let todosOsCampos = [EditNomeAluno, EditMatricula, EditCurso, EditCRA, EditMatriculaProfessor, EditNomeProfessor, EditDepto, 
            EditFormacao, EditCodigoDisciplina, EditNomeDisciplina ,EditCodTurma, EditProfTurma, EditDiscTurma, EditHorarioTurma, EditMaxAlunosTurma]
    let initialForms = M.fromList [ (n, editor n (Just 1) "") | n <- todosOsCampos ]
    
    let initialState = AppState 
          { _sistema     = sistema
          , _listaMenu   = L.list MenuPrincipal (Vec.fromList ["Cadastrar Aluno", "Cadastrar Professor", "Cadastrar Disciplina", "Cadastrar Turma", "Listar Alunos"]) 1
          , _listaMenuAlunos = L.list ListaAlunos Vec.empty 1
          , _telaAtiva   = TelaMenu
          , _formularios = initialForms
          , _foco        = focusRing [EditNomeAluno]
          }
    _ <- defaultMain app initialState
    return ()

