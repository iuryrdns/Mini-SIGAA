{-# LANGUAGE OverloadedStrings #-}

module BrickMenu.UI (drawUI) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import Brick.Focus (focusGetCurrent)
import Lens.Micro ((^.))
import qualified Data.Map as M
import Brick.Widgets.Center (hCenter)
import Brick.AttrMap (attrName)
import Data.List (isInfixOf, intersperse)
import Brick.Widgets.Table  
import Data.List (isInfixOf, intersperse, isPrefixOf)
import Models.Disciplina (getNomeDisciplina)


import Models.Turma (Turma, getDisciplinaTurma, getDiaTurma, getHoraTurma, getCodigoTurma, Dia (..))
import Sistema (Sistema(..))
import Models.Matricula (Matricula, getIdAlunoMatricula, getIdTurmaMatricula) 

import qualified Models.Aluno as A
import qualified Models.Professor as B
import BrickMenu.Tipos

-- FUNÇÃO AUXILIAR: Envolve qualquer conteúdo com a Logo e o limite de largura
templateUI :: String -> Widget Name -> Widget Name
templateUI titulo conteudo = 
    vBox [ padBottom (Pad 1) drawLogo
         , hCenter $ hLimit 70 $ 
           withAttr (attrName "border") $
           borderWithLabel (str titulo) $
             padLeftRight 2 $ vBox [ str " ", conteudo, str " " ]
         ]

drawUI :: AppState -> [Widget Name]
drawUI s = [center $ vBox [ui, drawFeedback s]]
  where
    ui = case s^.telaAtiva of
        TelaMenu             -> drawMenu s
        TelaCadAluno         -> templateUI " Cadastro de Aluno " $ drawForm [("Nome", EditNomeAluno), ("Matrícula", EditMatricula), ("Curso", EditCurso), ("CRA", EditCRA)] s
        TelaCadProfessor     -> templateUI " Cadastro de Professor " $ drawForm [("Matrícula", EditMatriculaProfessor), ("Nome", EditNomeProfessor), ("Departamento", EditDepto), ("Formação", EditFormacao)] s
        TelaCadDisciplina    -> templateUI " Cadastro de Disciplina " $ drawForm [("Código", EditCodigoDisciplina), ("Nome", EditNomeDisciplina)] s
        TelaCadTurma         -> templateUI " Cadastro de Turma " $ drawForm [ ("Cod. Turma", EditCodTurma), ("ID Professor", EditProfTurma), ("Cod. Disciplina", EditDiscTurma), ("Dia (2-Seg a 6-Sex)", EditDiaTurma), ("Hora (ex: 08:00)", EditHoraTurma), ("Qtd Max Alunos", EditMaxAlunosTurma) ] s
        TelaListaAlunos      -> drawListaAlunos s
        TelaListaProfessores -> drawListaProfessores s
        TelaMatriculas       -> drawMatriculaMenu s
        TelaAgenda           -> drawAgenda s
        TelaCadSolicitacao -> templateUI " Solicitar Matrícula " $ drawForm [ ("Matrícula Aluno", EditMatAluno), ("Código da Turma", EditMatTurma) ] s
        TelaListaSolicitacoes -> drawListaSolicitacoes s

drawMenu :: AppState -> Widget Name
drawMenu s = templateUI " Menu Principal " $
    vBox [ L.renderList (\selected el -> 
            if selected 
            then withAttr L.listSelectedAttr (str $ "> " ++ el) 
            else padLeft (Pad 2) (str el)) True (s^.listaMenu)
         , str " "
         , hCenter $ str "[Enter] Selecionar | [Esc] Sair" 
         ]

drawForm :: [(String, Name)] -> AppState -> Widget Name
drawForm campos s = 
    vBox [ vBox $ map (drawField s) campos
         , str " "
         , hCenter $ str "[Tab] Prox. Campo | [Enter] Salvar | [Esc] Voltar" ]

drawField :: AppState -> (String, Name) -> Widget Name
drawField s (label, name) = 
    let ed = (s^.formularios) M.! name
        focado = focusGetCurrent (s^.foco) == Just name
    in hBox [ hLimit 20 $ padLeft Max (str label)
            , str ": "
            , renderEditor (str . Prelude.unlines) focado ed 
            ]

drawListaAlunos :: AppState -> Widget Name
drawListaAlunos s = templateUI " Lista de Alunos Cadastrados " $
    let desenhaLinha selecionado aluno = 
            let estilo = if selecionado then withAttr L.listSelectedAttr else id
                matricula = A.getMatriculaAluno aluno
                nome      = A.getNomeAluno aluno
                cra       = A.getCraAluno aluno
                info = show matricula ++ " - " ++ nome ++ " (CRA: " ++ show cra ++ ")"
            in estilo $ str info
    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuAlunos)
            , str " "
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]

drawListaProfessores :: AppState -> Widget Name
drawListaProfessores s = templateUI " Lista de Professores Cadastrados " $
    let desenhaLinha selecionado professor = 
            let estilo = if selecionado then withAttr L.listSelectedAttr else id
                matricula = B.getMatriculaProfessor professor
                nome      = B.getNomeProfessor professor
                info = show matricula ++ " - " ++ nome
            in estilo $ str info
    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuProfessores)
            , str " "
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]

drawMatriculaMenu :: AppState -> Widget Name
drawMatriculaMenu s = templateUI " Período de Matrículas (Fase 1) " $
    vBox [ L.renderList (\selected el -> 
            if selected 
            then withAttr L.listSelectedAttr (str $ "> " ++ el) 
            else padLeft (Pad 2) (str el)) True (s^.listaMenu)
         , str " "
         , hCenter $ str "[Enter] Selecionar | [Esc] Sair do Programa" 
         ]

drawLogo :: Widget Name
drawLogo = padTop (Pad 2) $ withAttr (attrName "logo") $ vBox
    [ hCenter $ str " __  __ _      _   ___ _             "
    , hCenter $ str " |  \\/  (_)_ _ (_) / __(_)__ _ __ _ __ _ "
    , hCenter $ str " | |\\/| | | ' \\| | \\__ \\ / _` / _` / _` |"
    , hCenter $ str " |_|  |_|_|_||_|_| |___/_\\__, \\__,_\\__,_|"
    , hCenter $ str "                         |___/           "
    ]

drawFeedback :: AppState -> Widget Name
drawFeedback st = case st^.mensagemErro of
    Nothing -> emptyWidget
    Just msg -> 
        let estilo = if "sucesso" `isInfixOf` msg || "realizada" `isInfixOf` msg
                     then attrName "sucesso"
                     else attrName "erro"
        in hCenter $ padTop (Pad 1) $ withAttr estilo $ str msg

drawAgenda :: AppState -> Widget Name
drawAgenda st =
    let sis = st^.sistema

        mapDiscs = _disciplinas sis
        turmas = M.elems (_turmas sis)
        
        diasDaSemana = [Segunda, Terca, Quarta, Quinta, Sexta]
        
        -- ID da Disciplina -> Nome da Disciplina
        buscarNomeDisciplina dId = 
            case M.lookup dId mapDiscs of
                Just d  -> getNomeDisciplina d
                Nothing -> dId

        horarios = [ ("Manhã I (08h-10h)", ["08", "09"])
                   , ("Manhã II (10h-12h)", ["10", "11"])
                   ]

        pertenceAoBloco t diaAlvo prefixos =
            getDiaTurma t == diaAlvo && any (`isPrefixOf` getHoraTurma t) prefixos

        getTurmasNoBloco diaAlvo prefixos =
            let filtradas = [ withAttr (attrName "sucesso") $ 
                              str $ buscarNomeDisciplina (getDisciplinaTurma t) ++ 
                                    " - " ++ show (getCodigoTurma t) ++ 
                                    " - " ++ getHoraTurma t
                            | t <- turmas
                            , pertenceAoBloco t diaAlvo prefixos
                            ]
            in if null filtradas 
               then padAll 1 $ str "---" 
               else padAll 1 $ vBox $ intersperse (str " ") filtradas

        header = map (withAttr (attrName "logo") . str) (" Horários " : map show diasDaSemana)

        makeRow (label, prefixos) =
            str label : [ getTurmasNoBloco d prefixos | d <- diasDaSemana ]

        tabela = renderTable $ 
                 columnBorders True $
                 rowBorders True $
                 setDefaultColAlignment AlignCenter $
                 table (header : map makeRow horarios)


    in center $ hLimit 130 $ 
       withAttr (attrName "border") $
       borderWithLabel (str " Agenda de Turmas ") $
       vBox [ tabela
            , fill ' ' 
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]

drawListaSolicitacoes :: AppState -> Widget Name
drawListaSolicitacoes s = templateUI " Solicitações Pendentes " $
    let sis = s^.sistema
        desenhaLinha selecionado matriculaObj = 
            let estilo = if selecionado then withAttr L.listSelectedAttr else id
                idA = getIdAlunoMatricula matriculaObj
                idT = getIdTurmaMatricula matriculaObj
                
                nomeA = maybe (show idA) A.getNomeAluno (M.lookup idA (_alunos sis))
                turma = M.lookup idT (_turmas sis)
                nomeD = case turma of
                    Just t -> maybe "Disc. s/ nome" getNomeDisciplina (M.lookup (getDisciplinaTurma t) (_disciplinas sis))
                    Nothing -> "Turma inex."
                
                info = nomeA ++ " -> " ++ nomeD ++ " (Turma " ++ show idT ++ ")"
            in estilo $ str info
            
    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuSolicitacoes)
            , str " "
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]

