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

import qualified Models.Aluno as A
import BrickMenu.Tipos

-- FUNÇÃO AUXILIAR: Envolve qualquer conteúdo com a Logo e o limite de largura
templateUI :: String -> Widget Name -> Widget Name
templateUI titulo conteudo = 
    vBox [ padBottom (Pad 1) drawLogo
         , hCenter $ hLimit 70 $ borderWithLabel (str titulo) $
            padLeftRight 2 $ vBox [ str " ", conteudo, str " " ]
         ]

drawUI :: AppState -> [Widget Name]
drawUI s = [center $ ui]
  where
    ui = case s^.telaAtiva of
        TelaMenu          -> drawMenu s
        TelaCadAluno      -> templateUI " Cadastro de Aluno " $ drawForm [("Nome", EditNomeAluno), ("Matrícula", EditMatricula), ("Curso", EditCurso), ("CRA", EditCRA)] s
        TelaCadProfessor  -> templateUI " Cadastro de Professor " $ drawForm [("Matrícula", EditMatriculaProfessor), ("Nome", EditNomeProfessor), ("Departamento", EditDepto), ("Formação", EditFormacao)] s
        TelaCadDisciplina -> templateUI " Cadastro de Disciplina " $ drawForm [("Código", EditCodigoDisciplina), ("Nome", EditNomeDisciplina)] s
        TelaCadTurma      -> templateUI " Cadastro de Turma " $ drawForm [ ("Cod. Turma", EditCodTurma), ("ID Professor", EditProfTurma), ("Cod. Disciplina", EditDiscTurma), ("Horário", EditHorarioTurma), ("Qtd Max Alunos", EditMaxAlunosTurma) ] s
        TelaListaAlunos   -> drawListaAlunos s

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

drawLogo :: Widget Name
drawLogo = withAttr (attrName "logo") $ vBox
    [ hCenter $ str "  __  __ _      _   ___ _             "
    , hCenter $ str " |  \\/  (_)_ _ (_) / __(_)__ _ __ _ __ _ "
    , hCenter $ str " | |\\/| | | ' \\| | \\__ \\ / _` / _` / _` |"
    , hCenter $ str " |_|  |_|_|_||_|_| |___/_\\__, \\__,_\\__,_|"
    , hCenter $ str "                         |___/           "
    ]