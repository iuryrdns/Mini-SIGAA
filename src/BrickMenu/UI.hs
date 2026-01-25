{-# LANGUAGE OverloadedStrings #-}

module BrickMenu.UI (drawUI) where

import Brick
import Brick.AttrMap (attrName)
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import BrickMenu.Tipos
import Data.List (isInfixOf, sortOn)
import qualified Data.Map as M
import Lens.Micro ((^.))
import qualified Models.Aluno as A
import qualified Models.Disciplina as D
import qualified Models.Professor as B
import qualified Models.Turma as T
import Models.Types
import Sistema (Sistema (..))

-- FUNÇÃO AUXILIAR: Envolve qualquer conteúdo com a Logo e o limite de largura
templateUI :: String -> Widget Name -> Widget Name
templateUI titulo conteudo =
  vBox
    [ padBottom (Pad 1) drawLogo,
      hCenter $
        hLimit 70 $
          withAttr (attrName "border") $
            borderWithLabel (str titulo) $
              padLeftRight 2 $
                vBox [str " ", conteudo, str " "]
    ]

drawUI :: AppState -> [Widget Name]
drawUI s = [center $ vBox [ui, drawFeedback s]]
  where
    ui = case s ^. telaAtiva of
      TelaInicial -> drawTelaInicial s
      TelaMenu -> drawMenu s
      TelaCadAluno -> templateUI " Cadastro de Aluno " $ drawForm [("Nome", EditNomeAluno), ("Matrícula", EditMatricula), ("Curso", EditCurso), ("CRA", EditCRA)] s
      TelaCadProfessor -> templateUI " Cadastro de Professor " $ drawForm [("Matrícula", EditMatriculaProfessor), ("Nome", EditNomeProfessor), ("Departamento", EditDepto), ("Formação", EditFormacao)] s
      TelaCadDisciplina -> templateUI " Cadastro de Disciplina " $ drawForm [("Código", EditCodigoDisciplina), ("Nome", EditNomeDisciplina), ("Requisitos (sep. espaço)", EditRequisitos), ("Cursos (sep. espaço)", EditCursosPermitidos)] s
      TelaCadTurma -> templateUI " Cadastro de Turma " $ drawForm [("Cod. Turma", EditCodTurma), ("Mat. Professor", EditProfTurma), ("Cod. Disciplina", EditDiscTurma), ("Horário", EditHorario), ("Sala", EditSala), ("Qtd Max Alunos", EditMaxAlunosTurma)] s
      TelaListaAlunos -> drawListaAlunos s
      TelaListaProfessores -> drawListaProfessores s
      TelaListaTurmas -> drawListaTurmas s
      TelaListaDisciplinas -> drawListaDisciplinas s
      TelaMatriculas -> drawMatriculaMenu s
      TelaAgenda -> drawListaTurmas s -- Legacy fallback
      TelaCadSolicitacao -> templateUI " Cadastrar Matrícula " $ drawForm [("Matrícula Aluno", EditMatAluno), ("Código da Turma", EditMatTurma)] s
      TelaListaSolicitacoes -> drawListaSolicitacoes s
      TelaRelatorio -> drawRelatorio s
      TelaConfirmacao -> drawConfirmacao
      TelaConfirmacaoFimSemestre -> drawConfirmacaoFimSemestre

drawTelaInicial :: AppState -> Widget Name
drawTelaInicial st =
  center $
    withAttr (attrName "border") $
      borderWithLabel (str " Bem-vindo ao Mini-SIGAA ") $
        padAll 2 $
          vBox
            [ drawLogo,
              str " ",
              hCenter $ str (maybe "Sistema carregado." id (st ^. mensagemErro)),
              str " ",
              hCenter $ withAttr (attrName "sucesso") $ str "Pressione [Enter] para iniciar"
            ]

drawMenu :: AppState -> Widget Name
drawMenu s =
  templateUI " Menu Principal " $
    vBox
      [ L.renderList
          ( \selected el ->
              if selected
                then withAttr L.listSelectedAttr (str $ "> " ++ el)
                else padLeft (Pad 2) (str el)
          )
          True
          (s ^. listaMenu),
        str " ",
        hCenter $ str "[Enter] Selecionar | [Esc] Sair"
      ]

drawForm :: [(String, Name)] -> AppState -> Widget Name
drawForm campos s =
  vBox
    [ vBox $ map (drawField s) campos,
      str " ",
      hCenter $ str "[Tab] Prox. Campo | [Enter] Salvar | [Esc] Voltar"
    ]

drawField :: AppState -> (String, Name) -> Widget Name
drawField s (label, name) =
  let ed = (s ^. formularios) M.! name
      focado = focusGetCurrent (s ^. foco) == Just name
   in hBox
        [ hLimit 20 $ padLeft Max (str label),
          str ": ",
          renderEditor (str . Prelude.unlines) focado ed
        ]

checkEmpty :: (Foldable t) => t a -> Widget Name -> Widget Name
checkEmpty l widget = if null l then center $ str "Nenhum registro encontrado." else widget

drawListaAlunos :: AppState -> Widget Name
drawListaAlunos s =
  templateUI " Lista de Alunos Cadastrados " $
    let desenhaLinha selecionado aluno =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              matricula = unMatricula $ A.getMatriculaAluno aluno
              nome = unNome $ A.getNomeAluno aluno
              cra = unCRA $ A.getCraAluno aluno
              info = show matricula ++ " - " ++ nome ++ " (CRA: " ++ show cra ++ ")"
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (s ^. listaMenuAlunos) $ L.renderList desenhaLinha True (s ^. listaMenuAlunos),
            str " ",
            hCenter $ str "[Esc] Voltar ao Menu"
          ]

drawListaProfessores :: AppState -> Widget Name
drawListaProfessores s =
  templateUI " Lista de Professores Cadastrados " $
    let desenhaLinha selecionado professor =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              matricula = unMatricula $ B.getMatriculaProfessor professor
              nome = unNome $ B.getNomeProfessor professor
              info = show matricula ++ " - " ++ nome
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (s ^. listaMenuProfessores) $ L.renderList desenhaLinha True (s ^. listaMenuProfessores),
            str " ",
            hCenter $ str "[Esc] Voltar ao Menu"
          ]

drawListaDisciplinas :: AppState -> Widget Name
drawListaDisciplinas s =
  templateUI " Lista de Disciplinas " $
    let desenhaLinha selecionado disc =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              cod = unCodigo $ D.getCodigoDisciplina disc
              nome = unNome $ D.getNomeDisciplina disc
              info = cod ++ " - " ++ nome
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (s ^. listaMenuDisciplinas) $ L.renderList desenhaLinha True (s ^. listaMenuDisciplinas),
            str " ",
            hCenter $ str "[Esc] Voltar ao Menu"
          ]

drawMatriculaMenu :: AppState -> Widget Name
drawMatriculaMenu s =
  let fase = _fase (s ^. sistema)
      titulo = case fase of
        2 -> " Período de Matrículas (Fase 2) "
        3 -> " Período de Rematrículas (Fase 3) "
        _ -> " Menu de Matrículas "
   in templateUI titulo $
        vBox
          [ L.renderList
              ( \selected el ->
                  if selected
                    then withAttr L.listSelectedAttr (str $ "> " ++ el)
                    else padLeft (Pad 2) (str el)
              )
              True
              (s ^. listaMenu),
            str " ",
            hCenter $ str "[Enter] Selecionar | [Esc] Sair do Programa"
          ]

drawLogo :: Widget Name
drawLogo =
  padTop (Pad 2) $
    withAttr (attrName "logo") $
      vBox
        [ hCenter $ str "__  __ _      _   ___ _             ",
          hCenter $ str "  |  \\/  (_)_ _ (_) / __(_)__ _ __ _ __ _ ",
          hCenter $ str "  | |\\/| | | ' \\| | \\__ \\ / _` / _` / _` |",
          hCenter $ str "  |_|  |_|_|_||_|_| |___/_\\__, \\__,_\\__,_|",
          hCenter $ str "                           |___/           "
        ]

drawFeedback :: AppState -> Widget Name
drawFeedback st = case st ^. mensagemErro of
  Nothing -> emptyWidget
  Just msg ->
    let estilo =
          if "sucesso" `isInfixOf` msg || "realizada" `isInfixOf` msg
            then attrName "sucesso"
            else attrName "erro"
     in hCenter $ padTop (Pad 1) $ withAttr estilo $ str msg

drawListaTurmas :: AppState -> Widget Name
drawListaTurmas st =
  templateUI " Lista de Turmas (Ativas/Pendentes) " $
    let desenhaLinha selecionado turma =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              codT = T.getCodigoTurma turma
              codD = unCodigo $ T.getDisciplinaTurma turma
              prof = unMatricula $ T.getProfessorTurma turma
              horario = show $ T.getHorarioTurma turma
              sala = T.getSalaTurma turma

              info = "T" ++ show codT ++ " | " ++ codD ++ " | Prof " ++ show prof ++ " | " ++ horario ++ " | Sala " ++ sala
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (st ^. listaMenuTurmas) $ L.renderList desenhaLinha True (st ^. listaMenuTurmas),
            str " ",
            hCenter $ str "[Esc] Voltar ao Menu"
          ]

drawRelatorio :: AppState -> Widget Name
drawRelatorio st =
  templateUI " Relatório Geral " $
    vBox
      [ vLimit 20 $ viewport ScrollRelatorio Vertical (str (st ^. textoRelatorio)),
        str " ",
        hCenter $ str "[Esc] Voltar | [Setas] Rolar"
      ]

drawSection :: String -> Widget Name -> Widget Name
drawSection title w =
  vBox
    [ padTop (Pad 1) $ withAttr (attrName "border") $ hBorderWithLabel (str title),
      padTop (Pad 1) $ padLeft (Pad 2) w
    ]

drawAlunosReport :: Sistema -> Widget Name
drawAlunosReport s =
  let alunos = sortOn (unNome . A.getNomeAluno) $ M.elems (_alunos s)
   in if null alunos
        then str "Nenhum aluno cadastrado."
        else vBox $ map mkRow alunos
  where
    mkRow a =
      str $
        show (unMatricula $ A.getMatriculaAluno a)
          ++ " - "
          ++ unNome (A.getNomeAluno a)
          ++ " ("
          ++ unCurso (A.getCursoAluno a)
          ++ ", CRA: "
          ++ show (unCRA $ A.getCraAluno a)
          ++ ")"

drawProfessoresReport :: Sistema -> Widget Name
drawProfessoresReport s =
  let profs = sortOn (unNome . B.getNomeProfessor) $ M.elems (_professores s)
   in if null profs
        then str "Nenhum professor cadastrado."
        else vBox $ map mkRow profs
  where
    mkRow p =
      str $
        show (unMatricula $ B.getMatriculaProfessor p)
          ++ " - "
          ++ unNome (B.getNomeProfessor p)
          ++ " ("
          ++ B.getDepartamentoProfessor p
          ++ ")"

drawDisciplinasReport :: Sistema -> Widget Name
drawDisciplinasReport s =
  let discs = sortOn (unNome . D.getNomeDisciplina) $ M.elems (_disciplinas s)
   in if null discs
        then str "Nenhuma disciplina cadastrada."
        else vBox $ map mkRow discs
  where
    mkRow d = str $ unCodigo (D.getCodigoDisciplina d) ++ " - " ++ unNome (D.getNomeDisciplina d)

drawTurmasReport :: Sistema -> Widget Name
drawTurmasReport s =
  let active = M.elems (_turmas s)
      pending = M.elems (_cadastroDeTurmas s)
      allTurmas = sortOn T.getCodigoTurma (active ++ pending)

      buscaDisc cod = case M.lookup cod (_disciplinas s) of
        Just d -> unNome (D.getNomeDisciplina d)
        Nothing -> unCodigo cod
   in if null allTurmas
        then str "Nenhuma turma cadastrada."
        else vBox $ map (mkRow buscaDisc) allTurmas
  where
    mkRow findDisc t =
      str $
        "T"
          ++ show (T.getCodigoTurma t)
          ++ " - "
          ++ findDisc (T.getDisciplinaTurma t)
          ++ " ("
          ++ show (T.getHorarioTurma t)
          ++ " | Sala "
          ++ T.getSalaTurma t
          ++ ")"

drawMatriculasReport :: Sistema -> Widget Name
drawMatriculasReport s =
  let matriculas = _matriculas s -- [(Matricula, Int)]
      buscaAluno mat = case M.lookup mat (_alunos s) of
        Just a -> unNome (A.getNomeAluno a)
        Nothing -> show (unMatricula mat)
   in if null matriculas
        then str "Nenhuma matrícula efetivada."
        else vBox $ map (mkRow buscaAluno) matriculas
  where
    mkRow findAluno (m, tId) = str $ findAluno m ++ " -> Turma " ++ show tId

drawListaSolicitacoes :: AppState -> Widget Name
drawListaSolicitacoes s =
  templateUI " Matrículas Realizadas " $
    let sis = s ^. sistema
        desenhaLinha selecionado (idA, idT) =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id

              nomeA = case M.lookup idA (_alunos sis) of
                Just a -> unNome (A.getNomeAluno a)
                Nothing -> "Aluno " ++ show (unMatricula idA)

              nomeD = case M.lookup idT (_turmas sis) of
                Just t ->
                  case M.lookup (T.getDisciplinaTurma t) (_disciplinas sis) of
                    Just d -> unNome (D.getNomeDisciplina d)
                    Nothing -> "Disc. " ++ unCodigo (T.getDisciplinaTurma t)
                Nothing -> "Turma " ++ show idT

              info = nomeA ++ " -> " ++ nomeD ++ " (Turma " ++ show idT ++ ")"
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (s ^. listaMenuSolicitacoes) $ L.renderList desenhaLinha True (s ^. listaMenuSolicitacoes),
            str " ",
            hCenter $ str "[Esc] Voltar ao Menu"
          ]

drawConfirmacao :: Widget Name
drawConfirmacao =
  center $
    withAttr (attrName "border") $
      borderWithLabel (str " Confirmação ") $
        padAll 2 $
          vBox
            [ hCenter $ str "Deseja salvar as alterações antes de sair?",
              str " ",
              hCenter $
                hBox
                  [ str "[S]im (Salvar e Sair)",
                    str "   ",
                    str "[N]ão (Sair sem Salvar)"
                  ],
              str " ",
              hCenter $ str "[Esc] Cancelar"
            ]

drawConfirmacaoFimSemestre :: Widget Name
drawConfirmacaoFimSemestre =
  center $
    withAttr (attrName "border") $
      borderWithLabel (str " Finalizar Semestre ") $
        padAll 2 $
          vBox
            [ hCenter $ str "Deseja salvar as alterações ao finalizar o semestre?",
              str " ",
              hCenter $
                hBox
                  [ str "[S]im (Salvar)",
                    str "   ",
                    str "[N]ão (Não Salvar)"
                  ]
            ]