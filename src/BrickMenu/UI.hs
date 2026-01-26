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
import Data.List (isInfixOf)
import qualified Data.Map as M
import Lens.Micro ((^.))

import qualified Models.Aluno as A
import qualified Models.Disciplina as D
import qualified Models.Professor as B
import qualified Models.Turma as T
import Models.Types (unMatricula, unNome, unCodigo, unCurso, unCRA, Matricula, Codigo)
import Sistema (Sistema (..), getFase)

templateUI :: String -> Widget Name -> Widget Name
templateUI titulo conteudo =
  vBox
    [ padBottom (Pad 1) drawLogo,
      hCenter $
        hLimit 80 $
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
      TelaCadDisciplina -> templateUI " Cadastro de Disciplina " $ drawForm [("Código", EditCodigoDisciplina), ("Nome", EditNomeDisciplina), ("Requisitos", EditRequisitos), ("Cursos", EditCursosPermitidos)] s
      TelaCadTurma -> templateUI " Cadastro de Turma " $ drawForm [("Cód. Turma", EditCodTurma), ("Mat. Prof.", EditProfTurma), ("Cód. Disc.", EditDiscTurma), ("Horário", EditHorario), ("Sala", EditSala), ("Capacidade", EditMaxAlunosTurma)] s
      
      TelaListaAlunos -> drawListaAlunos s
      TelaListaProfessores -> drawListaProfessores s
      TelaListaTurmas -> drawListaTurmas s
      TelaListaDisciplinas -> drawListaDisciplinas s
      TelaConflitos -> drawConflitos s
      
      TelaCadSolicitacao -> templateUI " Cadastrar Solicitação " $ drawForm [("Mat. Aluno", EditMatAluno), ("Cód. Turma", EditMatTurma)] s
      TelaListaSolicitacoes -> drawListaSolicitacoes s
      TelaLancarNotas -> templateUI " Lançamento de Notas " $ drawForm [("Mat. Aluno", EditNotaMatricula), ("Cód. Disc.", EditNotaCodDisc), ("Nota", EditNotaValor)] s
      
      TelaEditarTurma -> templateUI " Editar Turma Pendente " $ drawForm [("Nova Sala (Vazio=Manter)", EditPendenteSala), ("Novo Horário (Vazio=Manter)", EditPendenteHorario)] s

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
  let fase = getFase (s ^. sistema)
      titulo = case fase of
        0 -> " Fase 0: Planejamento (Alterações) "
        1 -> " Fase 1: Matrículas "
        2 -> " Fase 2: Rematrículas "
        4 -> " Fase 4: Menu Acadêmico "
        _ -> " Menu Principal "
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
        [ hLimit 25 $ padLeft Max (str label),
          str ": ",
          renderEditor (str . Prelude.unlines) focado ed
        ]

checkEmpty :: (Foldable t) => t a -> Widget Name -> Widget Name
checkEmpty l widget = if null l then center $ str "Nenhum registro encontrado." else widget

drawListaAlunos :: AppState -> Widget Name
drawListaAlunos s =
  templateUI " Lista de Alunos " $
    let desenhaLinha selecionado aluno =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              info = show (unMatricula $ A.getMatriculaAluno aluno) ++ " - " ++ unNome (A.getNomeAluno aluno)
           in estilo $ str info
     in vBox [vLimit 15 $ checkEmpty (s ^. listaMenuAlunos) $ L.renderList desenhaLinha True (s ^. listaMenuAlunos), str " ", hCenter $ str "[Esc] Voltar"]

drawListaProfessores :: AppState -> Widget Name
drawListaProfessores s =
  templateUI " Lista de Professores " $
    let desenhaLinha selecionado professor =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              info = show (unMatricula $ B.getMatriculaProfessor professor) ++ " - " ++ unNome (B.getNomeProfessor professor)
           in estilo $ str info
     in vBox [vLimit 15 $ checkEmpty (s ^. listaMenuProfessores) $ L.renderList desenhaLinha True (s ^. listaMenuProfessores), str " ", hCenter $ str "[Esc] Voltar"]

drawListaDisciplinas :: AppState -> Widget Name
drawListaDisciplinas s =
  templateUI " Lista de Disciplinas " $
    let desenhaLinha selecionado disc =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              info = unCodigo (D.getCodigoDisciplina disc) ++ " - " ++ unNome (D.getNomeDisciplina disc)
           in estilo $ str info
     in vBox [vLimit 15 $ checkEmpty (s ^. listaMenuDisciplinas) $ L.renderList desenhaLinha True (s ^. listaMenuDisciplinas), str " ", hCenter $ str "[Esc] Voltar"]

drawListaTurmas :: AppState -> Widget Name
drawListaTurmas st =
  let fase = getFase (st ^. sistema)
      titulo = if fase == 0 then " Gerenciar Turmas Pendentes " else " Lista de Turmas Oficiais "
      rodape = if fase == 0 
               then "[Del] Remover | [Enter] Editar | [Esc] Voltar" 
               else "[Esc] Voltar"
  in templateUI titulo $
    let desenhaLinha selecionado turma =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              codT = unCodigo $ T.getCodigoTurma turma
              codD = unCodigo $ T.getDisciplinaTurma turma
              horario = T.getHorarioTurma turma
              sala = T.getSalaTurma turma
              info = "T" ++ codT ++ " | " ++ codD ++ " | " ++ horario ++ " | Sala " ++ sala
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (st ^. listaMenuTurmas) $ L.renderList desenhaLinha True (st ^. listaMenuTurmas),
            str " ",
            hCenter $ str rodape
          ]

drawConflitos :: AppState -> Widget Name
drawConflitos st =
    templateUI " Relatório de Conflitos (Fase 0) " $
    let desenhaLinha selecionado (t1, t2) =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              info = "Conflito: " ++ unCodigo (T.getCodigoTurma t1) ++ " e " ++ unCodigo (T.getCodigoTurma t2) ++ 
                     " (" ++ T.getSalaTurma t1 ++ " | " ++ T.getHorarioTurma t1 ++ ")"
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (st ^. listaConflitos) $ L.renderList desenhaLinha True (st ^. listaConflitos),
            str " ",
            hCenter $ str "Edite as turmas em 'Gerenciar Turmas' para resolver."
          ]

drawListaSolicitacoes :: AppState -> Widget Name
drawListaSolicitacoes s =
  templateUI " Matrículas/Rematrículas " $
    let sis = s ^. sistema
        desenhaLinha selecionado (idA, idT) =
          let estilo = if selecionado then withAttr L.listSelectedAttr else id
              nomeA = case M.lookup idA (_alunos sis) of
                Just a -> unNome (A.getNomeAluno a)
                Nothing -> "Aluno " ++ show (unMatricula idA)
              info = nomeA ++ " -> Turma " ++ unCodigo idT
           in estilo $ str info
     in vBox
          [ vLimit 15 $ checkEmpty (s ^. listaMenuSolicitacoes) $ L.renderList desenhaLinha True (s ^. listaMenuSolicitacoes),
            str " ",
            hCenter $ str "[Esc] Voltar"
          ]

drawRelatorio :: AppState -> Widget Name
drawRelatorio st =
  templateUI " Relatório " $
    vBox
      [ vLimit 20 $ viewport ScrollRelatorio Vertical (str (st ^. textoRelatorio)),
        str " ",
        hCenter $ str "[Esc] Voltar | [Setas/PgUp/PgDn] Rolar"
      ]

drawConfirmacao :: Widget Name
drawConfirmacao = center $ withAttr (attrName "border") $ borderWithLabel (str " Confirmação ") $ padAll 2 $
    vBox [hCenter $ str "Encerrar esta fase e processar matrículas?", str " ", hCenter $ str "[S]im | [N]ão"]

drawConfirmacaoFimSemestre :: Widget Name
drawConfirmacaoFimSemestre = center $ withAttr (attrName "border") $ borderWithLabel (str " Fim de Semestre ") $ padAll 2 $
    vBox [hCenter $ str "Encerrar semestre e limpar turmas?", str " ", hCenter $ str "[S]im | [N]ão"]

drawLogo :: Widget Name
drawLogo =
  padTop (Pad 2) $
    withAttr (attrName "logo") $
      vBox
        [ hCenter $ str "__  __ _      _   ___ _             ",
          hCenter $ str " |  \\/  (_)_ _ (_) / __(_)__ _ __ _ __ _ ",
          hCenter $ str " | |\\/| | | ' \\| | \\__ \\ / _` / _` / _` |",
          hCenter $ str " |_|  |_|_|_||_|_| |___/_\\__, \\__,_\\__,_|",
          hCenter $ str "                       |___/           "
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