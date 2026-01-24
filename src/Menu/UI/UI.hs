{-|
Module      : Menu.UI.UI
Description : Orquestrador principal da Interface de Usuário (UI).

Este módulo define a função 'drawUI', que atua como o roteador visual do sistema.
Ele decide qual componente renderizar com base na 'telaAtiva' do 'AppState',
unificando elementos comuns (logo, feedback) com telas específicas.
-}

module Menu.UI.UI (drawUI) where

-- Bibliotecas Externas
import Brick
import Brick.Widgets.Center (center)
import Lens.Micro ((^.))

-- Módulos de Tipos e Estado
import Menu.Tipos

-- Submódulos de UI (Divisão de Responsabilidades)
import Menu.UI.Common
import Menu.UI.Listas
import Menu.UI.Agenda

-------------------------------------------------------------------------------
-- Renderização Principal
-------------------------------------------------------------------------------

-- | Função principal de desenho exigida pelo Brick.
-- Envolve a interface ativa e a mensagem de feedback em uma caixa vertical centralizada.
drawUI :: AppState -> [Widget Name]
drawUI s = [center $ vBox [ui, drawFeedback s]]
  where
    -- Roteamento de telas baseado no estado
    ui = case s^.telaAtiva of
        -- Menus Genéricos
        TelaMenu             -> drawGenericMenu s " Menu Principal " "[Enter] Selecionar | [Esc] Sair"
        TelaMatriculas       -> drawGenericMenu s " Período de Matrículas (Fase 1) " "[Enter] Selecionar | [Esc] Sair do Programa"
        TelaMenuNotas         -> drawGenericMenu s " Lançamento de Notas (Fase 2) " "[Enter] Selecionar | [Esc] Sair do Programa"
        
        -- Telas de Cadastro (Formulários)
        TelaCadAluno         -> templateUI " Cadastro de Aluno " $ drawForm camposAluno s
        TelaCadProfessor     -> templateUI " Cadastro de Professor " $ drawForm camposProfessor s
        TelaCadDisciplina    -> templateUI " Cadastro de Disciplina " $ drawForm camposDisciplina s
        TelaCadTurma         -> templateUI " Cadastro de Turma " $ drawForm camposTurma s
        TelaCadSolicitacao -> templateUI " Solicitar Matrícula " $ drawForm camposSolicitacao s
        TelaInserirNotas       -> templateUI " Lançamento de Notas " $ drawForm camposNotas s
        TelaConsultarNotas     -> templateUI " Consulta de Desempenho " $ drawForm camposConsulta s

        -- Telas de Listagem e Visualização
        TelaListaAlunos      -> drawListaAlunos s
        TelaListaProfessores -> drawListaProfessores s
        TelaListaDisciplinas  -> drawListaDisciplinas s
        TelaAgenda           -> drawAgenda s
        TelaListaSolicitacoes -> drawListaSolicitacoes s
        TelaExibirNotasAluno  -> drawTabelaNotas s
        TelaListaResultados   -> drawListaResultados s

-------------------------------------------------------------------------------
-- Definição dos Campos de Formulários (Configuração)
-------------------------------------------------------------------------------

    camposAluno = 
        [ ("Nome", EditNomeAluno), ("Matrícula", EditMatricula)
        , ("Curso", EditCurso), ("CRA", EditCRA) ]

    camposProfessor = 
        [ ("Matrícula", EditMatriculaProfessor), ("Nome", EditNomeProfessor)
        , ("Departamento", EditDepto), ("Formação", EditFormacao) ]

    camposDisciplina = 
        [ ("Código", EditCodigoDisciplina), ("Nome", EditNomeDisciplina)
        , ("Pré-Requisitos", EditPreRequisitosDisciplina), ("Período", EditPeriodoDisciplina)]

    camposTurma = 
        [ ("Cod. Turma", EditCodTurma), ("ID Professor", EditProfTurma)
        , ("Cod. Disciplina", EditDiscTurma), ("Horário", EditHorarioTurma)
        , ("Qtd Max Alunos", EditMaxAlunosTurma), ("Sala", EditSalaTurma) ]

    camposSolicitacao = 
        [ ("Matrícula Aluno", EditMatAluno), ("Código da Turma", EditMatTurma) ]

    camposNotas = 
        [ ("Matrícula Aluno", EditMatriculaNota), ("Código da Turma", EditTurmaNota), ("Nota 1", EditNota1)
        , ("Nota 2", EditNota2), ("Nota 3", EditNota3) ]

    camposConsulta = 
        [ ("Matrícula Aluno", EditConsultaNotaMatricula) ]