{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Menu.Eventos.FaseCadastro
Description : Lógica de controle para a fase inicial de cadastros.

Este módulo define o comportamento da interface durante a 'Fase 0' (Cadastro).
Ele gerencia a navegação entre formulários de Alunos, Professores, Disciplinas 
e Turmas, além de controlar a transição para a fase de Matrículas.
-}

module Menu.Eventos.FaseCadastro (handleEscFaseCadastro, handleEnterFaseCadastro) where

-- Bibliotecas Externas
import Brick
import qualified Brick.Widgets.List as L
import Brick.Focus (focusRing, focusGetCurrent)
import Lens.Micro ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Vector as Vec
import Data.List (sortBy)
import Data.Ord (comparing)

-- Módulos Internos do Projeto
import Menu.Tipos
import qualified Models.Disciplina as D (getPeriodoDisciplina, getNomeDisciplina)
import qualified Models.Professor as B (getDepartamentoProfessor, getNomeProfessor) 
import Menu.Eventos.Util
    ( finalizarCadastro
    , extrairAluno
    , extrairProfessor
    , extrairDisciplina
    , extrairTurma
    )
import Sistema
import Utils.Database (salvarSistema)

-------------------------------------------------------------------------------
-- Tratamento de Teclas
-------------------------------------------------------------------------------

-- | Gerencia o acionamento da tecla 'ESC' na Fase de Cadastro.
-- 
-- Comportamento de navegação:
-- 1. No 'TelaMenu', encerra o aplicativo ('halt').
-- 2. Em telas de formulário ou listagem, limpa mensagens de erro,
--    reseta o foco e retorna ao Menu Principal.
handleEscFaseCadastro :: AppState -> EventM Name AppState ()
handleEscFaseCadastro st = case st^.telaAtiva of
    -- No menu principal da Fase Cadastro, ESC fecha o programa
    TelaMenu -> halt
    
    -- Qualquer outra tela de cadastro ou lista volta para o Menu Principal
    _ -> modify $ \s -> s 
        { _telaAtiva = TelaMenu
        , _mensagemErro = Nothing 
        , _foco = focusRing [] 
        }

-- | Gerencia o acionamento da tecla 'Enter' na Fase de Cadastro.
-- 
-- Roteia o fluxo de execução conforme a tela ativa:
-- * 'TelaMenu': Processa a opção selecionada na lista.
-- * 'TelaCad...': Inicia a validação e persistência dos dados coletados.
handleEnterFaseCadastro :: AppState -> EventM Name AppState ()
handleEnterFaseCadastro st = case st^.telaAtiva of
    TelaMenu -> case focusGetCurrent (st^.foco) of
        Nothing -> handleMenuSelectionFaseCadastro st
        Just _  -> return ()

    -- Finalização de formulários usando a função genérica de extração
    TelaCadAluno -> finalizarCadastro extrairAluno [EditNomeAluno, EditMatricula, EditCurso, EditCRA]
    TelaCadProfessor -> finalizarCadastro extrairProfessor [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao]
    TelaCadDisciplina -> finalizarCadastro extrairDisciplina [EditNomeDisciplina, EditCodigoDisciplina, EditPreRequisitosDisciplina, EditPeriodoDisciplina]
    TelaCadTurma -> finalizarCadastro extrairTurma [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorarioTurma, EditMaxAlunosTurma]
    
    _ -> return ()

-------------------------------------------------------------------------------
-- Lógica de Seleção de Menu
-------------------------------------------------------------------------------

-- | Trata a lógica de seleção do menu principal da Fase de Cadastro.
-- Esta função é responsável por instanciar as listas de visualização ou
-- preparar o 'FocusRing' para a entrada de dados em novos cadastros.
handleMenuSelectionFaseCadastro :: AppState -> EventM Name AppState ()
handleMenuSelectionFaseCadastro st = case L.listSelectedElement (st^.listaMenu) of
    
    -- Navegação para Formulários (Define a ordem do TAB no FocusRing)
    Just (_, "Cadastrar Aluno") -> 
        modify $ \s -> s { _telaAtiva = TelaCadAluno, _foco = focusRing [EditNomeAluno, EditMatricula, EditCurso, EditCRA] }
    
    Just (_, "Cadastrar Professor") -> 
        modify $ \s -> s { _telaAtiva = TelaCadProfessor, _foco = focusRing [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao] }

    Just (_, "Cadastrar Turma") -> 
        modify $ \s -> s { _telaAtiva = TelaCadTurma, _foco = focusRing [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorarioTurma, EditMaxAlunosTurma] }

    Just (_, "Cadastrar Disciplina") -> 
        modify $ \s -> s { _telaAtiva = TelaCadDisciplina, _foco = focusRing [EditCodigoDisciplina, EditNomeDisciplina, EditPreRequisitosDisciplina, EditPeriodoDisciplina] }
    
    -- Navegação para Listagens (Converte Maps do sistema em Vectors para o Widget de Lista)
    Just (_, "Listar Alunos") -> do
        let todosAlunos = M.elems (_alunos (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaAlunos, _listaMenuAlunos = L.list ListaAlunos (Vec.fromList todosAlunos) 1 }
    
    Just (_, "Listar Professores") -> do
        let todosProfs = M.elems (_professores (st^.sistema))
            ordenados = sortBy (comparing (\p -> (B.getDepartamentoProfessor p, B.getNomeProfessor p))) todosProfs
            
        modify $ \s -> s 
            { _telaAtiva = TelaListaProfessores
            , _listaMenuProfessores = L.list ListaProfessores (Vec.fromList ordenados) 1 
            }

    Just (_, "Listar Disciplinas") -> do
        let todasDisciplinas = M.elems (_disciplinas (st^.sistema))
            ordenadas = sortBy (comparing (\d -> (D.getPeriodoDisciplina d, D.getNomeDisciplina d))) todasDisciplinas
        
        modify $ \s -> s { _telaAtiva = TelaListaDisciplinas, _listaMenuDisciplinas = L.list ListaDisciplinas (Vec.fromList ordenadas) 1 }
    
    Just (_, "Listar Turmas") -> 
        modify $ \s -> s { _telaAtiva = TelaAgenda }

    -- Transição de Fase: Cadastro -> Matrícula
    Just (_, "Período de Matrículas") -> do
        case abrirPeriodoMatriculas (st^.sistema) of
            Left erro -> modify $ \s -> s { _mensagemErro = Just erro }
            Right novoSistema -> do
                liftIO $ salvarSistema novoSistema
                let novasOpcoes = Vec.fromList ["Cadastrar Matrícula", "Mostrar Solicitações", "Encerrar Período de Matrículas", "Sair"]
                modify $ \s -> s 
                    { _sistema = novoSistema
                    , _telaAtiva = TelaMatriculas 
                    , _listaMenu = L.list MenuPrincipal novasOpcoes 1
                    , _foco = focusRing [] 
                    , _mensagemErro = Nothing 
                    }
    
    Just (_, "Sair") -> halt
    _ -> return ()