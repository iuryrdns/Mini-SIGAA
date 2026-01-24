{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Menu.Eventos.FaseMatricula
Description : Lógica de controle para o período de matrículas.

Este módulo gerencia a 'Fase 1' do sistema. Ele permite a criação de 
solicitações de matrícula, visualização das solicitações existentes e 
a transição definitiva para a fase de lançamento de notas.
-}

module Menu.Eventos.FaseMatricula (handleEscFaseMatricula, handleEnterFaseMatricula) where

-- Bibliotecas Externas
import Brick
import qualified Brick.Widgets.List as L
import Brick.Focus (focusRing, focusGetCurrent)
import Lens.Micro ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as Vec

-- Módulos Internos
import Menu.Tipos
import Menu.Eventos.Util (finalizarCadastro, extrairSolicitacao)
import Sistema
import Utils.Database (salvarSistema)

-------------------------------------------------------------------------------
-- Tratamento de Teclas
-------------------------------------------------------------------------------

-- | Gerencia o acionamento da tecla 'ESC' na Fase de Matrícula.
-- 
-- 1. Se estiver na tela principal de matrículas, encerra o programa.
-- 2. Se estiver em sub-telas (Cadastro ou Listagem), retorna à tela de Matrículas
--    limpando o estado visual.
handleEscFaseMatricula :: AppState -> EventM Name AppState ()
handleEscFaseMatricula st = case st^.telaAtiva of
    -- No menu principal da Fase Matrícula, ESC fecha o programa
    TelaMatriculas -> halt
        
    -- Qualquer outra tela desta fase volta para a tela de Matrículas
    _ -> modify $ \s -> s 
        { _telaAtiva = TelaMatriculas
        , _mensagemErro = Nothing 
        , _foco = focusRing [] 
        }

-- | Gerencia o acionamento da tecla 'Enter' na Fase de Matrícula.
-- 
-- Roteia o fluxo de execução conforme a tela ativa:
-- * 'TelaMatriculas': Processa a opção selecionada na lista.
-- * 'TelaCadSolicitacao': Inicia a validação e persistência dos dados coletados.
handleEnterFaseMatricula :: AppState -> EventM Name AppState ()
handleEnterFaseMatricula st = case st^.telaAtiva of
    TelaMatriculas -> case focusGetCurrent (st^.foco) of
        Nothing -> handleMenuSelectionFaseMatricula st
        Just _  -> return () 
    
    -- Finalização do formulário de matrícula
    TelaCadSolicitacao -> finalizarCadastro extrairSolicitacao [EditMatAluno, EditMatTurma]
    
    _ -> return ()

-------------------------------------------------------------------------------
-- Lógica de Seleção de Menu
-------------------------------------------------------------------------------

-- | Roteia a seleção do menu de Matrículas para a ação correspondente.
handleMenuSelectionFaseMatricula :: AppState -> EventM Name AppState ()
handleMenuSelectionFaseMatricula st = case L.listSelectedElement (st^.listaMenu) of
    
    -- Inicia formulário de nova matrícula
    Just (_, "Cadastrar Matrícula") ->
        modify $ \s -> s { _telaAtiva = TelaCadSolicitacao, _foco = focusRing [EditMatAluno, EditMatTurma] }

    -- Prepara a lista de solicitações para visualização
    Just (_, "Mostrar Solicitações") -> do
        let todasSols = _solicitacoes (st^.sistema)
        modify $ \s -> s 
            { _telaAtiva = TelaListaSolicitacoes
            , _listaMenuSolicitacoes = L.list ListaSolicitacoes (Vec.fromList todasSols) 1 
            }

    -- Transição de Fase: Processamento Real das Matrículas
    Just (_, "Encerrar Período de Matrículas") -> do
        -- 1. CHAMA A LÓGICA DE NEGÓCIO: Processa CRA, Vagas e troca para Fase 2
        let sisProcessado = efetivarMatriculas (st^.sistema) 
        
        -- 2. Persiste a mudança no arquivo JSON
        liftIO $ salvarSistema sisProcessado
        
        -- 3. Configura o novo menu da fase de notas
        let menuFaseNotas = Vec.fromList 
                [ "Inserir Nota"
                , "Consultar Notas"
                , "Finalizar Semestre"
                , "Sair"
                ]
        
        -- 4. Atualiza o estado da interface para a nova fase
        modify $ \s -> s 
            { _sistema = sisProcessado
            , _telaAtiva = TelaMenuNotas
            , _listaMenu = L.list MenuPrincipal menuFaseNotas 1
            , _mensagemErro = Just "Matrículas encerradas! Iniciando Lançamento de Notas."
            , _foco = focusRing [] 
            }

    Just (_, "Sair") -> halt
    _ -> return ()