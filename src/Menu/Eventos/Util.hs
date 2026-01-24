{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Menu.Eventos.Util
Description : Utilitários de manipulação de formulários e extração de dados.

Este módulo fornece funções genéricas para gerenciar campos de texto (Editores),
validar entradas do usuário e converter strings em tipos de dados do sistema.
É o elo entre a interface visual e a lógica de negócio.
-}

module Menu.Eventos.Util where

-- Bibliotecas Externas
import Brick
import Brick.Widgets.Edit (handleEditorEvent, getEditContents, editor)
import Brick.Focus (focusRing)
import Lens.Micro ((^.), lens)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import qualified Data.Map as M
import Data.Char (isSpace)

-- Módulos do Projeto
import Menu.Tipos
import Sistema
import Models.Aluno
import Models.Professor
import Models.Disciplina
import Models.Turma
import qualified Models.Turma as T
import Models.Types (Matricula, Codigo, Horario(..))
import Utils.Database (salvarSistema)

-------------------------------------------------------------------------------
-- LÓGICA DE FORMULÁRIOS E EDITORES
-------------------------------------------------------------------------------

-- | Direciona eventos de teclado para um editor específico dentro do Map de formulários.
-- Utiliza uma lente (lens) dinâmica para focar no campo identificado pelo 'Name'.
handleEditorGeneric :: AppState -> Name -> BrickEvent Name e -> EventM Name AppState ()
handleEditorGeneric _st n e = 
    let campoLens = lens (\s -> M.findWithDefault (editor n (Just 1) "") n (s^.formularios))
                         (\s ed -> s { _formularios = M.insert n ed (s^.formularios) })
    in zoom campoLens (handleEditorEvent e)

-- | Função de alta ordem que processa a submissão de um formulário.
-- 
-- 1. Tenta extrair os dados usando a função fornecida.
-- 2. Em caso de sucesso: salva o sistema, limpa os campos e exibe sucesso.
-- 3. Em caso de erro: mantém os dados e exibe a mensagem de erro.
finalizarCadastro :: (AppState -> Either String Sistema) -> [Name] -> EventM Name AppState ()
finalizarCadastro extrair camposParaReset = do
    st <- get
    case extrair st of
        Left erro -> modify $ \s -> s { _mensagemErro = Just erro }
        Right novoSistema -> do
            liftIO $ salvarSistema novoSistema
            let novosForms = foldr (\n m -> M.insert n (editor n (Just 1) "") m) (st^.formularios) camposParaReset
            
            let proximaTela = case getFase novoSistema of
                                1 -> TelaMatriculas
                                2 -> TelaMenuNotas   -- <--- Adicionado para a Fase 2
                                _ -> TelaMenu

            modify $ \s -> s 
                { _sistema = novoSistema
                , _telaAtiva = proximaTela
                , _foco = focusRing [] 
                , _formularios = novosForms 
                , _mensagemErro = Just "Operação realizada com sucesso!" 
                }

-------------------------------------------------------------------------------
-- AUXILIARES DE EXTRAÇÃO (TRANSFORMA TEXTO EM DADOS)
-------------------------------------------------------------------------------

-- | Recupera o conteúdo textual de um campo de edição pelo seu nome.
getCampo :: Name -> AppState -> String
getCampo n st = case M.lookup n (st^.formularios) of
    Just ed -> concat $ getEditContents ed
    Nothing -> ""

-- | Extrai e valida dados para criação de um Aluno.
extrairAluno :: AppState -> Either String Sistema
extrairAluno st = do
    let n = getCampo EditNomeAluno st
    let c = getCampo EditCurso st
    
    -- 1. Validação de campos obrigatórios
    if null n || null c
       then Left "Nome e Curso são obrigatórios"
       else do
           -- 2. Parsing (Conversão de String para número)
           -- Como é um type alias, o readMaybe já devolve o tipo correto (Int/Double)
           m   <- maybe (Left "Matrícula inválida") Right (readMaybe $ getCampo EditMatricula st)
           cra <- maybe (Left "CRA inválido") Right (readMaybe $ getCampo EditCRA st)
           
           -- 3. Cadastro
           cadastrarAluno (criarAluno m n c cra) (st^.sistema)

-- | Extrai e valida dados para criação de um Professor.
extrairProfessor :: AppState -> Either String Sistema
extrairProfessor st = do
    let txtMatricula = getCampo EditMatriculaProfessor st
    let nome         = getCampo EditNomeProfessor st
    let depto        = getCampo EditDepto st
    let formacao     = getCampo EditFormacao st
    
    -- 1. Validação de campos obrigatórios
    if null nome || null txtMatricula || null depto || null formacao
       then Left "Todos os campos do professor são obrigatórios"
       else do
           -- 2. Parsing da Matrícula
           -- Como é um type alias de Int, o readMaybe já devolve o tipo correto.
           valMat <- maybe (Left "Matrícula do professor deve ser um número") Right (readMaybe txtMatricula)
           
           -- 3. Criação e Cadastro
           -- Passamos valMat direto porque Matricula == Int
           let prof = criarProfessor valMat nome depto formacao
           cadastrarProfessor prof (st^.sistema)

-- | Extrai e valida dados para criação de uma Disciplina.
extrairDisciplina :: AppState -> Either String Sistema
extrairDisciplina st = do
    let n = getCampo EditNomeDisciplina st
    let c = getCampo EditCodigoDisciplina st
    let preReqsTxt = getCampo EditPreRequisitosDisciplina st
    let periodoTxt = getCampo EditPeriodoDisciplina st 
    
    -- 1. Validação de campos obrigatórios (Strings)
    if null n || null c || null periodoTxt
       then Left "Nome, Código e Período são obrigatórios"
       else do
           -- 2. Conversão de tipos (Parsing)
           p <- maybe (Left "O Período deve ser um número inteiro") Right (readMaybe periodoTxt)
           
           -- 3. Processamento de pré-requisitos (converte String em [String])
           let preReqs = splitComma preReqsTxt
           
           -- 4. Criação e tentativa de cadastro
    
           let novaDisc = criarDisciplina c n preReqs p
           cadastrarDisciplina novaDisc (st^.sistema)


-- | Extrai e valida dados para criação de uma Turma.
extrairTurma :: AppState -> Either String Sistema
extrairTurma st = do
    -- 1. Extração e Parsing básico (Tipagem)
    cod  <- maybe (Left "Cód. de Turma inválido (deve ser número)") Right 
                  (readMaybe (getCampo EditCodTurma st) :: Maybe Int)
    prof <- maybe (Left "Matrícula do Prof. inválida") Right 
                  (readMaybe (getCampo EditProfTurma st) :: Maybe Int)
    maxA <- maybe (Left "Qtd. Máxima inválida") Right 
                  (readMaybe (getCampo EditMaxAlunosTurma st) :: Maybe Int)
    
    let discStr = getCampo EditDiscTurma st
    let horStr  = getCampo EditHorarioTurma st
    let salaStr = getCampo EditSalaTurma st

    -- 2. Validações de campo vazio
    if null discStr || null horStr || null salaStr
       then Left "Campos de Disciplina, Horário e Sala são obrigatórios" 
       else do
           -- 3. Processamento da String de Horários
           -- Ex: "2M23, 4M23" -> ["2M23", "4M23"]
           let partes = words [if c == ',' then ' ' else c | c <- horStr]
           let listaHorarios = map Horario partes
           
           -- 4. Validação básica de formato (opcional)
           -- Verifica se ao menos um horário foi reconhecido
           if null listaHorarios 
              then Left "Nenhum horário válido informado. Use o formato: 2M23, 4M45"
              else do
                  -- 5. Criação e Cadastro
                  let novaTurma = criarTurma cod prof discStr listaHorarios maxA salaStr
                  cadastrarTurma novaTurma (st^.sistema)

-- | Extrai dados para uma Solicitação de Matrícula.
extrairSolicitacao :: AppState -> Either String Sistema
extrairSolicitacao st = do
    idA <- maybe (Left "ID Aluno inválido") Right (readMaybe (getCampo EditMatAluno st) :: Maybe Int)
    idT <- maybe (Left "ID Turma inválido") Right (readMaybe (getCampo EditMatTurma st) :: Maybe Int)
    cadastrarSolicitacao idA idT (st^.sistema)

-- | Extrai dados para lançamento de notas.
extrairLancamentoNota :: AppState -> Either String Sistema
extrairLancamentoNota st = do
    idA  <- maybe (Left "Matrícula inválida") Right (readMaybe (getCampo EditMatriculaNota st) :: Maybe Int)
    idT  <- maybe (Left "Cód. Turma inválido") Right (readMaybe (getCampo EditTurmaNota st) :: Maybe Int)
    
    -- Captura Notas
    val1 <- maybe (Left "Nota 1 inválida") Right (readMaybe (getCampo EditNota1 st) :: Maybe Double)
    val2 <- maybe (Left "Nota 2 inválida") Right (readMaybe (getCampo EditNota2 st) :: Maybe Double)
    val3 <- maybe (Left "Nota 3 inválida") Right (readMaybe (getCampo EditNota3 st) :: Maybe Double)
    
    -- Placeholder para futura lógica de lançamento
    lancarNotas idA idT val1 val2 val3 (st^.sistema)

-- | Processa a consulta de notas e retorna uma string formatada para exibição.
exibirResultadoConsulta :: AppState -> Either String String
exibirResultadoConsulta st = do
    matTxt <- Right (getCampo EditConsultaMatricula st)
    
    if null matTxt 
       then Left "Digite uma matrícula"
       else return $ "Aluno " ++ matTxt ++ " - Média: 8.5 [APROVADO (Simulação)]"

-------------------------------------------------------------------------------
-- UTILITÁRIOS DE PARSING E STRING
-------------------------------------------------------------------------------

-- | Divide uma string por vírgulas e remove espaços em branco.
splitComma :: String -> [String]
splitComma = map trim . splitOn ','
  where
    splitOn _ [] = []
    splitOn d s  = let (a, b) = break (== d) s
                   in a : case b of
                            []     -> []
                            (_:xs) -> splitOn d xs
    trim = f . f where f = reverse . dropWhile isSpace