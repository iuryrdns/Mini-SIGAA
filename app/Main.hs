module Main (main) where

import Models.Aluno (criarAluno, getNomeAluno, getCursoAluno)
import Models.Disciplina (criarDisciplina)
import Models.Professor (criarProfessor, getNomeProfessor, getDepartamentoProfessor)
import Sistema (Sistema (_matriculas), abrirPeriodoMatriculas, cadastrarAluno, cadastrarDisciplina, cadastrarProfessor, getAlunos, getFase, getProfessores, realizarMatricula, sistemaVazio, verificarRequisitos, cadastrarTurma, getMatriculasRealizadas)
import System.IO (hFlush, stdout)
import Utils.Database (carregarSistema, salvarSistema)
import Models.Turma (criarTurma)
import qualified Data.Map as Map


main :: IO ()
main = do
  putStrLn "__Seja bem-vindo ao Mini-SIGAA__\n"
  putStrLn "Carregando Sistema..."
  sistemaInicial <- carregarSistema
  putStr "\nPressione Enter para continuar..."
  hFlush stdout
  _ <- getLine

  let fase = getFase sistemaInicial

  case fase of
    0 -> do
      menuPrincipal sistemaInicial
    1 -> do
      menuMatricula sistemaInicial
    2 -> do
      putStrLn "<TODO> fase de correção de conflitos da primeira fase"
    3 -> do
      putStrLn "<TODO> fase de matrículas dos alunos"
    4 -> do
      putStrLn "<TODO> fase de correção de conflitos da terceira fase"
    5 -> do
      putStrLn "<TODO> final das operações mostrando resultados"
    _ -> do
      putStrLn "Caso de erro impossível"

menuPrincipal :: Sistema -> IO ()
menuPrincipal sistema = do
  putStrLn "\n--- Período de Alteração Geral ---"
  putStrLn "1. Cadastrar Professor"
  putStrLn "2. Cadastrar Aluno"
  putStrLn "3. Cadastrar Disciplina"
  putStrLn "4. Cadastrar Turma"
  putStrLn "5. Ver Alunos Cadastrados"
  putStrLn "6. Ver Professores Cadastrados"
  putStrLn "7. Salvar Alterações"
  putStrLn "8. Abrir Periodo de Matriculas"
  putStrLn "0. Sair"

  putStr "Escolha uma opção: "
  hFlush stdout

  opcao <- getLine

  case opcao of
    "1" -> do
      putStr "Nome do Professor: "
      hFlush stdout
      nomeProfessor <- getLine

      putStr "Matricula do Professor: "
      hFlush stdout
      matricula <- getLine

      putStr "Departamento: "
      hFlush stdout
      depProf <- getLine

      putStr "Formação: "
      hFlush stdout
      formacao <- getLine

      let novoProf = criarProfessor (read matricula) nomeProfessor depProf formacao

      case cadastrarProfessor novoProf sistema of
        Left erro -> do
          putStrLn erro
          menuPrincipal sistema
        Right novoSistema -> do
          putStrLn "Professor cadastrado com sucesso!"
          menuPrincipal novoSistema
    "2" -> do
      putStr "Nome do Aluno: "
      hFlush stdout
      nomeAluno <- getLine

      putStr "Matricula: "
      hFlush stdout
      matriculaAluno <- getLine

      putStr "Curso: "
      hFlush stdout
      cursoAluno <- getLine

      putStr "CRA: "
      hFlush stdout
      craAluno <- getLine

      let novoAluno = criarAluno (read matriculaAluno) nomeAluno cursoAluno (read craAluno)

      case cadastrarAluno novoAluno sistema of
        Left erro -> do
          putStrLn erro
          menuPrincipal sistema
        Right novoSistema -> do
          putStrLn "Aluno cadastrado com sucesso!"
          menuPrincipal novoSistema
    "3" -> do
      putStr "Codigo da Disciplina: "
      hFlush stdout
      codigo <- getLine

      putStr "Nome da Disciplina: "
      hFlush stdout
      nomeDisciplina <- getLine

      putStr "Requisitos: "
      hFlush stdout
      entrada <- getLine

      let requisitos = words entrada

      case verificarRequisitos requisitos sistema of
        Left erro -> do
          putStrLn erro
          menuPrincipal sistema
        Right requisitosValidados -> do
          let novaDisciplina = criarDisciplina codigo nomeDisciplina requisitos

          case cadastrarDisciplina novaDisciplina sistema of
            Left erro -> do
              putStrLn erro
              menuPrincipal sistema
            Right novoSistema -> do
              putStrLn "Disciplina cadastrada."
              menuPrincipal novoSistema
    "4" -> do
      putStr "Codigo da Turma: "
      hFlush stdout
      codigoTurma <- getLine

      putStr "Matricula Professor: "
      hFlush stdout
      professorDisciplina <- getLine

      putStr "Codigo da Disciplina: "
      hFlush stdout
      disciplina <- getLine

      putStr "Horario: "
      hFlush stdout
      horario <- getLine

      putStr "Sala: "
      hFlush stdout
      sala <- getLine

      putStr "Quantidade MAX de alunos: "
      hFlush stdout
      qtdAlunos <- getLine

      let novaTurma = criarTurma (read codigoTurma) (read professorDisciplina) disciplina horario sala (read qtdAlunos)

      case cadastrarTurma novaTurma sistema of
        Left erro -> do
          putStrLn erro
          menuPrincipal sistema
        Right novoSistema -> do
          putStrLn "Disciplina cadastrada com sucesso!"
          menuPrincipal novoSistema

    "5" -> do
      let mapaAlunos = getAlunos sistema
      if Map.null mapaAlunos
        then putStr "\nNao ha alunos cadastrados"
        else do
            let alunos = Map.toList mapaAlunos
            
            -- Cabeçalho
            putStrLn "\n========================================================"
            putStrLn "                  LISTA DE ALUNOS"
            putStrLn "========================================================"
            putStrLn ""
            
            -- Títulos das colunas
            putStrLn "  #   | Matrícula | Nome                    | Curso"
            putStrLn " ---  +-----------+-------------------------+-----------"
            
            -- Linhas de dados
            mapM_ (\(pos, (matricula, aluno)) -> do
                let nome = getNomeAluno aluno
                let curso = getCursoAluno aluno
                
                -- Formatação inline sem funções extras
                let num = take 3 (" " ++ show pos ++ ". ")
                let mat = take 10 ("| " ++ show matricula ++ "        ")
                let nom = take 24 ("| " ++ nome ++ repeat ' ')
                let cur = take 12 ("| " ++ curso ++ repeat ' ')
                
                putStrLn $ " " ++ num ++ "  " ++ mat ++ "  " ++ nom ++ "  " ++ cur
                ) (zip [1..] alunos)
            
            -- Total
            putStrLn ""
            putStrLn $ "Total: " ++ show (length alunos) ++ " aluno(s)"

      putStr "\nPressione Enter para continuar..."
      hFlush stdout
      _ <- getLine
          
      menuPrincipal sistema
    "6" -> do
      let mapaProf = getProfessores sistema
      if Map.null mapaProf
        then putStr "\nNao ha professores cadastrados"
        else do
          putStrLn "\n--- Lista de Professores ---\n"
          let professores = Map.toList mapaProf
          mapM_(\(pos, (matricula, professor)) -> do
            let nome = getNomeProfessor professor
            let departamento = getDepartamentoProfessor professor
            putStrLn $ show pos ++ ". "++ show matricula ++ " | " ++ nome ++ " - Dep: " ++ departamento ++ "\n"
            ) (zip[1..] professores)

      putStr "\nPressione Enter para continuar..."
      hFlush stdout
      _ <- getLine
      
      menuPrincipal sistema
    "7" -> do
      putStrLn "Salvando alterações..."
      salvarSistema sistema
      menuPrincipal sistema
    "8" -> do
      case abrirPeriodoMatriculas sistema of
        Left erro -> do
          putStrLn erro
          menuPrincipal sistema
        Right novoSistema -> do
          putStrLn "Periodo de Matriculas Aberto!"
          menuMatricula novoSistema
    "0" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida!"
      menuPrincipal sistema

menuMatricula :: Sistema -> IO ()
menuMatricula sistema = do
  putStrLn "\n--- Período de Matrículas ---"
  putStrLn "1. Matricular aluno em turma"
  putStrLn "2. Ver Relatorio de Matriculas"
  putStrLn "0. Sair para o Menu Principal"
  putStr "Escolha uma opção: "
  hFlush stdout

  opcao <- getLine

  case opcao of
    "1" -> do
      putStr "Matrícula do aluno: "
      hFlush stdout
      matricula <- getLine
      
      putStr "Codigo da Turma: "
      hFlush stdout
      turma <- getLine

      case realizarMatricula (read matricula) (read turma) sistema of
        Left erro -> do
          putStrLn erro
          menuMatricula sistema
        Right novoSistema -> do
          putStrLn "Matricula cadastrada com sucesso"
          salvarSistema novoSistema
          menuMatricula novoSistema
    "2" -> do
      case getMatriculasRealizadas sistema of
        Left erro -> do
          putStrLn erro
          menuMatricula sistema
        Right relatorio -> do
          putStrLn relatorio
          menuMatricula sistema
    "0" -> do
      putStrLn "Saindo..."
      menuPrincipal sistema
    _ -> do
      putStrLn "Opção inválida"
      menuMatricula sistema