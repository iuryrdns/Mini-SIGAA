module Main (main) where

import System.IO (hFlush, stdout)
import Utils.Database (carregarSistema, salvarSistema)
import Sistema (Sistema, verificarRequisitos, cadastrarDisciplina, cadastrarAluno, sistemaVazio, cadastrarProfessor, getAlunos, getProfessores, abrirPeriodoMatriculas)
import Models.Disciplina (criarDisciplina)
import Models.Aluno (criarAluno)
import Models.Professor (criarProfessor)

main :: IO ()
main = do
  putStrLn "__Seja bem-vindo ao Mini-SIGAA__\n"
  putStrLn "Carregando Sistema..."
  sistemaInicial <- carregarSistema
  putStr "\nPressione Enter para continuar..."
  hFlush stdout
  _ <- getLine
  
  menuPrincipal sistemaInicial


menuPrincipal :: Sistema -> IO ()
menuPrincipal sistema = do
  putStrLn "\n--- MENU PRINCIPAL ---"
  putStrLn "1. Cadastrar Professor"
  putStrLn "2. Cadastrar Aluno"
  putStrLn "3. Cadastrar Disciplina"
  putStrLn "4. Cadastrar Turma"
  putStrLn "5. Ver Alunos Cadastrados"
  putStrLn "6. Ver Professores Cadastrados"
  putStrLn "7. Salvar Sistema"
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
      print ""
                 
    "5" -> do
      putStrLn "\n--- Lista de Alunos ---"
      print (getAlunos sistema)
      menuPrincipal sistema
      
    "6" -> do
      putStrLn "\n--- Lista de Professores ---"
      print (getProfessores sistema)
      menuPrincipal sistema
      
    "7" -> do
      putStrLn "Salvando Sistema..."
      salvarSistema sistema
      menuPrincipal sistema

    "8" -> do
      case abrirPeriodoMatriculas sistema of
        Left erro -> do
          putStrLn erro 
          menuPrincipal sistema 
        Right novoSistema -> do
          putStrLn "Periodo de Matriculas Aberto!"
          menuPrincipal novoSistema

    "0" -> putStrLn "Saindo..."
    
    _ -> do
      putStrLn "Opção inválida!"
      menuPrincipal sistema
