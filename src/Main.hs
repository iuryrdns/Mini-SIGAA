module Main where

import Models.Aluno ( Aluno(..) )
import Models.Disciplina ( Disciplina )
import Models.Professor ( Professor(..) )
import System.IO (hFlush, stdout)

main :: IO()

main = do
    putStrLn "__Seja bem-vindo ao Mini-SIGAA__\n"

    putStr "Pressione Enter para continuar..."
    hFlush stdout
    _ <- getLine
    sistema [] [] []
    
sistema :: [Aluno] -> [Professor] -> [Disciplina] -> IO ()
sistema alunos profs disciplinas = do
    putStrLn "\n--- MENU PRINCIPAL ---"
    putStrLn "1. Cadastrar Professor"
    putStrLn "2. Cadastrar Aluno"
    putStrLn "3. Ver Alunos Cadastrados"
    putStrLn "4. Ver Professores Cadastrados"
    putStrLn "0. Sair"
    
    putStr "Escolha uma opção: "
    hFlush stdout
    
    opcao <- getLine
    
    case opcao of
        "1" -> do
            putStr "Nome do Professor: "
            hFlush stdout
            nome <- getLine
            putStr "Departamento: "
            hFlush stdout
            depProf <- getLine
            putStr "Formação: "
            hFlush stdout
            formacao <- getLine
            let novoProf = Professor {
                nomeProf = nome,
                departamento = depProf,
                formacaoProf = formacao
            }
            sistema alunos (profs ++ [novoProf]) disciplinas
            
        "2" -> do
            putStr "Nome do Aluno: "
            hFlush stdout
            nome <- getLine
            
            putStr "Matricula: "
            hFlush stdout
            matriculaAluno <- getLine
            
            putStr "Curso: "
            hFlush stdout
            cursoAluno <- getLine
            
            putStr "CRA: "
            hFlush stdout
            craAluno <- getLine
            let novoAluno = Aluno {
                nomeAluno = nome,
                matricula = read matriculaAluno,
                curso = cursoAluno,
                cra = read craAluno,
                disciplinas = []
            }
            sistema (alunos ++ [novoAluno]) profs disciplinas
            
        "3" -> do
            putStrLn "\n--- Lista de Alunos ---"
            print alunos 
            sistema alunos profs disciplinas

        "4" -> do
            putStrLn "\n--- Lista de Professores ---"
            print profs
            sistema alunos profs disciplinas

        "0" -> putStrLn "Saindo..."
        
        _   -> do
            putStrLn "Opção inválida!"
            sistema alunos profs disciplinas
