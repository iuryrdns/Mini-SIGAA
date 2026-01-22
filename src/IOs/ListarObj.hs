module IOs.ListarObj where

import Sistema (Sistema(..))
import Models.Aluno (getNomeAluno)
import qualified Data.Map as Map
import Models.Turma (getDisciplinaTurma, getProfessorTurma, getHorarioTurma, getAlunosTurma, getCapacidadeTurma)
import Models.Types (unMatricula, unNome, unCodigo)

listarAlunoIO :: Sistema -> IO ()
listarAlunoIO sistema = do 
    let mapaAlunos = _alunos sistema 
    if Map.null mapaAlunos
        then putStrLn "Nenhum aluno cadastrado!"
        else do
            let listaAlunos = Map.toList mapaAlunos
            mapM_ (\(matricula, aluno) -> do
                let nome = unNome (getNomeAluno aluno)
                putStrLn $ show (unMatricula matricula) ++ " - " ++ nome
                ) listaAlunos
    putStrLn "\nPressione Enter para continuar..."
    _ <- getLine
    return ()

listarTurmasIO :: Sistema -> IO ()
listarTurmasIO sistema = do
    let mapaTurmas = _turmas sistema
    if Map.null mapaTurmas
        then putStrLn "Não há turmas cadastradas"
        else do
            let turmas = Map.toList mapaTurmas
            mapM_ (\(codigo, turma) -> do
                let disciplina = unCodigo (getDisciplinaTurma turma)
                let horario = show (getHorarioTurma turma)
                let nAlunos = length (getAlunosTurma turma)
                let totalAlunos = getCapacidadeTurma turma
                putStrLn $
                 show codigo ++ " - " ++ disciplina ++ " (" ++ horario ++ ") - " ++ 
                 "Alunos/Capacidade: " ++ show nAlunos ++ "/" ++ show totalAlunos
                ) turmas
    putStrLn "\nPressione Enter para continuar..."
    _ <- getLine
    return ()
