module IOs.ListarObj where

import Sistema (Sistema(..))
import Models.Aluno (getNomeAluno)
import qualified Data.Map as Map
import Models.Turma (getDisciplinaTurma, getProfessorTurma, getHorarioTurma, getAlunosTurma, getCapacidadeTurma)

listarAlunoIO :: Sistema -> IO ()
listarAlunoIO sistema = do 
    let mapaAlunos = _alunos sistema 
    if Map.null mapaAlunos
        then putStrLn "Nenhum aluno cadastrado!"
        else do
            let listaAlunos = Map.toList mapaAlunos
            mapM_ (\(matricula, aluno) -> do
                let nome = getNomeAluno aluno
                putStrLn $ show matricula ++ " - " ++ nome
                ) listaAlunos
    putStrLn "\nPressione Enter para continuar..."
    _ <- getLine
    return ()