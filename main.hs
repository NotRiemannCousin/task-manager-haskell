module Main where
 import Tipos
 import Funcoes
 import System.IO
 
 main :: IO ()
 main = do
 -- menu de ações --
    putStr "- O que gostaria de realizar?:\n "
    putStr "  1  > adicionar nova tarefa\n"
    putStr "   2  > remover uma tarefa\n"
    putStr "   3  > marcar uma tarefa como concluida\n"
    putStr "   4  > listar tarefas por categoria\n"
    putStr "   5  > listar tarefas por prioridade\n"
    putStr "   6  > ordenar tarefas por prioridade\n"
    putStr "   7  > filtrar tarefas por status\n"
    putStr "   8  > buscar por palavra chave\n"
    putStr "   9  > calcular dias restantes\n"
    putStr "  10  > filtrar tarefa por tag\n"
    putStr "  11  > nuvem de tags\n"
    putStr "- Digite o numero da ação: "
 -- menu de ações --
    
 -- resposta --
    input <- getLine
 -- resposta --

 -- lê a resposta --
    let result = read input :: Int
    print result
 -- lê a resposta --

