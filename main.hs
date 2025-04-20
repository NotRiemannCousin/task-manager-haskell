module Main where

import Data.Time.Calendar
import Funcoes
import System.IO
import Tipos
import Main (tarefas)
import Funcoes (
    adicionarTarefa,
    removerTarefa,
    marcarConcluida,
    listarPorCategoria,
    listarPorPrioridade,
    ordenarPorPrioridade,
    filtrarPorStatus,
    buscarPorPalavraChave,
    calcularDiasRestantes,
    filtrarPorTag,
    nuvemDeTags)


carregarDeArquivo :: IO [Tarefa]
carregarDeArquivo = do
    putStrLn "Carregando tarefas..."
    contents <- readFile "tarefas.txt"
    return (read contents :: [Tarefa])


main :: IO ()
main = do
    tarefas <- carregarDeArquivo
    res <- menu tarefas
    writeFile "tarefas.txt" (show res)








menu :: [Tarefa] -> IO [Tarefa]
menu tarefas = do
    -- menu de ações --
    putStr "- O que gostaria de realizar?:\n "
    putStr "   1  > adicionar nova tarefa\n"
    putStr "   2  > remover uma tarefa\n"
    putStr "   3  > marcar uma tarefa como concluida\n"
    putStr "   4  > listar tarefas por categoria\n"
    putStr "   5  > listar tarefas por prioridade\n"
    putStr "   6  > ordenar tarefas por prioridade\n"
    putStr "   7  > filtrar tarefas por status\n"
    putStr "   8  > buscar por palavra chave\n"
    putStr "   9  > verificar atrasos\n"
    putStr "  10  > calcular dias restantes\n"
    putStr "  11  > filtrar tarefa por tag\n"
    putStr "  12  > nuvem de tags\n"
    putStr "  13  > Sair\n"
    putStr "- Digite o numero da ação: "
    -- resposta --
    input <- getLine
    let result = read input :: Int
    -- lê a resposta --
    case result of
        1  -> menu (adicionarTarefaMenu       tarefas)
        2  -> menu (removerTarefaMenu         tarefas)
        3  -> menu (marcarConcluidaMenu       tarefas)
        4  -> menu (listarPorCategoriaMenu    tarefas)
        5  -> menu (listarPorPrioridadeMenu   tarefas)
        6  -> menu (ordenarPorPrioridadeMenu  tarefas)
        7  -> menu (filtrarPorStatusMenu      tarefas)
        8  -> menu (buscarPorPalavraChaveMenu tarefas)
        9  -> menu (verificarAtrasos          tarefas)
        10 -> menu (calcularDiasRestantesMenu tarefas)
        11 -> menu (filtrarPorTagMenu         tarefas)
        12 -> menu (nuvemDeTagsMenu           tarefas)
        13 -> return tarefas
        _ -> do
            putStr "Comando inválido!\n"
            menu tarefas




input :: Read a => String -> IO a
input text = do
    putStr text
    line <- getLine
    case reads line of
        [(x, "")] -> return x
        _         -> do
            putStr "Comando inválido!\n"
            input text



adicionarTarefaMenu :: IO [Tarefa]
adicionarTarefaMenu = do
    id         <- input "Digite o id da tarefa: " :: IO Int
    titulo     <- input "Digite o titulo da tarefa: " :: IO String
    descricao  <- input "Digite a descricao da tarefa: " :: IO String
    categoria  <- input "Digite a categoria da tarefa: " :: IO String
    prioridade <- input "Digite a prioridade da tarefa: " :: IO String
    status     <- input "Digite o status da tarefa: " :: IO String
    prazo      <- input "Digite o prazo da tarefa: " :: IO Day
    tagsStr    <- input "Digite as tags da tarefa (separadas por espaço): " :: IO String
    adicionarTarefa ++ Tarefa id titulo readstatus readprioridade readcategoria prazo (split ' ' tagsStr)

removerTarefaMenu :: Tarefa -> IO [Tarefa]
removerTarefaMenu tarefas = do
    id <- input "Digite o id da tarefa a ser removida: " :: IO Int
    removerTarefa id tarefas

marcarConcluidaMenu :: Tarefa -> IO [Tarefa]
marcarConcluidaMenu tarefas = do
    id <- input "Digite o id da tarefa a ser concluida: " :: IO Int
    marcarConcluida id tarefas


listarPorCategoriaMenu :: Tarefa -> IO [Tarefa]
listarPorCategoriaMenu tarefas = do
    categoria <- input "Digite a categoria da tarefa: " :: IO String
    print $ listarPorCategoria categoria tarefas -- lembrar de imprimir bonitinho dps
    tarefas -- ou o resultado da função?

listarPorPrioridadeMenu :: Tarefa -> IO [Tarefa]
listarPorPrioridadeMenu tarefas = do
    prioridade <- input "Digite a prioridade da tarefa: " :: IO String
    print $ listarPorPrioridade prioridade tarefas -- lembrar de imprimir bonitinho dps
    tarefas -- ou o resultado da função?

ordenarPorPrioridadeMenu :: Tarefa -> IO [Tarefa]
ordenarPorPrioridadeMenu tarefas = do
    putStrLn "Ordenando tarefas..."
    print $ ordenarPorPrioridade tarefas -- lembrar de imprimir bonitinho dps
    tarefas -- ou o resultado da função?

filtrarPorStatusMenu :: Tarefa -> IO [Tarefa]
filtrarPorStatusMenu tarefas = do
    status <- input "Digite o status da tarefa: " :: IO String
    print $ filtrarPorStatus status tarefas -- lembrar de imprimir bonitinho dps
    tarefas -- ou o resultado da função?


buscarPorPalavraChaveMenu :: Tarefa -> IO [Tarefa]
buscarPorPalavraChaveMenu tarefas = do
    palavraChave <- input "Digite a palavra chave: " :: IO String
    print $ buscarPorPalavraChave palavraChave tarefas -- lembrar de imprimir bonitinho dps
    tarefas -- ou o resultado da função?

verificarAtrasosMenu :: Tarefa -> IO [Tarefa]
verificarAtrasosMenu tarefas = do
    putStrLn "Atrasados:"
    print $ verificarAtrasos tarefas $ getCurrentTime . toGregorian . utctDay
    tarefas

calcularDiasRestantesMenu :: Tarefa -> IO [Tarefa]
calcularDiasRestantesMenu tarefas = do
    id <- input "Digite o id da tarefa: " :: IO Int
    dias <- calcularDiasRestantes (find id tarefa) tarefas
    case dias of
        Nothing -> putStr "Essa tarefa não tem prazo"
        _       -> putStr concat $ "Dias restantes: " (show dias)
    tarefas

filtrarPorTagMenu :: Tarefa -> IO [Tarefa]
filtrarPorTagMenu tarefas = do
    tag <- input "Digite a tag a ser procurada: " :: IO Int
    print filtrarPorTag tarefas -- lembrar de imprimir bonitinho dps
    tarefas -- ou o resultado da função?


nuvemDeTagsMenu :: Tarefa -> IO [Tarefa]
nuvemDeTagsMenu tarefas = do
    putStrLn "Tags:"
    print $ nuvemDeTags tarefas -- lembrar de imprimir bonitinho dps
    tarefas
