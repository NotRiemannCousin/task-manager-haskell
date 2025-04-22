module Main where

import Data.Time.Calendar
import Data.Time
import Data.List
import System.IO

import Persistencias
import Funcoes
import Tipos


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    tarefas <- carregarDeArquivo "tarefas.txt"
    res <- menu tarefas
    salvarEmArquivo "tarefas.txt" res



inputString :: String -> IO String
inputString text = do
    putStr text
    line <- getLine
    if line /= ""
       then return line
       else do
        putStr "String não vazia!\n"
        input text

input :: Read a => String -> IO a
input text = do
    putStr text
    line <- getLine
    case reads line of
        [(x, "")] -> return x
        _      -> do
            putStr "Comando inválido!\n"
            input text




menu :: [Tarefa] -> IO [Tarefa]
menu tarefas = do
    -- menu de ações --
    putStrLn $ replicate 60 '\n' -- limpa tudo
    putStrLn "- O que gostaria de realizar?:"
    putStrLn "   1  > adicionar nova tarefa"
    putStrLn "   2  > remover uma tarefa"
    putStrLn "   3  > marcar uma tarefa como concluida"
    putStrLn "   4  > listar tarefas por categoria"
    putStrLn "   5  > listar tarefas por prioridade"
    putStrLn "   6  > ordenar tarefas por prioridade"
    putStrLn "   7  > filtrar tarefas por status"
    putStrLn "   8  > buscar por palavra chave"
    putStrLn "   9  > verificar atrasos"
    putStrLn "  10  > calcular dias restantes"
    putStrLn "  11  > filtrar tarefa por tag"
    putStrLn "  12  > nuvem de tags"
    putStrLn "  13  > Sair"
    putStrLn "- Digite o numero da ação: "
    -- o getChar segura o terminal até uma linha ser precionada, dps vai para o menu
    let nextIteration ts = getChar >> menu ts
    -- resposta --
    input <- getLine
    case reads input of
        [(1  :: Int, "")] -> adicionarTarefaMenu tarefas       >>= nextIteration
        [(2  :: Int, "")] -> removerTarefaMenu tarefas         >>= nextIteration
        [(3  :: Int, "")] -> marcarConcluidaMenu tarefas       >>= nextIteration
        [(4  :: Int, "")] -> listarPorCategoriaMenu tarefas    >>= nextIteration
        [(5  :: Int, "")] -> listarPorPrioridadeMenu tarefas   >>= nextIteration
        [(6  :: Int, "")] -> ordenarPorPrioridadeMenu tarefas  >>= nextIteration
        [(7  :: Int, "")] -> filtrarPorStatusMenu tarefas      >>= nextIteration
        [(8  :: Int, "")] -> buscarPorPalavraChaveMenu tarefas >>= nextIteration
        [(9  :: Int, "")] -> verificarAtrasosMenu tarefas      >>= nextIteration
        [(10 :: Int, "")] -> calcularDiasRestantesMenu tarefas >>= nextIteration
        [(11 :: Int, "")] -> filtrarPorTagMenu tarefas         >>= nextIteration
        [(12 :: Int, "")] -> nuvemDeTagsMenu tarefas           >>= nextIteration
        [(13 :: Int, "")] -> return tarefas
        _ -> do
            putStr "Comando inválido!\n"
            getChar
            menu tarefas

formatarData :: Day -> String
formatarData = formatTime defaultTimeLocale "%Y-%m-%d"

tarefaParaString :: Tarefa -> String
tarefaParaString tarefa =
    let linha = "--------=== tarefa " ++ show (idTarefa tarefa) ++ "===--------"
        conteudo = unlines
            [ "descricao: " ++ descricao tarefa
            , "id: " ++ show (idTarefa tarefa)
            , "status: " ++ show (status tarefa)
            , "prioridade: " ++ show (prioridade tarefa)
            , "categoria: " ++ show (categoria tarefa)
            , "prazo: " ++ show (prazo tarefa)
            , "tags: " ++ show (tags tarefa)
            ]
    in unlines [linha, conteudo, linha]

tarefaParaString :: Tarefa -> String
tarefaParaString tarefa =
    let linha = "--------=== tarefa " ++ show (idTarefa tarefa) ++ "===--------"
        prazoStr = maybe "Sem prazo" formatarData (prazo tarefa)
        conteudo = unlines
            [ "descricao: " ++ descricao tarefa
            , "id: " ++ show (idTarefa tarefa)
            , "status: " ++ show (status tarefa)
            , "prioridade: " ++ show (prioridade tarefa)
            , "categoria: " ++ show (categoria tarefa)
            , "prazo: " ++ prazoStr
            , "tags: " ++ show (tags tarefa)
            ]
    in unlines [linha, conteudo, linha]


adicionarTarefaMenu :: [Tarefa] -> IO [Tarefa]
adicionarTarefaMenu tarefas = do
    id         <- input       "Digite o id da tarefa: " :: IO Int
    titulo     <- inputString "Digite o titulo da tarefa: "
    descricao  <- inputString "Digite a descricao da tarefa: "
    categoria  <- input       "Digite a categoria da tarefa: " :: IO Categoria
    prioridade <- input       "Digite a prioridade da tarefa: " :: IO Prioridade
    status     <- input       "Digite o status da tarefa: " :: IO Status

    putStrLn "Digite o prazo da tarefa:"
    prazo <- getLine >>= trataPrazo

    tagsStr    <- inputString "Digite as tags da tarefa (separadas por espaço): "

    let tarefa = Tarefa id titulo status prioridade categoria prazo (words tagsStr)

    case adicionarTarefa tarefa tarefas of
        Left msg -> do
            putStrLn msg
            return tarefas
        Right tarefas -> do
            putStrLn "Tarefa adicionada com sucesso!"
            return tarefas


removerTarefaMenu :: [Tarefa] -> IO [Tarefa]
removerTarefaMenu tarefas = do
    id <- input "Digite o id da tarefa a ser removida: " :: IO Int
    case removerTarefa id tarefas of
        Left msg -> do
            putStrLn msg
            return tarefas
        Right tarefas -> do
            putStrLn "Tarefa removida com sucesso!"
            return tarefas

marcarConcluidaMenu :: [Tarefa] -> IO [Tarefa]
marcarConcluidaMenu tarefas = do
    id <- input "Digite o id da tarefa a ser concluida: " :: IO Int
    let res = marcarConcluida id tarefas
    case res of
        Left msg      -> do
            putStrLn msg
            return tarefas
        Right tarefas -> do
            putStrLn "Tarefa concluida com sucesso!"
            return tarefas


listarPorCategoriaMenu :: [Tarefa] -> IO [Tarefa]
listarPorCategoriaMenu tarefas = do
    categoria <- input "Digite a categoria da tarefa: " :: IO Categoria
    let listaFiltrada = listarPorCategoria categoria tarefas
    putStrLn "\n------------=== Listando Por Categoria ===------------ \n"
    mapM_ (putStrLn.tarefaParaString) listaFiltrada
    putStrLn "------------===  Categorias Listadas   ===------------ \n"
    return tarefas

listarPorPrioridadeMenu :: [Tarefa] -> IO [Tarefa]
listarPorPrioridadeMenu tarefas = do
    prioridade <- input "Digite a prioridade da tarefa: " :: IO Prioridade
    let listaFiltrada = listarPorPrioridade prioridade tarefas
    putStrLn "\n------------=== Listando Por Prioridade ===------------ \n"
    mapM_ (putStrLn.tarefaParaString) listaFiltrada
    putStrLn "------------===  Prioridades Listadas   ===------------ \n"
    return tarefas

ordenarPorPrioridadeMenu :: [Tarefa] -> IO [Tarefa]
ordenarPorPrioridadeMenu tarefas = do
    putStrLn "Ordenando tarefas..."
    let listaOrdenada = ordenarPorPrioridade tarefas
    putStrLn "\n------------=== Ordenando Lista ===------------ \n"
    mapM_ (putStrLn.tarefaParaString) listaOrdenada
    putStrLn "\n------------=== Lista Ordenada ===------------ \n"
    return tarefas


filtrarPorStatusMenu :: [Tarefa] -> IO [Tarefa]
filtrarPorStatusMenu tarefas = do
    status <- input "Digite o status da tarefa: " :: IO Status
    let listaFiltrada = filtrarPorStatus status tarefas
    putStrLn "\n------------=== Filtrando Lista ===------------ \n"
    mapM_ (putStrLn.tarefaParaString) listaFiltrada
    putStrLn "\n------------=== Lista Filtrada ===------------ \n"
    return tarefas


buscarPorPalavraChaveMenu :: [Tarefa] -> IO [Tarefa]
buscarPorPalavraChaveMenu tarefas = do
    palavraChave <- inputString "Digite a palavra chave: "
    let listaFiltrada = buscarPorPalavraChave palavraChave tarefas
    putStrLn "\n------------=== Ordenando Lista ===------------ \n"
    mapM_ (putStrLn.tarefaParaString) listaFiltrada
    putStrLn "\n------------=== Ordenando Lista ===------------ \n"
    return tarefas

verificarAtrasosMenu :: [Tarefa] -> IO [Tarefa]
verificarAtrasosMenu tarefas = do
    currentTime <- getCurrentTime
    putStrLn "Atrasados:"
    print $ verificarAtrasos tarefas (utctDay currentTime)  -- lembrar de imprimir bonitinho dps (utctDay toGregorian.
    return tarefas

calcularDiasRestantesMenu :: [Tarefa] -> IO [Tarefa]
calcularDiasRestantesMenu tarefas = do
    id <- input "Digite o id da tarefa: " :: IO Int
    let tarefa = findTarefa id tarefas
    case tarefa of
        Nothing -> putStrLn "Tarefa não encontrada"
        Just t -> do
            currentTime <- getCurrentTime
            let dias = calcularDiasRestantes t (utctDay currentTime)
            case dias of
                Nothing -> putStrLn "Essa tarefa não tem prazo"
                Just d  -> putStrLn $ "Dias restantes: " ++ show d
    return tarefas
    where
        findTarefa _ [] = Nothing
        findTarefa id (t:ts)
            | idTarefa t == id = Just t
            | otherwise = findTarefa id ts

filtrarPorTagMenu :: [Tarefa] -> IO [Tarefa]
filtrarPorTagMenu tarefas = do
    tag <- inputString "Digite a tag a ser procurada: "
    let listaFiltrada = filtrarPorTag tag tarefas
    putStrLn "\n------------=== Filtrando Lista ===------------ \n"
    mapM_ (putStrLn.tarefaParaString) listaFiltrada
    putStrLn "\n------------=== Lista Filtrada ===------------ \n"
    return tarefas


nuvemDeTagsMenu :: [Tarefa] -> IO [Tarefa]
nuvemDeTagsMenu tarefas = do
    putStrLn "Tags:"
    print $ nuvemDeTags tarefas -- lembrar de imprimir bonitinho dps
    return tarefas
