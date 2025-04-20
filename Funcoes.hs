module Funcoes (
    adicionarTarefa,
    removerTarefa,
    marcarConcluida,
    listarPorCategoria,
    listarPorPrioridade,
    ordenarPorPrioridade,
    filtrarPorStatus,
    buscarPorPalavraChave,
    verificarAtrasos,
    calcularDiasRestantes,
    filtrarPorTag,
    mostrarTarefa,
    nuvemDeTags
) where
import Data.Time.Calendar (Day, diffDays)
import Tipos


-- FUNÇÕES BÁSICAS
adicionarTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionarTarefa novaTarefa tarefas
    | [t | t <- tarefas, idTarefa t == idTarefa novaTarefa] /= [] = Left "Erro!Tarefa já registrada"
    | otherwise = Right (novaTarefa : tarefas)

removerTarefa :: Int -> [Tarefa] -> Either String [Tarefa]
removerTarefa id tarefas =
    if any (\t -> idTarefa t == id) tarefas
       then Right (filter (\t -> idTarefa t /= id) tarefas)
       else Left "Erro! Tarefa não encontrada"

marcarConcluida :: Int -> [Tarefa] -> Either String [Tarefa]
marcarConcluida id tarefas =
    if any (\t -> idTarefa t == id) tarefas
       then Right (map (\t -> if idTarefa t == id then t {status = Concluída} else t) tarefas)
       else Left "Erro! Tarefa não encontrada"
-- FUNÇÕES BÁSICAS




-- filtra a lista "lista" com a categoria "cat"

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cat lista = filter (\t -> categoria t == cat) lista

-- filtra a lista "lista" com a prioridade "pri"

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade pri lista = filter (\t -> prioridade t == pri) lista

-- se a lista for unitaria, retorna o unico elemento, caso seja binaria, compara a com b e retorna [a, b] ou [b, a], 
-- dependendo da prioridade, caso o tamanho seja > 2, ela compara os valores um por um, salvando no topo da lista os de maior prioridade

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade [] = []
ordenarPorPrioridade [a] = [a]
ordenarPorPrioridade [a, b]
    |prioridade a > prioridade b = [a, b]
    |otherwise = [b, a]
ordenarPorPrioridade (a:b:c:xs)
    |prioridade a >= prioridade b && prioridade a >= prioridade c = a:ordenarPorPrioridade (b:c:xs)
    |prioridade b >= prioridade a && prioridade b >= prioridade c = b:ordenarPorPrioridade (a:c:xs)
    |otherwise = c:ordenarPorPrioridade(a:b:xs)

-- filter nesse caso tem como objetivo filtrar os elementos de um certo status t que são iguais a variavel estado da lista de tarefas principal

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus estado lista = filter (\t -> status t == estado) lista

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave tarefReq lista = 
    filter(\t -> transformarMinuscula (descricao t) == transformarMinuscula tarefReq) lista 

transformarMinuscula :: String -> String
transformarMinuscula [] = []
transformarMinuscula (x:xs)
    | x >= 'A' && x <= 'Z' = toEnum (fromEnum x + 32) : transformarMinuscula xs
    | otherwise            = x : transformarMinuscula xs


mostrarTarefa :: Tarefa -> String
mostrarTarefa tarefa =
    "ID: " ++ show (idTarefa tarefa) ++ "; " ++
    "Descrição: " ++ show (descricao tarefa) ++ "; " ++
    "Status: " ++ show (status tarefa) ++ "; " ++
    "Prioridade: " ++ show (prioridade tarefa) ++ "; " ++
    "Categoria: " ++ show (categoria tarefa) ++ "; " ++
    "Prazo: " ++ maybe "Sem prazo" show (prazo tarefa) ++ "; " ++
    "Tags: " ++ show (tags tarefa)


-- KG

-- Se a tarefa não tem prazo, o resultado é Nothing. Caso contrário, usa a função diffDays pra pegar a quantidade
-- de dias, transforma de Interger pra Int e retorna como Just (significa que o valor existe, não é nothing)
calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes (Tarefa {prazo = Nothing}) _ = Nothing
calcularDiasRestantes (Tarefa {prazo = Just prazo}) termino = Just $ fromIntegral $ diffDays prazo termino


verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos tarefas dia =
    [t | t <- tarefas,
     case prazo t of
       Just p -> p <= dia
       Nothing -> False
    ]


-- Quando cada tarefa entra no `filter` primeiro selecionamos a sua lista de tags e depois filtramos vendo
-- se a tag procurada está na lista.
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
filtrarPorTag tag tarefas = filter (any (== tag) . tags) tarefas


-- Essa é uma função auxiliar que não será exportada.
removerDuplicadas :: Eq a => [a] -> [a]
removerDuplicadas [] = []
removerDuplicadas (x:xs) = x : removerDuplicadas (filter (/= x) xs)


-- Em allTags, primeiro pegamos as listas de tags de cada tarefa com o `map` e concatenamos essas listas. Depois
-- com o map para cada tag contamos quantas vezes ela aparece na propria lista, e depois removemos as duplicadas.
nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags tarefas = removerDuplicadas $ map (\t -> (t, count t allTags)) allTags
                where
                    allTags         = concat $ map (tags) tarefas
                    count x xs      = length $ filter (==x) xs
