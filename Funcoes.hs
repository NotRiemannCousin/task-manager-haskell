module Funcoes where
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
       then Right (map (\t -> if idTarefa t == id then t {status = Concluida} else t) tarefas)
       else Left "Erro! Tarefa não encontrada"
-- FUNÇÕES BÁSICAS

-- filtra a lista "lista" com a categoria "cat"

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cat lista = filter (\t -> categoria t == cat) lista

-- filtra a lista "lista" com a prioridade "pri"

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade pri lista = filter (\t -> prioridade t == pri) lista


-- filter nesse caso tem como objetivo filtrar os elementos de um certo
-- status t que são iguais a variavel estado da lista de tarefas principal
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
