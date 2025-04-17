module Funcoes where
 import Tipos

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
