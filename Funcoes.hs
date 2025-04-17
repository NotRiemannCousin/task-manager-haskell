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
