module Funcoes where
 import Tipos

 -- filtra a lista "lista" com a categoria "cat"

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cat lista = filter (\t -> categoria t == cat) lista


-- filtra a lista "lista" com a prioridade "pri"

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade pri lista = filter (\t -> prioridade t == pri) lista

