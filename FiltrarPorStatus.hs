filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus estado lista = filter (\t -> status t == estado) lista
-- filter nesse caso tem como objetivo filtrar os elementos de um certo
-- status t que são iguais a variavel estado da lista de tarefas principal
