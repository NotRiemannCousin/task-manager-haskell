buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave tarefReq lista = filter(\t -> transformarMinuscula (descricao t) == transformarMinuscula tarefReq) lista 



transformarMinuscula :: String -> String
transformarMinuscula [] = []
transformarMinuscula (x:xs)
    | x >= 'A' && x <= 'Z' = toEnum (fromEnum x + 32) : transformarMinuscula xs
    | otherwise            = x : transformarMinuscula xs
