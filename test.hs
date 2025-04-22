module Testes (runTests) where

import Test.QuickCheck
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Tipos
import Funcoes

instance Arbitrary Day where
    arbitrary = do
        ano <- choose (2000, 2030)
        mes <- choose (1, 12)
        dia <- case () of
            _ | mes == 2 -> choose (1, 28)
              | mes `elem` [4,6,9,11] -> choose (1, 30)
              | otherwise -> choose (1, 31)
        return $ fromGregorian ano mes dia

instance Arbitrary Status where
    arbitrary = elements [Pendente, Concluída] -- usamos a função elements para sortear aleatoriamente 

instance Arbitrary Prioridade where
    arbitrary = elements [Baixa, Media, Alta]

instance Arbitrary Categoria where
    arbitrary = elements [Trabalho, Estudos, Pessoal, Outro]

instance Arbitrary Tarefa where
    arbitrary = do
        id <- arbitrary -- onde id representa o IdTarefa
        des <- arbitrary -- onde des representa a descricao
        stat <- arbitrary -- onde stat representa o status
        pri <- arbitrary -- onde pri representa prioridade
        cat <- arbitrary -- onde cat representa a categoria
        praz <- arbitrary:: Gen (Maybe Day) -- onde praz representa o prazo
        tg <- arbitrary:: Gen [String] -- onde tg representa a tag, e gerara uma lista de string aleatorias
        return $ Tarefa id des stat pri cat praz tg

propAddTarefaNp :: Tarefa -> [Tarefa] -> Property
propAddTarefaNp x y = 
    null [ t| t <- y, idTarefa t == idTarefa x ] ==> -- se nao existem tarefas com mesmo id o teste é executado
        adicionarTarefa x y == Right (x:y)

propAddProblem :: Tarefa -> [Tarefa] -> Property
propAddProblem x y =
    not (null[t| t <- y, idTarefa t == idTarefa x]) ==> -- se ja existe tarefa com mesmo id o teste é executado
        adicionarTarefa x y == Left "Erro!"

propRemovNp :: Int -> [Tarefa] -> Property
propRemovNp x y =
    any (\t -> idTarefa t == x) y ==> 
        removerTarefa x y == Right (filter(\t -> idTarefa t /= x) y) 

propRemovProblem :: Int -> [Tarefa] -> Property
propRemovProblem x y =
    not (any(\t -> idTarefa t == x) y) ==>
        removerTarefa x y == Left "Erro!"

propMarcarConcluidaNp :: Int -> [Tarefa] -> Property
propMarcarConcluidaNp x y = 
    any (\t -> idTarefa t == x) y ==> -- se encontrar a tarefa o teste é executado
        marcarConcluida x y == Right (map (\t -> if idTarefa t == x then t {status = Concluída} else t) y)

propMarcarConcluidaProblem :: Int -> [Tarefa] -> Property
propMarcarConcluidaProblem x y =
    all (\t -> idTarefa t /= x) y ==> -- se nao encontrar a tarefa o teste e executado
        marcarConcluida x y == Left "Erro! Tarefa não encontrada!"

proplistarPorCategoriaNp :: Categoria -> [Tarefa] -> Property
proplistarPorCategoriaNp x y =
    not(null(filter (\t -> categoria t == x) y)) ==>
        listarPorCategoria x y == filter (\t -> categoria t == x) y

proplistarPorCategoriaProblem :: Categoria -> [Tarefa] -> Property
proplistarPorCategoriaProblem x y =
    null [t | t <- y, categoria t == x] ==>
        listarPorCategoria x y == []

propfiltrarPorStatusNp :: Status->[Tarefa]->Property
propfiltrarPorStatusNp x y =
    not(null (filter (\t-> status t == x ) y)) ==> 
        filtrarPorStatus x y == filter (\t -> status t == x) y

propfiltrarporStatusProblem :: Status -> [Tarefa] -> Property
propfiltrarporStatusProblem x y =
    null (filter (\t -> status t == x) y) ==>
        filtrarPorStatus x y == []

propbuscarPorPalavraChaveNp :: String -> [Tarefa] -> Property
propbuscarPorPalavraChaveNp x y =
    not (null (filter (\t -> transformarMinuscula (descricao t ) == transformarMinuscula x) y)) ==> 
        buscarPorPalavraChave x y == filter(\t -> transformarMinuscula (descricao t) == transformarMinuscula x) y

propbuscarPorPalavraChaveProblem :: String -> [Tarefa] -> Property
propbuscarPorPalavraChaveProblem x y =
    null (filter (\t -> transformarMinuscula (descricao t ) == transformarMinuscula x) y) ==>
        buscarPorPalavraChave x y == []

propCalcularDiasRestantesNp :: Tarefa -> Day -> Property 
propCalcularDiasRestantesNp x dia = 
    prazo x /= Nothing ==> -- executa caso tenha um prazo definido
        calcularDiasRestantes x dia == (maybe Nothing (\p -> Just (fromIntegral (diffDays p dia)))) (prazo x)

propCalcularDiasRestantesProblem :: Tarefa -> Day -> Property
propCalcularDiasRestantesProblem x dia =
    prazo x == Nothing ==> -- executa caso a tarefa não possua um prazo definido 
        calcularDiasRestantes x dia == Nothing

propVerificaAtrasosNp :: [Tarefa] -> Day -> Property 
propVerificaAtrasosNp x dia =
    not (null x) ==> -- somente executa se a lista não for vazia
        all (\t -> case prazo t of
            Just p -> if p < dia then t `elem` verificarAtrasos x dia else True
            Nothing -> True) x


propVerificaAtrasosProblem :: [Tarefa] -> Day -> Property 
propVerificaAtrasosProblem x dia =
    not (null x) ==>
        all (\t -> case prazo t of
                Just p -> if p > dia then notElem t (verificarAtrasos x dia) else True
                Nothing -> notElem t (verificarAtrasos x dia)) x

propfiltrarPorTagNp :: String -> [Tarefa] -> Property 
propfiltrarPorTagNp x y =
    not (null y) ==> -- o codigo é executado quando a lista tarefas nao é vazia
        all (\t -> any (==x) (tags t)) (filtrarPorTag x y)

propFiltrarPorTagProblem :: String -> [Tarefa] -> Property 
propFiltrarPorTagProblem x y =
    all (\t -> not (any (== x) (tags t))) y ==>
        filtrarPorTag x y == []

propNuvemDeTagsNp :: [Tarefa] -> Property 
propNuvemDeTagsNp y = 
    not (null y) ==>
        all (\(tag, count) -> count == length (filter (== tag) (concatMap tags y))) (nuvemDeTags y)

propNuvemDeTagsProblem :: [Tarefa] -> Property
propNuvemDeTagsProblem y =
    null y ==> nuvemDeTags y == []

runTests :: IO()
runTests = do
    putStrLn "-----Testando Funções...-----"
    quickCheck propAddTarefaNp
    quickCheck propAddProblem
    quickCheck propRemovNp
    quickCheck propRemovProblem
    quickCheck propMarcarConcluidaNp
    quickCheck propMarcarConcluidaProblem
    quickCheck proplistarPorCategoriaNp
    quickCheck proplistarPorCategoriaProblem
    quickCheck propfiltrarPorStatusNp
    quickCheck propfiltrarporStatusProblem
    quickCheck propbuscarPorPalavraChaveNp
    quickCheck propbuscarPorPalavraChaveProblem
    quickCheck propCalcularDiasRestantesNp
    quickCheck propCalcularDiasRestantesProblem
    quickCheck propVerificaAtrasosNp
    quickCheck propVerificaAtrasosProblem
    quickCheck propfiltrarPorTagNp
    quickCheck propFiltrarPorTagProblem
    quickCheck propNuvemDeTagsNp
    quickCheck propNuvemDeTagsProblem









