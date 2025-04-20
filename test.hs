module Testes (runTests) where

import Test.QuickCheck
import Tipos
import Funcoes

instance Arbitrary Status where
    arbitrary = elements [Pendente, Concluída] -- usamos a função elements para sortear aleatoriamente 

instance Arbitrary Propriedades where
    arbitrary = elements [Baixa, Media, Alta]

instance Arbitrary Categoria where
    arbitrary = elements [Trabalho, Estudos, Pessoal, Outro]

instance Arbitrary Tarefas where
    arbitrary = do
        id <- arbitrary -- onde id representa o IdTarefa
        des <- arbitrary -- onde des representa a descricao
        stat <- arbitrary -- onde stat representa o status
        pri <- arbitrary -- onde pri representa prioridade
        cat <- arbitrary -- onde cat representa a categoria
        praz <- arbitrary:: Gen (Maybe String) -- onde praz representa o prazo
        tg <- arbitrary:: Gen [String] -- onde tg representa a tag, e gerara uma lista de string aleatorias
        return $ Tarefa id des stat cat praz tg [t]

propAddTarefaNp :: Tarefa -> [Tarefa] -> Property
propAddTarefaNp x y = 
    null [t| t <- y, IdTarefa t == IdTarefa x] ==> -- se nao existem tarefas com mesmo id o teste é executado
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
    all (\t -> idTarefa t =/ x) y ==> -- se nao encontrar a tarefa o teste e executado
        marcarConcluida x y == Left "Erro! Tarefa não encontrada!"

listarPorCategoriaNp :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoriaNp x y =
    not(null(filter (\t -> categoria t == x))) y ==>
        listarPorCategoria x y == filter (\t -> categoria t == x) y

listarPorCategoriaProblem :: Categoria -> [Tarefa] -> Property
listarPorCategoriaProblem x y =
    null(filter (\t -> categoria t == x) y) ==>
        listarPorCategoria x y == Left "Erro! Categoria não encontrada!"

filtrarPorStatusNp :: Status->[Tarefa]->[Tarefa]
filtrarPorStatusNp x y =
    not(null (filter (\t-> status t == x ) y)) ==> 
        filtrarPorStatus x y = filter (\t -> categoria t == x) y

filtrarporStatusProblem :: Status -> [Tarefa] -> [Tarefa]
