module Tipos where

import Data.Time.Calendar
import Data.Time

data Status = Pendente | Concluída deriving (Show, Read, Eq)

data Prioridade = Baixa | Media | Alta deriving (Show, Read, Eq, Ord)

data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Read, Eq)

data Tarefa = Tarefa
    { idTarefa    :: Int
    , descricao   :: String
    , status      :: Status
    , prioridade  :: Prioridade
    , categoria   :: Categoria
    , prazo       :: Maybe Day
    , tags       :: [String]
    } deriving (Read, Eq)

instance Show Tarefa where
    show tarefa =
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
        where
            formatarData = formatTime defaultTimeLocale "%Y-%m-%d"