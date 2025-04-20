module Tipos where

 import Data.Time.Calendar (Day)

 data Status = Pendente | Concluída deriving (Read, Show, Eq)

 data Prioridade = Baixa | Media | Alta deriving (Read, Show, Eq, Ord)

 data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Read, Show, Eq)

 data Tarefa = Tarefa
  { idTarefa    :: Int
  , descricao   :: String
  , status      :: Status
  , prioridade  :: Prioridade
  , categoria   :: Categoria
  , prazo       :: Maybe Day
  , tags       :: [String]
  } deriving (Read, Show, Eq)
