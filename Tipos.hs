module Tipos where

 import Data.Time.Calendar (Day)

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
  } deriving (Show, Read, Eq)

