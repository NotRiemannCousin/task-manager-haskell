module Persistencias (
    salvarEmArquivo,
    carregarDeArquivo
) where
 import Tipos
 import Funcoes
 import System.IO
 import Data.Maybe (mapMaybe)
 import Text.Read (readMaybe)
 import Control.Exception (evaluate)

 stringParaTarefa :: String -> Maybe Tarefa
 stringParaTarefa = readMaybe

 carregarDeArquivo :: FilePath -> IO [Tarefa]
 carregarDeArquivo ca = do
    co <- readFile ca
    let l = lines co
        tarefas = mapMaybe stringParaTarefa l
    evaluate (length tarefas)
    return tarefas

 salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
 salvarEmArquivo ca co = do
    withFile ca WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle.show) co
