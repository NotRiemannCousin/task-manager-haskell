import Data.Time.Calendar (fromGregorian)
import Funcoes
import Tipos

t1 = Tarefa 1 "CS dos crias" Pendente Alta Outro (Just (fromGregorian 2025 6 7)) ["lazer", "jogo", "importante"]
t2 = Tarefa 2 "Ler livro de Haskell" Pendente Alta Estudos (Just (fromGregorian 2025 4 20)) ["leitura", "importante", "estudo"]
t3 = Tarefa 3 "Enviar relatório" Concluída Media Trabalho (Just (fromGregorian 2025 4 18)) ["trabalho", "importante", "prazo"]
t4 = Tarefa 4 "Comprar presente" Pendente Baixa Pessoal Nothing ["lazer", "pessoal", "lembrança"]
t5 = Tarefa 5 "Fazer exercícios de álgebra" Concluída Alta Estudos Nothing ["estudo", "importante", "exercício"]
t6 = Tarefa 6 "Organizar arquivos" Pendente Media Trabalho (Just (fromGregorian 2025 5 1)) ["organização", "trabalho", "importante"]
t7 = Tarefa 7 "Planejar viagem" Concluída Baixa Pessoal (Just (fromGregorian 2025 7 10)) ["planejamento", "lazer", "pessoal"]


-- trocar pra adicionarTarefa dps
tarefas = [t1, t2, t3, t4, t5, t6, t7]
