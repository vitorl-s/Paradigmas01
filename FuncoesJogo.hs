module FuncoesJogo
(   coletaJogadores
,   sorteiaCarta
,   carregaCartas
,   converte
,   obterNumero
,   obterNome
,   obterPontuacao
)where

import System.Random (randomRIO)

data Jogador = Jogador  { numero :: Int
                        , nome :: String
                        , pontuacao :: Int
                        } deriving (Show, Read, Eq)

data Carta = Carta      { numCarta :: Int
                        , resposta :: String
                        , assunto :: String
                        , dica1 :: String
                        , dica2 :: String
                        , dica3 :: String
                        , dica4 :: String
                        , dica5 :: String
                        , dica6 :: String
                        , dica7 :: String
                        , dica8 :: String
                        , dica9 :: String
                        , dica10 :: String
                        , dica11 :: String
                        , dica12 :: String
                        , dica13 :: String
                        , dica14 :: String
                        , dica15 :: String
                        , dica16 :: String
                        , dica17 :: String
                        , dica18 :: String
                        , dica19 :: String
                        , dica20 :: String
                        } deriving (Show, Read, Eq)


coletaJogadores :: [FuncoesJogo.Jogador] -> Int -> Int -> IO [FuncoesJogo.Jogador]
coletaJogadores jogadores numJ aux = do
    if numJ /= aux then do
        putStrLn ("\nDigite o nome do jogador " ++ show (aux+1) ++  ": ")
        jogador <- getLine
        let novoJogador = [Jogador { numero = aux, nome = jogador , pontuacao = 0 }]
        coletaJogadores (jogadores ++ novoJogador) numJ (aux+1)
    else
        return $ jogadores

sorteiaCarta :: IO Int
sorteiaCarta  = do
    number <- randomRIO (0,1) :: IO Int
    -- putStrLn ("dentro da funcao " ++ show (number))
    return number
    
carregaCartas :: IO [String]
carregaCartas = do
    arq <- readFile "listaDicas.txt"
    let cartas = lines arq
    -- putStrLn ("carrega cartas " ++ show (dicas))
    return $ cartas

converte :: [String] -> [FuncoesJogo.Carta] -> [FuncoesJogo.Carta]
converte [] [] = []
converte [] ys = ys
converte [x] [] = [read x :: FuncoesJogo.Carta]
converte (x:xs) [y] = converte xs (( read x :: FuncoesJogo.Carta ) : [y])
converte (x:xs) ys = converte xs (( read x :: FuncoesJogo.Carta ) : ys)

obterNumero :: Jogador -> Int
obterNumero (Jogador numero _ _) = numero

obterNome :: Jogador -> String
obterNome (Jogador _ nome _) = nome

obterPontuacao :: Jogador -> Int
obterPontuacao (Jogador _ _ pontuacao) = pontuacao


    