module FuncoesJogo
(   coletaJogadores
,   sorteiaCarta
,   carregaCartas
,   converte
,   formaListaCartas
,   pegaCartaSorteada
,   avisaAssunto
,   loopJogo
,   obterNumero
,   obterNome
,   obterNumeroCarta
,   obterResposta
,   obterAssunto
,   obterDica1
,   obterDica2
,   obterDica3
,   obterDica4
,   obterDica5
,   obterDica6
,   obterDica7
,   obterDica8
,   obterDica9
,   obterDica10
,   obterDica11
,   obterDica12
,   obterDica13
,   obterDica14
,   obterDica15
,   obterDica16
,   obterDica17
,   obterDica18
,   obterDica19
,   obterDica20
)where

import System.Random (randomRIO)
import Data.Char

data Jogador = Jogador  { numero :: Int
                        , nome :: String
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
        let novoJogador = [Jogador { numero = aux, nome = jogador}]
        coletaJogadores (jogadores ++ novoJogador) numJ (aux+1)
    else
        return $ jogadores

sorteiaCarta :: IO Int
sorteiaCarta  = do
    number <- randomRIO (0,15) :: IO Int
    return number
    
carregaCartas :: IO [String]
carregaCartas = do
    arq <- readFile "listaDicas.txt"
    let cartas = lines arq
    return $ cartas

converte :: [String] -> [FuncoesJogo.Carta] -> [FuncoesJogo.Carta]
converte [] [] = []
converte [] ys = ys
converte [x] [] = [read x :: FuncoesJogo.Carta]
converte (x:xs) [y] = converte xs (( read x :: FuncoesJogo.Carta ) : [y])
converte (x:xs) ys = converte xs (( read x :: FuncoesJogo.Carta ) : ys)

formaListaCartas :: IO [FuncoesJogo.Carta]
formaListaCartas  = do
    cartas <- carregaCartas
    let listaCartas = reverse $ converte cartas []
    return listaCartas

pegaCartaSorteada :: Monad m => Int -> [a] -> m a
pegaCartaSorteada numeroCarta listaCartas = do
    let cartaSorteada = listaCartas !! numeroCarta
    return cartaSorteada

avisaAssunto :: FuncoesJogo.Carta -> IO ()
avisaAssunto cartaSorteada = do
    putStrLn "\n\n--------------------------------------------------------------------------------"
    putStrLn ("\nAvise aos jogadores que sou um(a) " ++ (map toUpper(obterAssunto cartaSorteada)))
    putStrLn "--------------------------------------------------------------------------------"

loopJogo :: Int -> [FuncoesJogo.Jogador] -> FuncoesJogo.Carta -> [Int] -> Int -> IO ()
loopJogo numJ jogadores cartaSorteada dicas aux = do
    if length dicas /= 20 then do
        if aux == numJ then do
            loopJogo numJ jogadores cartaSorteada dicas 0
        else do
            putStrLn ("\n\nVez do Jogador " ++ show (obterNumero(jogadores !! aux) + 1) ++ " - " ++ (map toUpper(obterNome (jogadores !! aux))))
            putStrLn "Digite um número de 1 a 20: "
            numeroDica <- getLine
            let numDica = read numeroDica :: Int
            if  numDica > 20 || numDica < 1 then do
                putStrLn "Opsss...Essa dica não existe...digite novamente"
                loopJogo numJ jogadores cartaSorteada dicas aux
            else do
                let strDica = show numDica
                let numDicaLista = read strDica :: Int
                let confereListaDica = numDica `elem` dicas
                if  confereListaDica == True then do
                    putStrLn "Essa dica já saiu... digite novamente"
                    loopJogo numJ jogadores cartaSorteada dicas aux
                else do
                    case numDica of 1 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica1(cartaSorteada)))
                                        putStrLn "--------------------------------------------------------------------------------"
                                    2 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica2(cartaSorteada)))
                                        putStrLn "--------------------------------------------------------------------------------"
                                    3 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica3(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    4 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica4(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    5 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica5(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    6 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica6(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    7 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica7(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    8 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica8(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    9 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica9(cartaSorteada)))    
                                        putStrLn "--------------------------------------------------------------------------------"
                                    10 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica10(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    11 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica11(cartaSorteada)))
                                        putStrLn "--------------------------------------------------------------------------------"
                                    12 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica12(cartaSorteada)))
                                        putStrLn "--------------------------------------------------------------------------------"
                                    13 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica13(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    14 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica14(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    15 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica15(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    16 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica16(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    17 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica17(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    18 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica18(cartaSorteada)))  
                                        putStrLn "--------------------------------------------------------------------------------"
                                    19 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica19(cartaSorteada)))    
                                        putStrLn "--------------------------------------------------------------------------------"
                                    20 -> do
                                        putStrLn "\n\n--------------------------------------------------------------------------------"
                                        putStrLn (show (obterDica20(cartaSorteada)))
                                        putStrLn "--------------------------------------------------------------------------------"

                    putStrLn "\nDeseja dar um palpite ? (s para sim ou aperte qualquer tecla para não)"   
                    escolha <- getChar
                    if toLower(escolha) == 's' then do
                        putStrLn "\nDigite seu palpite: "
                        resposta <- getLine
                        if resposta == obterResposta(cartaSorteada) then do
                            putStrLn("\nO jogador(a) " ++ (map toUpper(obterNome (jogadores !! aux))) ++ " venceu =)")
                            return ()
                        else do
                            putStrLn "\nEssa nao é a resposta :("
                            loopJogo numJ jogadores cartaSorteada (numDicaLista : dicas) (aux+1)
                    else
                        loopJogo numJ jogadores cartaSorteada (numDicaLista : dicas) (aux+1)
    else 
        putStrLn "\nO número máximo de dicas foi atingido. Nenhum jogador venceu a partida"


obterNumero :: Jogador -> Int
obterNumero (Jogador numero _ ) = numero

obterNome :: Jogador -> String
obterNome (Jogador _ nome) = nome

obterNumeroCarta :: FuncoesJogo.Carta -> Int
obterNumeroCarta (Carta numCarta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = numCarta

obterResposta :: FuncoesJogo.Carta -> String
obterResposta (Carta _ resposta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) = resposta

obterAssunto :: FuncoesJogo.Carta -> String
obterAssunto (Carta _ _ assunto _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = assunto

obterDica1 :: FuncoesJogo.Carta -> String
obterDica1 (Carta _ _ _ dica1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = dica1

obterDica2 :: FuncoesJogo.Carta -> String
obterDica2 (Carta _ _ _ _ dica2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = dica2

obterDica3 :: FuncoesJogo.Carta -> String
obterDica3 (Carta _ _ _ _ _ dica3 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = dica3

obterDica4 :: FuncoesJogo.Carta -> String
obterDica4 (Carta _ _ _ _ _ _ dica4 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = dica4

obterDica5 :: FuncoesJogo.Carta -> String
obterDica5 (Carta _ _ _ _ _ _ _ dica5 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = dica5

obterDica6 :: FuncoesJogo.Carta -> String
obterDica6 (Carta _ _ _ _ _ _ _ _ dica6 _ _ _ _ _ _ _ _ _ _ _ _ _ _) = dica6

obterDica7 :: FuncoesJogo.Carta -> String
obterDica7 (Carta _ _ _ _ _ _ _ _ _ dica7 _ _ _ _ _ _ _ _ _ _ _ _ _) = dica7

obterDica8 :: FuncoesJogo.Carta -> String
obterDica8 (Carta _ _ _ _ _ _ _ _ _ _ dica8 _ _ _ _ _ _ _ _ _ _ _ _) = dica8

obterDica9 :: FuncoesJogo.Carta -> String
obterDica9 (Carta _ _ _ _ _ _ _ _ _ _ _ dica9 _ _ _ _ _ _ _ _ _ _ _) = dica9

obterDica10 :: FuncoesJogo.Carta -> String
obterDica10 (Carta _ _ _ _ _ _ _ _ _ _ _ _ dica10 _ _ _ _ _ _ _ _ _ _) = dica10

obterDica11 :: FuncoesJogo.Carta -> String
obterDica11 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ dica11 _ _ _ _ _ _ _ _ _) = dica11

obterDica12 :: FuncoesJogo.Carta -> String
obterDica12 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica12 _ _ _ _ _ _ _ _) = dica12

obterDica13 :: FuncoesJogo.Carta -> String
obterDica13 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica13 _ _ _ _ _ _ _) = dica13

obterDica14 :: FuncoesJogo.Carta -> String
obterDica14 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica14 _ _ _ _ _ _) = dica14

obterDica15 :: FuncoesJogo.Carta -> String
obterDica15 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica15 _ _ _ _ _) = dica15

obterDica16 :: FuncoesJogo.Carta -> String
obterDica16 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica16 _ _ _ _) = dica16

obterDica17 :: FuncoesJogo.Carta -> String
obterDica17 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica17 _ _ _) = dica17

obterDica18 :: FuncoesJogo.Carta -> String
obterDica18 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica18 _ _) = dica18

obterDica19 :: FuncoesJogo.Carta -> String
obterDica19 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica19 _) = dica19

obterDica20 :: FuncoesJogo.Carta -> String
obterDica20 (Carta _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ dica20) = dica20


    