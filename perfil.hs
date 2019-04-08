import System.IO
import FuncoesJogo
import Data.List
-- import System.Console.ANSI
 
menu = do
  putStrLn "\n\nBem vindo ao jogo Perfil em Haskell"
  putStrLn "Escolha uma opção abaixo"
  putStrLn "1 - Jogar"
  putStrLn "2 - Regras"
  putStrLn "0 - Sair"
  putStrLn "Digite sua opção: "
  opcao <- getLine
  let numOpcao = read opcao :: Int
  if  numOpcao < 0 || numOpcao > 2 then do
    putStrLn "Digite uma opcao valida"
    menu
  else do
    case numOpcao of  1 -> do
                        putStrLn "Quantas pessoas irao jogar ?"
                        qtd <- getLine
                        let numJ = read qtd :: Int
                        let jogadores = []
                        listaJogadores <- coletaJogadores jogadores numJ 0
                        numeroCarta <- sorteiaCarta
                        listaCartas <- formaListaCartas
                        cartaSorteada <- pegaCartaSorteada numeroCarta listaCartas
                        avisaAssunto cartaSorteada
                        let dicas = []
                        loopJogo numJ listaJogadores cartaSorteada dicas 0
                        menu
                    
                      2 -> do
                        regras <- readFile "regras.txt"
                        putStrLn regras
                        putStrLn "\n\nAperte ENTER para voltar ao menu"
                        confirma <- getChar
                        menu
                      0 -> do
                        putStrLn "\n\n--------------------------------------------------------------------------------"
                        putStrLn "Finalizando jogo ..."
                        putStrLn "Até mais"
                        putStrLn "--------------------------------------------------------------------------------"
                        return ()
