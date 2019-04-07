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
  case opcao of "1" -> do
                  putStr "\ESC[2J"
                  -- hClearScreen
                  putStrLn "Quantas pessoas irao jogar ?"
                  qtd <- getLine
                  -- putStrLn (qtd ++ " jogadores")
                  let numJ = read qtd :: Int
                  let jogadores = []
                  listaJogadores <- coletaJogadores jogadores numJ 0
                  -- print (show listaJogadores)
                  numeroCarta <- sorteiaCarta
                  -- putStrLn ("fora da funcao " ++ show (numeroCarta))
                  listaCartas <- formaListaCartas
                  -- print listaCartas
                  cartaSorteada <- pegaCartaSorteada numeroCarta listaCartas
                  -- print cartaSorteada
                  avisaAssunto cartaSorteada
                  let dicas = []
                  loopJogo numJ listaJogadores cartaSorteada dicas 0
                  -- print $ show $ length listaCartas
                  menu
                  
                "2" -> do
                  regras <- readFile "regras.txt"
                  putStrLn regras
                  putStrLn "\n\nAperte ENTER para voltar ao menu"
                  confirma <- getChar
                  menu
                "0" -> do
                  putStrLn "Finalizando jogo ..."
                  putStrLn "Até mais"
                  return ()
