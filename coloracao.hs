-- Importações
 
import System.Environment
import qualified Data.Bimap as Bimap
import SAT.Mios (CNFDescription (..), solveSAT)
    

-- Declarações de tipo úteis

type Vertice = Int
type Aresta = (Vertice, Vertice)
type Grafo = [Aresta]
type Cor = Int
type Clausula = [Int]
type Formula = [Clausula]
type Mapa = Bimap.Bimap Int (Vertice, Cor)


-- Função principal

main :: IO ()
main = do 
    args <- getArgs
    case args of
         [file] -> do
            texto <- readFile file
            let resolverSAT grafo lista solAtual mapAtual = case lista of
                    [] -> return ()
                    (x:xs) -> do
                                let codificao = codificar $ grafoParaClausulas grafo x 
                                    clauses = fst codificao
                                    mapa = snd codificao
                                    desc = CNFDescription (Bimap.size mapa) (length clauses) ""
                                asg <- solveSAT desc clauses
                                if null asg
                                    then (putStr $ "Numero cromatico: " ++ (show $ x+1) ++ "\n" ++ (formatarSaida $ decodificar solAtual mapAtual)) >> return ()
                                    else resolverSAT grafo xs asg mapa
            resolverSAT (textoParaGrafo texto) (reverse [1 .. ((maiorGrau $ textoParaGrafo texto)+1)]) [] (Bimap.empty)
         _ -> putStr "Argumentos invalidos"    
       
       
-----------  Demais funções


---- Manipulação de Texto

textoParaGrafo :: String -> Grafo
textoParaGrafo texto = map (\linha -> (read (linha!!1) :: Int, read (linha!!2) :: Int)) (filtrarArestas texto)

filtrarArestas :: String -> [[String]]
filtrarArestas texto = filter (\linha -> (head linha) == "e") (separarPalavras texto)

separarPalavras :: String -> [[String]]
separarPalavras texto = map words (lines texto)


---- Propriedades de Grafos

maiorGrau :: Grafo -> Int
maiorGrau g = maximum $ map (\x -> contarGrau g (fst x)) g

contarGrau :: Grafo -> Vertice -> Int
contarGrau g v = length $ filter (\x -> (fst x) == v) g 

numeroVertices :: Grafo -> Int
numeroVertices g = length $ foldl adicionarVerticeSemDuplicata [] g

adicionarVerticeSemDuplicata :: [Vertice] -> Aresta -> [Vertice]
adicionarVerticeSemDuplicata visto (x, y) = if elem x visto
                                            then if elem y visto
                                                 then visto
                                                 else y : visto
                                            else if elem y visto 
                                                 then x : visto 
                                                 else x : y : visto
                         
                         
---- Codificação para SAT

grafoParaClausulas :: Grafo -> Int -> ([[(Vertice, Cor, Int)]], Mapa)
grafoParaClausulas g nc = 
    let nv = numeroVertices g
        restricao1 = [[(v, c, 1) | c <- [1..nc]] | v <- [1..nv]]
        restricao2 = concat $ map (\(x, y) -> [[(x, c, -1), (y, c, -1)] | c <- [1..nc]]) g
        listaCorrespondencia = Bimap.fromList $ listaTuplasParaListaIndexada (criarListaTuplasVerticeCor nv nc) 1
    in (restricao1 ++ restricao2, listaCorrespondencia)

criarListaTuplasVerticeCor :: Int -> Int -> [(Vertice, Cor)]
criarListaTuplasVerticeCor nv nc = [(v, c) | v <- [1..nv], c <- [1..nc]]

listaTuplasParaListaIndexada :: [(Vertice, Cor)] -> Int -> [(Int, (Vertice, Cor))]
listaTuplasParaListaIndexada [] _ = []
listaTuplasParaListaIndexada ((v, c):xs) x = (x, (v, c)) : listaTuplasParaListaIndexada xs (x+1) 

codificar :: ([[(Vertice, Cor, Int)]], Mapa) -> (Formula, Mapa)
codificar (lista, m) = 
    let novaLista = map (converterVariavel m) lista
    in (novaLista, m)
    
converterVariavel :: Mapa -> [(Vertice, Cor, Int)] -> Clausula
converterVariavel m lista = map (\(v, c, x) -> x * (desempacotar m (v, c))) lista

desempacotar :: Mapa -> (Int, Int) -> Int
desempacotar m t = case (Bimap.lookupR t m) of
                        Just x -> x
                        Nothing -> 0

                        
---- Decodificar do SAT 

decodificar :: [Int] -> Mapa -> [(Vertice, Cor)]
decodificar f m = map (desconverterVariavel m) (filter (\x -> x > 0) f)

desconverterVariavel :: Mapa -> Int -> (Vertice, Cor)
desconverterVariavel m x = case (Bimap.lookup x m) of
                                Just y -> y
                                Nothing -> (0, 0)
    
formatarSaida :: [(Vertice, Cor)] -> String
formatarSaida lista = concat $ map (\(v,c) -> "Vertice " ++ (show v) ++ " com cor " ++ (show c) ++ "\n") lista 
