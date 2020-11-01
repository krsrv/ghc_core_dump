------------------------------------------------------------------------------------
--Generate a graphviz file for the core syntax tree
--Set True/False in the main function to set the program to be verbose/not verbose
--To collapse dictionaries (RVar $...) for simple programs, uncomment Line 45. Doing this however, will lead to errors for programs like those which contain typeclasses.
--The program will generate faulty syntax for programs which contain the symbol < either implicitly (like in Ord classes)
-------------------------------------------------------------------------------------


import Core
import ResolvedCore
import System.Random

data Graph = Node {idN::String, namN::String, typN::String, infN::String}  --UniqueID Name Type Add-Info
           | Link {parL::String, chiL::String, optL::String}  --parent child options
           | Cluster {idC::String, hasbegun::Bool}
           | Descriptor
           | End
          deriving (Show)

generateID :: String -> IO String                         --Need this to generate unique ids for nodes/clusters. Hence the IO type for resolveEXPR
generateID typ = do
              g <- randomIO :: IO Word
              return $ typ ++ (show g)

createNode :: RExpr -> String -> Graph                    --Covers creating nodes for Variables, Lambdas and Applications. For functions and AltCons, nodes are manually created
createNode (RVar name) iD = Node iD name "Var" ""
createNode (RLam (name, clas) _) iD = Node iD name "Lam" clas
createNode (RApp _ _) iD = Node iD "@" "App" ""

addDesc :: [Graph] -> RExpr -> [Graph]        
addDesc ((Node id na ty inf):x) (RVar nam) = Node id na ty (unwords [inf, nam]) : x
addDesc ((Node id na ty inf):x) (RLit nam) = Node id (processname na) ty (unwords [inf, (processname nam)]) : x  --remove the # at the end if needed
addDesc ((Node id na ty inf):x) (RType nam) = Node id na ty (unwords [inf, nam]) : x

addDesc ((Node _ _ _ _):_) _ = error "Unknown descriptor"
addDesc (Descriptor:_) _ = error "Error in processing descriptor"
addDesc ((Link _ _ _):_) _ = error "Unknown link"
addDesc ((Cluster _ _):_) _ = error "Unknown cluster"
addDesc (End:_) _ = error "Unexpected end"
addDesc _ _ = error "Empty list"

--addDesc (first:rest) des = (:) first $ addDesc rest des

resolveEXPR :: RExpr -> String -> IO [Graph]
--String is Parent's ID
--resolveEXPR inp@(RVar ('$':_:_)) _ = return $ [Descriptor]
resolveEXPR inp@(RVar _) par = do
                           iD <- generateID "Node"
                           let node = createNode inp iD
                           return $ node : Link par iD "" : []
resolveEXPR inp@(RLit _) _ = return $ [Descriptor]
resolveEXPR inp@(RType _) par = return $ [Descriptor]

resolveEXPR inp@(RLam na expr) par = do
                                 iD <- generateID "Node"
                                 let node = createNode inp iD
                                 rest <- resolveEXPR expr iD
                                 return $ node : Link par iD "" : rest

resolveEXPR inp@(RApp expr1 expr2) par = do
                                     iD <- generateID "Node"
                                     gra1 <- resolveEXPR expr1 iD
                                     gra2 <- resolveEXPR expr2 iD
                                     collapsed <- resolveEXPR expr1 par         --using lazy evaluation here. "collapsed" should be generated when the other child is a "descriptor", i.e., dictionary, type or literal
                                     let newchild = addDesc collapsed expr2
                                     let appnode = createNode inp iD
                                     case gra2 of
                                         [Descriptor] -> return $ newchild
                                         _ -> return $ [appnode, Link par iD ""] ++ gra2 ++ gra1

resolveEXPR inp@(RCase expr altlist) par = do
                                   iD <- generateID "cluster_"
                                   (node : link : rest) <- resolveEXPR expr iD
                                   let lastkey = idN . lastnode $ (node:link:rest)
                                   alt <- sequence $ map (\x -> resolveALT x lastkey iD) altlist
                                   let newlink = Link par (chiL link) $ unwords [optL link, "lhead=" ++ iD]
                                   return $ (node : newlink : Cluster iD True : node : rest) ++ (Cluster iD False :  concat alt)

resolveEXPR inp@(RLet rb expr) par = do
                                   iD <- generateID "cluster_"
                                   (node : rest) <- makeGraph rb
                                   let link = Link par (idN node) $ "lhead=" ++ iD
                                   let lastkey = idN . lastnode $ rest
                                   (enode : elink : erest) <- resolveEXPR expr lastkey
                                   let newelink = Link lastkey (idN enode) $ unwords [optL elink, "ltail="++ iD]
                                   return $ (node : link : Cluster iD True : node : rest) ++ (Cluster iD False : enode : newelink : erest)

--resolveEXPR _ _ = return $ []


resolveALT :: RAlt -> String -> String -> IO [Graph]
resolveALT inp@(RDataAlt na, binds, expr) node_par clus_par = do
                                  iD <- generateID "Node"
                                  let altnode = Node iD (processname na) "DataAlt" $ unwords binds
                                  let link = Link node_par iD $ "ltail=" ++ clus_par
                                  rest <- resolveEXPR expr iD
                                  return $ altnode : link : rest
resolveALT inp@(RLitAlt na, binds, expr) node_par clus_par = do
                                  iD <- generateID "Node"
                                  let altnode = Node iD (processname na) "DataAlt" $ unwords binds
                                  let link = Link node_par iD $ "ltail=" ++ clus_par
                                  rest <- resolveEXPR expr iD
                                  return $ altnode : link : rest
resolveALT inp@(RDEFAULT, binds, expr) node_par clus_par = do
                                  iD <- generateID "Node"
                                  let altnode = Node iD "Default" "DataAlt" $ unwords binds
                                  let link = Link node_par iD $ "ltail=" ++ clus_par
                                  rest <- resolveEXPR expr iD
                                  return $ altnode : link : rest
                               

processname :: String -> String
processname string = case last string of
                       '#' -> processname $ init string
                       _ -> string

lastnode :: [Graph] -> Graph
lastnode list = let temp = last list in case temp of
                  (Node _ _ _ _) -> temp
                  _ -> lastnode $ init list

makeGraph :: RBind -> IO [Graph]
--makeGraph (RNonRec ('$':_) expr)  = return $ [End]
makeGraph (RNonRec name expr) = do
                                iD <- generateID "Function"
                                rest <- resolveEXPR expr iD
                                return $ Node iD name "Function" "" : rest
makeGraph (RRec list) = do 
                        rests <- sequence . map (\(name, expr) -> makeGraph (RNonRec name expr)) $ list
                        return $ concat rests

makeDot :: [Graph] -> Bool -> [String]
makeDot [End] _ = []
makeDot [] _ = []
makeDot ((Node id na _ ""):ch) True = (id ++ " [shape=plaintext, label=<" ++ na ++ ">];") : makeDot ch True
makeDot ((Node id na _ de):ch) True = (id ++ " [shape=plaintext, label=<" ++ na ++ "<BR/><FONT POINT-SIZE=\"10\">" ++ de ++ "</FONT> >];") : makeDot ch True
makeDot ((Node id na _ _):ch) False = (id ++ " [shape=plaintext, label=<" ++ na ++ ">];") : makeDot ch True

makeDot ((Link par chi ""):ch) b = (par ++ " -> " ++ chi ++ ";") : makeDot ch b
makeDot ((Link par chi opt):ch) b = (par ++ " -> " ++ chi ++ " [" ++ opt ++ "];") : makeDot ch b
makeDot ((Cluster id True):ch) b = ("subgraph " ++ id ++ "{") : makeDot ch b
makeDot ((Cluster id False):ch) b = ("}") : makeDot ch b

graph f = do
         core <- compileToCore f
         let rbind = map toRBind core
         --putStrLn (show . length $ rbind)
         let ioGraphlist = map makeGraph rbind
         tree <- sequence ioGraphlist
         --putStrLn (show tree)
         let dot = map (\x -> makeDot x True)  tree --Set True/False for being "verbose"
         putStrLn $ "digraph G{"
         putStrLn $ "compound=true;"                --Needed for processing lhead and ltail options
         putStrLn $ unlines $ map unlines dot
         putStrLn $ "}"

main = graph "Add"
