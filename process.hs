import Data.Char
import Data.String
import Data.String.Utils

type Module = String
type Name = String
type Type = String

data SynTree =
 EmptyTree |
 Unary Name String Type SynTree |   --Suited for Lambdas, Function names
 Multi Name String Type [SynTree]   --Suited for Vars
 deriving (Show)

--Remove uninteded characters from end of string. Though also removes ] from [] in a list
clean :: String -> String
clean x
 | z == ']' = clean . init $ y
 | z == ',' = clean . init $ y
 | z == '#' = clean . init $ y
 | otherwise = y
 where
  y = rstrip x
  z = last y

--Create (hopefully) unique names using a global counter while generating dot syntax
naming :: String -> Int -> String
naming processed unique = "StartaksdfowqhraksDone" ++ (show unique) ++ "NAME" ++ (rstrip processed)

--Process symbols to replace with names for node names in dot
processName :: String -> String -> String -> String
processName nam _ "Func" = nam
processName x des "Var"
 | x == "+" = "Plus"
 | x == "-" = "Minus"
 | x == "*" = "Star"
 | x == "/" = "Divide"
 | x == ":" = "Cons"
 | x == "[" = "EmptyList"
 | x == "I" = des
 | otherwise = "Var" ++ x
processName x _ "Lam" = x

--Add shape types, proper labels for each node
describe :: SynTree -> String -> String
describe (Unary nam _ "Func" _) name = name ++ " [shape = plaintext, label =\"" ++ nam ++ "\"];"
describe (Unary nam des "Lam" _) name = name ++ " [shape = plaintext, label = <" ++ nam ++ "   " ++ "<FONT POINT-SIZE=\"10\">" ++ des ++ " </FONT> " ++ ">];"
describe (Multi nam des "Var" _) name = name ++ " [shape = plaintext, label = <" ++ nam ++ "   " ++ "<FONT POINT-SIZE=\"10\">"  ++ des ++ " </FONT>" ++ ">];"

--Join parent and child
connect :: String -> String -> String
connect parent child = parent ++ " -> " ++ child ++ ";"

--Create tree for each explicit function
resolve :: String -> Int -> SynTree -> [String]

resolve _ _ (EmptyTree) = []

resolve parent unique node@(Unary nam des typ child) =
 let
  name = naming (processName nam des typ) unique
  style = describe node name
  link = connect parent name
  rest = resolve name (unique + 1) child
 in style : (link : rest)

resolve parent unique node@(Multi nam des typ children) =
 let
  name = naming (processName nam des typ) unique
  style = describe node name
  link = connect parent name
  rest = foldr (++) [] $ map (resolve name (unique + 1)) children
 in style : (link : rest)

--Create dot syntax
makeDot :: (Module, [SynTree]) -> String
makeDot (mod, tree) =
 let list = map (unlines . resolve mod 0) tree in
 "digraph G {\n" ++ (unlines list) ++ "\n}"


--Count number of right applications and return the following content and count
separate :: [String] -> Int -> (Int, [String])
separate input@(first:remaining) count = 
 case lstrip first of
  "RApp" -> separate remaining (count + 1)
  _ -> (count, input)


--Update description (like type and dictionaries for +)
addDesc :: SynTree -> String -> SynTree
addDesc (Multi nam des typ chi) addinfo = Multi nam (unwords [addinfo, des]) typ chi





--Process Vars. Additional info is stored in immediate lines. Process those too
rVar :: [String] -> SynTree -> (SynTree, [String])
rVar [] tree = (tree, [])
rVar input@(first:remaining) EmptyTree = 
 case words first of
  ["RVar", x] -> rVar remaining (Multi (clean x) "" "Var" [])
rVar input@(first:remaining) tree=
 case words first of
  ["RLit", x] -> rVar remaining (addDesc tree $ "" ++ (clean x))
  ["RType", x] -> rVar remaining (addDesc tree $ "Type:" ++ x)
  ["RVar", '$':x] -> rVar remaining (addDesc tree $ "Dict:" ++ x)
  _ -> (tree, input)


--Number of descriptions added to Var (to find number of rApps used up)
varCallcount :: SynTree -> Int
varCallcount (Multi _ d _ _) = length . words $ d


--Parse the rApp part
rApp :: Int-> [String] -> SynTree -> (SynTree, [String])

rApp 0 x tree = (tree, x) --Handles x = [], i.e., end of program

rApp count input@(first:remaining) EmptyTree =
 case words first of
  ["RVar", x] -> let (subVartree, subVarrem) = rVar input EmptyTree in
   rApp (count - varCallcount subVartree) subVarrem subVartree

rApp count input@(first:remaining) tree@(Multi nam des typ chi) = 
 case words first of
  ["RNonRec"] -> (tree, input)
  ["RVar", x] -> rApp (count - 1) subVarrem (Multi nam des typ (subVartree:chi))
  ["RApp"] -> rApp (count - 1) subApprem (Multi nam des typ (chi ++ [subApptree]))
  "RLam":x -> rApp (count - 1) subLamrem (Multi nam des typ (subLamtree:chi))
  where
   (subLamtree, subLamrem) = rLam input
   (subVartree, subVarrem) = rVar input EmptyTree
   (interAppcou, interApprem) = separate remaining 1
   (subApptree, subApprem) = rApp interAppcou interApprem EmptyTree
   --(Unary prevname prevdes prevchi):list = chi

--FIX
--Does not handle in-line lambdas currently
rLam :: [String] -> (SynTree, [String])
rLam input@(first:remaining) =
 case words first of
  ("RLam" : (name : (_ : typ))) -> (Unary name (unwords typ) "Lam" child, remainder)
  ("RApp" : [])  -> rApp appCount appRem EmptyTree
  where
   (child, remainder) = rLam remaining
   (appCount, appRem) = separate remaining 1


makeTree :: [String] -> (SynTree, [String])
-- Create a syntax tree upto RNonRec
makeTree [] = (EmptyTree, [])
makeTree input@(first:remaining) =
 case head $ words first of
  "RNonRec" -> (EmptyTree, input)
  "RApp" -> rApp appCount afterBody EmptyTree
  "RLam" -> rLam input
  "RVar" -> rVar input EmptyTree
  name -> (Unary name "" "Func" (fst remTree), snd remTree)
  where
   (appCount, afterBody) = separate remaining 1
   remTree = makeTree remaining


getModuleName :: [String] -> String -> (String, [String])
-- Module name is present on the very last line, along with some additional characters like , ] #
getModuleName [] previous = (clean . last $ words previous, [])
getModuleName input@((_:"RNonRec"):_) previous = (clean . last $ words previous, input)
getModuleName (current:next) _ = getModuleName next current


genTM :: [String] -> (Either Module SynTree, [String])
-- genTM examines the immediate line and sends the control to either:
-- 1. getModuleName - to extract the name of the module or 
-- 2. makeTree - to make the syntax tree
genTM input@(first:_) =
 case lstrip first of
  "$trModule" -> (Left mod, remMod)
  _ -> (Right tre, remTre)
  where
   (tre, remTre) = makeTree input
   (mod, remMod) = getModuleName input ""


parseHelper :: [String] -> Module -> [SynTree] -> (Module, [SynTree])
-- On seeing a RNonRec, parse until the next such occurence or the end of remaining text
-- The parseHelper should always process only RNonRec. If it doesn't there's something wrong in the logic
parseHelper [] mod tree = (mod, tree)
parseHelper ((_:"RNonRec"):input)  mod tree =
 let y = genTM input
 in case y of
  (Right newTree, remaining) -> parseHelper remaining mod (newTree:tree)
  (Left newMod, remaining) -> parseHelper remaining newMod tree
parseHelper input _ _ = error "Unexpected input"


parse :: String -> String
parse contents = makeDot $ parseHelper (lines contents) "" []

parseAux :: String -> (String, [SynTree])
parseAux contents = parseHelper (lines contents) "" []


main = do
    contents <- getContents
    putStr (parse contents)
    --putStrLn (show $ parseAux contents)



