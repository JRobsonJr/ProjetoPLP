import System.IO

-- esse nome da conflito com um bagulho q ja existe, mas n sei q nome botar
data Word = Word { 
    text :: String,
    theme :: String,
    level :: Int
} deriving (Show)

setUpWords :: IO [Main.Word]
setUpWords = do
    words <- readWords
    return $ createWordList words

readWords :: IO [String]
readWords = do
    words <- openFile "words.csv" ReadMode
    contents <- hGetContents words
    return $ lines contents

createWordList :: [String] -> [Main.Word]
createWordList [] = []
createWordList (head:tail) = [createWord $ splitOnComma head] ++ createWordList tail

createWord :: [String] -> Main.Word
createWord (text:theme:level:_) = Word text theme (read level)

splitOnComma :: String -> [String]
splitOnComma s = split s ""

-- essa funcao podia ter um nome melhor
split :: String -> String -> [String]
split [] w = [w]
split (',':tail) w = [w] ++ split tail ""
split (head:tail) w = split tail (w ++ [head])

showOpening :: IO()
showOpening = do
    putStrLn "      ____________..___________                                                 "
    putStrLn "     | .___________))__________|                                                "
    putStrLn "     | | / /       ||                                                           "
    putStrLn "     | |/ /        ||                          _                                "
    putStrLn "     | | /         ||.-''.                    | |                               "
    putStrLn "     | |/          |/  _  \\                   | | ___   __ _  ___              "
    putStrLn "     | |           ||  `/,|               _   | |/ _ \\ / _` |/ _ \\            "
    putStrLn "     | |           (\\\\`_.'               | |__| | (_) | (_| | (_) |           "
    putStrLn "     | |          .-`--'.                 \\____/ \\___/ \\___ |\\___/          "
    putStrLn "     | |         /Y . . Y\\                              __/ |                  "
    putStrLn "     | |        // |   | \\\\                            |___/                  "
    putStrLn "     | |       //  | . |  \\\\                                                  "
    putStrLn "     | |      ')   | _ |   (`         _           ______                        "
    putStrLn "     | |           || ||             | |         |  ____|                       "
    putStrLn "     | |           || ||           __| | __ _    | |__ ___  _ __ ___ __ _       "
    putStrLn "     | |           || ||          / _` |/ _` |   |  __/ _ \\| '__/ __/ _` |     "
    putStrLn "     | |           || ||         | (_| | (_| |   | | | (_) | | | (_| (_| |      "
    putStrLn "     | |          / | | \\         \\____|\\____|   |_|  \\___/|_|  \\___\\____|"
    putStrLn "     | |          `-' `-'                                                       "
    putStrLn "     |_|                                                                        "
    putStrLn "                                   Aguarde...                                   "

showMenu :: IO()
showMenu = do
    putStrLn "\n---------------------------------     MENU     ---------------------------------\n\n"
    putStrLn "                                1  -  Jogar"
    putStrLn "                                2  -  Regras"
    putStrLn "                                3  -  Ranking"
    putStrLn "                                4  -  Nova Palavra"
    putStrLn "                                5  -  Sair"
    option <- getOption
    selectMenuOption (read option)
    
getOption :: IO String
getOption = do
    putStr "\n\n                    Informe o número da opção desejada: "
    option <- getLine
    return option

selectMenuOption :: Int -> IO()
selectMenuOption 1 = showGameModes
selectMenuOption 2 = showRules
selectMenuOption 3 = showRanking
selectMenuOption 4 = getWordData
selectMenuOption 5 = quit
selectMenuOption n = showInvalidOptionMessage

showInvalidOptionMessage :: IO()
showInvalidOptionMessage = do
    putStrLn "                       Opção inválida... Tente novamente!\n"

showGameModes :: IO()
showGameModes = do
    putStrLn "\n-----------------------------     MODO DE JOGO     -----------------------------\n\n"
    putStrLn "                                1  -  Jogo Rápido\n"
    putStrLn "                                2  -  Modo Campeonato\n"
    putStrLn "                                3  -  Voltar\n"

showRules :: IO()
showRules = do 
    putStrLn "\n--------------------------------     REGRAS     --------------------------------\n\n"
    putStrLn "REGRAS!"

showRanking :: IO()
showRanking = do
    putStrLn "\n--------------------------------     RANKING     -------------------------------\n\n\n"
    putStrLn "RANKING!"

getWordData :: IO()
getWordData = do
    putStrLn "\n---------------------------     CADASTRAR PALAVRA     --------------------------\n\n\n"
    putStrLn "CADASTRO!"

quit :: IO()
quit = do
    putStrLn "\n\n                                 Até a próxima!"
    putStrLn "\n\n             Paradigmas de Linguagem de Programação - 2018.1 - UFCG"
    putStrLn "\n\n                                DESENVOLVIDO POR:\n"
    putStrLn "                              Fanny Batista Vieira"
    putStrLn "                       José Robson da Silva Araujo Junior" 
    putStrLn "                            Matheus Alves dos Santos" 
    putStrLn "                         Misael Augusto Silva da Costa" 
    putStrLn "                            Paulo José Bastos Leitão\n\n" 

main :: IO()
main = do
    showOpening
    showMenu