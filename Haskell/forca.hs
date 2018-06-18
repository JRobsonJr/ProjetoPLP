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
    where createWord (text:theme:level:_) = Word text theme (read level)

splitOnComma :: String -> [String]
splitOnComma s = splitOnComma' s ""
    where
        splitOnComma' [] w = [w]
        splitOnComma' (',':tail) w = [w] ++ splitOnComma' tail ""
        splitOnComma' (head:tail) w = splitOnComma' tail (w ++ [head])

getThemes :: IO [String]
getThemes = do
    words <- setUpWords
    return $ getThemes' words []
    where
        getThemes' [] result = result
        getThemes' (head:tail) result
            | (theme head) `elem` result = getThemes' tail result
            | otherwise = getThemes' tail (result ++ [theme head])

filterByTheme :: String -> IO [Main.Word]
filterByTheme t = do
    words <- setUpWords
    return $ filter (\word -> (theme word) == t) words

filterByLevel :: Int -> IO [Main.Word]
filterByLevel l = do
    words <- setUpWords
    return $ filter (\word -> (level word) == l) words

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
    selectMenuOption $ read option
    
getOption :: IO String
getOption = do
    putStrLn "\n\n                    Informe o número da opção desejada: "
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

notImplementedYet :: IO()
notImplementedYet = do
    putStrLn "                         Not implemented yet, meu anjo!\n"

showGameModes :: IO()
showGameModes = do
    putStrLn "\n-----------------------------     MODO DE JOGO     -----------------------------\n"
    putStrLn "                                1  -  Jogo Rápido"
    putStrLn "                                2  -  Modo Campeonato"
    putStrLn "                                3  -  Voltar"
    
    option <- getOption
    selectGameMode $ read option

selectGameMode :: Int -> IO()
selectGameMode 1 = fastMatchMode
selectGameMode 2 = championshipMode
selectGameMode 3 = notImplementedYet
selectGameMode n = showInvalidOptionMessage

fastMatchMode :: IO()
fastMatchMode = do
    putStrLn "\n-----------------------------     JOGO RÁPIDO     ------------------------------\n"
    putStrLn "                      Como sua palavra deve ser escolhida?\n"
    putStrLn "                              1  -  Por Tema"
    putStrLn "                              2  -  Por Dificuldade"
    putStrLn "                              3  -  Aleatoriamente"
    putStrLn "                              4  -  Voltar"

    option <- getOption
    selectFastMatchType $ read option

selectFastMatchType :: Int -> IO()
selectFastMatchType 1 = themedFastMatch
selectFastMatchType 2 = leveledFastMatch
selectFastMatchType 3 = randomFastMatch
selectFastMatchType 4 = notImplementedYet
selectFastMatchType n = showInvalidOptionMessage

themedFastMatch :: IO()
themedFastMatch = notImplementedYet

leveledFastMatch :: IO()
leveledFastMatch = notImplementedYet

randomFastMatch :: IO()
randomFastMatch = notImplementedYet

championshipMode :: IO()
championshipMode = notImplementedYet

getHiddenWord :: [Char] -> [Char]
getHiddenWord [] = []
getHiddenWord (' ':tail) = [' '] ++ getHiddenWord tail
getHiddenWord (head:tail) = ['_'] ++ getHiddenWord tail

getScore :: Main.Word -> Int -> Int -> Int
getScore word 0 tipsUsed = 0
getScore word lives tipsUsed = wordLength * wordLevel * lives + 50 * wordLevel - 25 * tipsUsed
    where wordLength = length $ text word
          wordLevel = level word

showGuesses :: [Char] -> [Char]
showGuesses [] = []
showGuesses (head:[]) = [head]
showGuesses (head:tail) = [head] ++ [' '] ++ showGuesses tail

revealLetter :: Char -> [Char] -> [Char] -> [Char]
revealLetter letter [] [] = []
revealLetter letter (head:tail) (head':tail')
    | letter == head = [letter] ++ revealLetter letter tail tail'
    | otherwise = [head'] ++ revealLetter letter tail tail'

showRules :: IO()
showRules = do 
    putStrLn "\n--------------------------------     REGRAS     --------------------------------\n\n"
    notImplementedYet

showRanking :: IO()
showRanking = do
    putStrLn "\n--------------------------------     RANKING     -------------------------------\n\n\n"
    notImplementedYet

getWordData :: IO()
getWordData = do
    putStrLn "\n---------------------------     CADASTRAR PALAVRA     --------------------------\n\n\n"
    notImplementedYet

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
    print $ revealLetter 'A' "SIA KATE" "_I_ __T_"