import System.IO
import Data.Char

-- esse nome da conflito com um bagulho q ja existe, mas n sei q nome botar
data Word = Word { 
    text :: String,
    theme :: String,
    level :: Int
} deriving (Show)

data Player = Player {
    name :: String,
    score:: Int
} deriving (Show)

setUpPlayers :: IO [Player]
setUpPlayers = do
    players <- readPlayers
    return $ createPlayerList players
    where readPlayers = do
              players <- openFile "../resources/players.csv" ReadMode
              contents <- hGetContents players
              return $ lines contents
          createPlayerList [] = []
          createPlayerList (head:tail) = [createPlayer $ splitOnComma head] ++ createPlayerList tail
              where createPlayer (name:score:_) = Player name (read score)

setUpWords :: IO [Main.Word]
setUpWords = do
    words <- readWords
    return $ createWordList words
    where readWords = do
              words <- openFile "../resources/words.csv" ReadMode
              contents <- hGetContents words
              return $ lines contents
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
    selectMenuOption option
    
getOption :: IO Int
getOption = do
    putStrLn "\n\n                    Informe o número da opção desejada: "
    option <- getLine
    return $ read option

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
    selectGameMode option

selectGameMode :: Int -> IO()
selectGameMode 1 = fastMatchMode
selectGameMode 2 = championshipMode
selectGameMode 3 = showMenu
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
    selectFastMatchType option

selectFastMatchType :: Int -> IO()
selectFastMatchType 1 = themedFastMatch
selectFastMatchType 2 = leveledFastMatch
selectFastMatchType 3 = randomFastMatch
selectFastMatchType 4 = showGameModes
selectFastMatchType n = showInvalidOptionMessage

themedFastMatch :: IO()
themedFastMatch = do
    selectTheme
    notImplementedYet

selectTheme :: IO String
selectTheme = do
    putStrLn "\n----------------------------     SELECIONAR TEMA     ---------------------------\n"
    showThemes
    themes <- getThemes
    option <- getOption
    if (option < length themes) 
        then return $ themes !! (option - 1)
        else do
            showInvalidOptionMessage
            selectTheme

showThemes :: IO()
showThemes = do
    themes <- getThemes
    putStrLn $ showThemes' themes 0
    where
        showThemes' [] index = []
        showThemes' (head:tail) index = spaces ++ show (index + 1) ++ "  -  " ++ head ++ "\n" ++ showThemes' tail (index + 1)
            where
                spaces = "                              "

leveledFastMatch :: IO()
leveledFastMatch = notImplementedYet

randomFastMatch :: IO()
randomFastMatch = notImplementedYet

championshipMode :: IO()
championshipMode = notImplementedYet

getHiddenWord :: String -> String
getHiddenWord [] = []
getHiddenWord (' ':tail) = [' '] ++ getHiddenWord tail
getHiddenWord (head:tail) = ['_'] ++ getHiddenWord tail

getScore :: Main.Word -> Int -> Int -> Int
getScore word 0 tipsUsed = 0
getScore word lives tipsUsed = wordLength * wordLevel * lives + 50 * wordLevel - 25 * tipsUsed
    where wordLength = length $ text word
          wordLevel = level word

showGuesses :: String -> String
showGuesses [] = []
showGuesses (head:[]) = [head]
showGuesses (head:tail) = [head] ++ [' '] ++ showGuesses tail

revealLetter :: Char -> String -> String -> String
revealLetter letter [] [] = []
revealLetter letter (head:tail) (head':tail')
    | letter == head = [letter] ++ revealLetter letter tail tail'
    | otherwise = [head'] ++ revealLetter letter tail tail'
    
getLetter :: IO Char
getLetter = do
    letter <- getChar
    getChar
    return letter

guessLetter' :: [Char] -> Char -> IO()
guessLetter' _ '#' = putChar 'A' -- getTip
guessLetter' guesses letter 
    | isLetter letter && not(letter `elem` guesses) = do 
        putChar letter
    | not (isLetter letter) = do
        putStrLn "Uma letra, meu anjo..."
        guessLetter guesses
    | otherwise = do
        putStrLn "Essa letra já foi sugerida. Tente outra!"
        guessLetter guesses

guessLetter :: [Char] -> IO()
guessLetter guesses = do
    letter <- getLetter
    guessLetter' guesses letter

toUpper' :: String -> String
toUpper' [] = []
toUpper' (head:tail) = [toUpper head] ++ toUpper' tail

showHangmanBody :: Int -> IO()
showHangmanBody lives
    | lives == 7 = do
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"
    
    | lives == 6 = do
        putStrLn "                                 #    ('-')    #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"

    | lives == 5 = do
        putStrLn "                                 #    ('-')__  #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"

    | lives == 4 = do
        putStrLn "                                 #  __('-')__  #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"

    | lives == 3 = do
        putStrLn "                                 #  __('-')__  #"
        putStrLn "                                 #      |      #"
        putStrLn "                                 #             #"
        putStrLn "                                 #             #"

    | lives == 2 = do
        putStrLn "                                 #  __('-')__  #"
        putStrLn "                                 #      |      #"
        putStrLn "                                 #     /       #"
        putStrLn "                                 #             #"

    | lives == 1 = do
        putStrLn "                                 #  __('-')__  #"
        putStrLn "                                 #      |      #"
        putStrLn "                                 #     / \\     #"
        putStrLn "                                 #             #"

    | otherwise = do
        putStrLn "                                 #      |      #"
        putStrLn "                                 #    (-.-)    #"
        putStrLn "                                 #     /|\\     #"
        putStrLn "                                 #     / \\     #"

showHangman :: Int -> IO()
showHangman lives = do
    putStrLn "                                 ###############"
    putStrLn "                                 #### FORCA ####"
    putStrLn "                                 ###############"
    putStrLn "                                 #      |      #"
    
    showHangmanBody lives
    
    putStrLn "                                 ###############"
    putStrLn "                                  /\\         /\\"
    putStrLn "                                 /  \\       /  \\ \n"

showVictoryHangman :: IO()
showVictoryHangman = do
    putStrLn "                                 ###############"
    putStrLn "                                 #### FORCA ####"
    putStrLn "                                 ###############"
    putStrLn "                                 #      |      #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 ###############     \\('◡')/"
    putStrLn "                                  /\\         /\\         |"
    putStrLn "                                 /  \\       /  \\       / \\ \n\n"

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
    -- showOpening
    -- showMenu
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    let lista = ['a'..'c']
    guessLetter lista
