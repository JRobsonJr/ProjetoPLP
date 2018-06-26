import System.IO
import Data.Char
import Data.Time.Clock

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

writePlayer :: String -> Int -> IO ()
writePlayer nickname score = do
    let player = joinWithCommas [nickname, show score]
    appendFile "../resources/players.csv" ("\n" ++ player)

writeWord :: String -> String -> IO ()
writeWord text theme = do
    let word = joinWithCommas $ map toUpper' [text, theme, (getLevel text)]
    appendFile "../resources/words.csv" ("\n" ++ word)
    where getLevel text
            | length text < 6  = "1"
            | length text < 10 = "2"
            | otherwise        = "3"

splitOnComma :: String -> [String]
splitOnComma s = splitOnComma' s ""
    where
        splitOnComma' [] w = [w]
        splitOnComma' (',':tail) w = [w] ++ splitOnComma' tail ""
        splitOnComma' (head:tail) w = splitOnComma' tail (w ++ [head])

joinWithCommas :: [String] -> String
joinWithCommas [] = []
joinWithCommas (head:[]) = head
joinWithCommas (head:tail) = head ++ "," ++ joinWithCommas tail

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
    theme <- selectTheme
    words <- filterByTheme theme
    randomWord <- getRandomWord words
    putStrLn (text randomWord)
    -- run game
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
leveledFastMatch = do
    level <- selectLevel
    words <- filterByLevel level
    randomWord <- getRandomWord words
    putStrLn (text randomWord)
    -- startGame

selectLevel :: IO Int
selectLevel = do
    showLevels
    level <- getOption

    if (level < 1 || level > 3) 
        then do
            showInvalidOptionMessage
            selectLevel
    else do
        return level


showLevels :: IO()
showLevels = do
    putStrLn "\n------------------------     SELECIONAR DIFICULDADE     ------------------------\n\n";
    putStrLn "                              1  -  Fácil"
    putStrLn "                              2  -  Médio"
    putStrLn "                              3  -  Difícil\n\n"


getCurrentTimestamp :: IO Int
getCurrentTimestamp = do
    currentTime <- getCurrentTime
    let currTimestamp = floor $ utctDayTime currentTime :: Int
    return currTimestamp

randomFastMatch :: IO()
randomFastMatch = do
    words <- setUpWords
    randomWord <- getRandomWord words
    putStrLn (text randomWord)
    -- startGame

getRandomWord  :: [Main.Word] -> IO Main.Word
getRandomWord words = do
    currTimestamp <- getCurrentTimestamp
    let index = currTimestamp `mod` (length words)
    let word = words !! index
    return word

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
    

getHint :: Int -> Main.Word -> [Char] -> IO Char
getHint tipsUsed word guesses =  do 
    currTimestamp <- getCurrentTimestamp
    let value = text word
    let index = currTimestamp `mod` (length value)
    let tip = value !! index

    if level word > tipsUsed then do 
        if tip `elem` guesses then do
                getHint tipsUsed word guesses
        else do
            return tip
    else do
        showTipLimitExceeded (level word)
        letter <- guessLetter tipsUsed word guesses
        return letter

getLetter :: IO Char
getLetter = do
    letter <- getChar
    _ <- getChar
    return letter

guessLetter :: Int -> Main.Word -> [Char] -> IO Char
guessLetter tipsUsed word guesses = do
    letter <- getLetter
    result <- guessLetter' tipsUsed word guesses letter
    return result

guessLetter' :: Int -> Main.Word -> [Char] -> Char -> IO Char
guessLetter' tipsUsed word guesses letter 
    | letter == '#' = do
        tip <- getHint tipsUsed word guesses
        return tip
    | isLetter letter && not(letter `elem` guesses) = do 
        return letter
    | not (isLetter letter) = do
        putStrLn "Uma letra, meu anjo..."
        guessLetter tipsUsed word guesses
    | otherwise = do
        putStrLn "Essa letra já foi sugerida. Tente outra!"
        guessLetter tipsUsed word guesses

toUpper' :: String -> String
toUpper' s = map toUpper s

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
    
    putStr "    No jogo da forca, o jogador deve acertar a palavra que lhe foi proposta a pa"
    putStr "rtir do número de letras dessa palavra e do tema ligado a ela. A cada rodada, o "
    putStr "jogador deve sugerir uma letra. Se a palavra proposta contiver a letra sugerida,"
    putStr " todas as ocorrências dessa letra são reveladas. Caso contrário, uma parte do en"
    putStr "forcado será desenhada. Para vencer, o jogador deve ser capaz de revelar a palav"
    putStrLn "ra por completo antes que todo o corpo do enforcado seja desenhado."
    
    putStr "    O jogador poderá se desafiar em dois modos distintos: o rápido e o campeonat"
    putStr "o. No modo rápido, o jogador enfrentará apenas uma palavra, podendo especificar "
    putStr "a dificuldade ou o tema dela, se assim desejar. Já no modo campeonato, o jogador"
    putStr "enfrentará diversas palavras em sequência e acumulará pontos a cada palavra reve"
    putStr "lada. Nesse modo, o jogo segue até que o jogador perca uma partida ou adivinhe "
    putStr "todas as palavras possíveis. Em qualquer um dos casos, seu desempenho será regis"
    putStr "trado no ranking.\n\n"
    
    putStrLn "                         [ Pressione ENTER para voltar ]\n\n"
    notImplementedYet

showTipLimitExceeded::Int -> IO()
showTipLimitExceeded level =  
    putStrLn $ "\n\n                    O limite de dicas para essa palavra é: " ++(show level)++ ".\n\n"

showVictoryMessage :: IO()
showVictoryMessage = do
    putStrLn "\n                    Parabéns, você acaba de salvar uma vida!\n"
    showVictoryHangman


showGameOverMessage :: IO()
showGameOverMessage = do
    putStrLn "\n                       É realmente uma pena, fim de jogo...\n"
    showHangman 0


revealWord :: Main.Word -> IO()
revealWord word = do
    putStrLn $ "\nA palavra era: "++ (text word) ++".\n\n"
    putStr "                         [ Pressione ENTER para voltar ]"
    
getSpaces :: Int -> String
getSpaces 0 = ""
getSpaces n = " " ++ getSpaces (n - 1)

getLengthSpacing :: Int -> Int -> String
getLengthSpacing length scoreLength = do
    if (scoreLength > 2) then getSpaces (length - (scoreLength - 2)) else getSpaces length
    
showPlayers :: [Player] -> Int -> String
showPlayers [] 10 = "                       " ++ show(10) ++ "º ------------       ---------\n"
showPlayers [] i = "                        " ++ show(i) ++ "º ------------       ---------\n" ++ showPlayers [] (i + 1)
showPlayers (head:tail) i = "                        " ++ show(i) ++ "º " ++ name head ++ getLengthSpacing (22 - length(name head)) (length (show(score head))) ++ show(score head) ++ "\n" ++ showPlayers tail (i + 1)

showRanking :: IO()
showRanking = do
    putStrLn "\n--------------------------------     RANKING     -------------------------------\n\n\n"
    putStrLn "                             Jogador          Pontuação\n\n"
    
    players <- setUpPlayers
    
    -- Ordenar os jogadores pela pontuacao
    
    putStrLn (showPlayers players 1)
    
    putStrLn "\n                         [ Pressione ENTER para voltar ]\n\n"
    
    -- Pause

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
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    showOpening
    showMenu
