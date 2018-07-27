import System.IO
import Data.Char
import Data.List
import Data.Ord
import Data.Time.Clock.POSIX
import qualified System.Process
import Control.Monad

data Word = Word { 
    text :: String,
    theme :: String,
    level :: Int
} deriving (Eq, Show)

data Player = Player {
    name :: String,
    score:: Int
} deriving (Ord, Eq, Show)

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

isAlreadyRegistered :: String -> String -> IO Bool
isAlreadyRegistered text theme = do
    words <- setUpWords
    return $ isAlreadyRegistered' words text theme

isAlreadyRegistered' :: [Main.Word] -> String -> String -> Bool
isAlreadyRegistered' [] _ _ = False
isAlreadyRegistered' (head:tail) newText newTheme
    | isSameWord && isSameTheme = True
    | otherwise = isAlreadyRegistered' tail newText newTheme
    where isSameWord = (text head) == (toUpper' newText)
          isSameTheme = (theme head) == (toUpper' newTheme)

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

clearScreen :: IO()
clearScreen = do
    _ <- System.Process.system "clear"
    return ()

sleep3s :: IO()
sleep3s = do
    _ <- System.Process.system "sleep 3s"
    return ()

pause :: IO ()
pause = do
    x <- getChar
    putStrLn ""

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
    sleep3s

showMenu :: IO()
showMenu = do
    clearScreen
    putStrLn "\n---------------------------------     MENU     ---------------------------------\n\n"
    putStrLn "                                1  -  Jogar"
    putStrLn "                                2  -  Regras"
    putStrLn "                                3  -  Ranking"
    putStrLn "                                4  -  Nova Palavra"
    putStrLn "                                5  -  Sair"
    option <- getOption
    selectMenuOption option
    clearScreen
    when (not $ option == 5) $ do clearScreen; showMenu
    
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
    putStrLn "           Opção inválida... Pressione ENTER para tentar novamente!\n"
    pause

showGameModes :: IO()
showGameModes = do
    clearScreen
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
    clearScreen
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
    score <- startGame randomWord
    putStrLn (show score)

selectTheme :: IO String
selectTheme = do
    clearScreen
    putStrLn "\n----------------------------     SELECIONAR TEMA     ---------------------------\n"
    showThemes
    themes <- getThemes
    option <- getOption
    if (option > 0 && option <= length themes) 
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
            where spaces = "                              "

getRandomWord :: [Main.Word] -> Main.Word
getRandomWord words = words !! 0 --notImplementedYet

startGame :: Main.Word -> IO()
startGame word = do
	print hiddenWord
		where hiddenWord = getHiddenWord $ text word
	runGame word hiddenWord [] 7

runGame :: Main.Word -> String -> [Char] -> Int -> IO()
runGame originalWord hiddenWord guesses lives = do
	--showHangman lives
	putStrLn $ "Tema: " ++ theme originalWord 
	putStrLn $ "Palavra: " ++ text originalWord
	print $ showGuesses guesses
	notImplementedYet

leveledFastMatch :: IO()
leveledFastMatch = do
    level <- selectLevel
    words <- filterByLevel level
    randomWord <- getRandomWord words
    score <- startGame randomWord
    putStrLn (show score)

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
    clearScreen
    putStrLn "\n------------------------     SELECIONAR DIFICULDADE     ------------------------\n\n";
    putStrLn "                              1  -  Fácil"
    putStrLn "                              2  -  Médio"
    putStrLn "                              3  -  Difícil\n\n"

randomFastMatch :: IO()
randomFastMatch = do
    words <- setUpWords
    randomWord <- getRandomWord words
    score <- startGame randomWord
    putStrLn (show score)
    
getCurrentTimestamp :: IO Int
getCurrentTimestamp = do
    currentTime <- getPOSIXTime
    let currTimestamp = floor $ currentTime * 100000
    return currTimestamp

getRandomWord  :: [Main.Word] -> IO Main.Word
getRandomWord words = do
    currTimestamp <- getCurrentTimestamp
    let index = currTimestamp `mod` (length words)
    let word = words !! index
    return word

goBackChampionship :: String -> Bool
goBackChampionship nickname = (length nickname) == 1 && nickname == "#";

championshipMode :: IO()
championshipMode = do
    nickname <- getPlayerData
    
    if not (goBackChampionship nickname) then do
        words <- getRandomOrderWords 1 []
        score <- championshipMode' 1 1 words
        writePlayer nickname score
        sleep3s
        clearScreen
        showChampionshipScore score

    else showMenu

championshipMode':: Int-> Int -> [Main.Word] -> IO Int
championshipMode' score totalScore (head:tail)
    | (score == 0) || ((length tail) == 0) = return totalScore
    | otherwise = do
        let total = totalScore + score
        score <- startGame(head)
        championshipMode' score total tail

getRandomOrderWords :: Int -> [Main.Word]-> IO [Main.Word]
getRandomOrderWords 4 randomOrderWords = return randomOrderWords 
getRandomOrderWords level randomOrderWords = do
    currentLevelWords <- filterByLevel level
    shuffleList <- getRandomOrderWords' [] currentLevelWords
    getRandomOrderWords (level+1) (randomOrderWords++shuffleList)
    
getRandomOrderWords' :: [Main.Word] -> [Main.Word] -> IO [Main.Word]
getRandomOrderWords' randomOrderWords currentLevelWords = do
    if (length randomOrderWords) < (length currentLevelWords) then do 
        randomOrderWord <- getRandomOrderWord randomOrderWords currentLevelWords
        getRandomOrderWords' (randomOrderWords++[randomOrderWord]) currentLevelWords
    else
        return randomOrderWords

getRandomOrderWord :: [Main.Word] -> [Main.Word] -> IO Main.Word
getRandomOrderWord randomOrderWords currentLevelWords = do 
    word <- getRandomWord currentLevelWords
    if word `elem` randomOrderWords then
        getRandomOrderWord randomOrderWords currentLevelWords
    else
        return word

getPlayerData :: IO String
getPlayerData = do
    clearScreen
    putStrLn "\n---------------------------     MODO CAMPEONATO     ----------------------------\n\n"
    putStrLn "                         (Insira # para voltar...)\n\n\n"
    putStr "                              Insira o seu nick: "
    
    nickname <- getLine
    
    if not (goBackChampionship nickname)
    then putStrLn "\n\n                         Jogador cadastrado com sucesso!"
    else putStrLn ""
    
    putStrLn "                                   Aguarde...\n\n"
    
    sleep3s
    return nickname
    
startGame :: Main.Word -> IO Int
startGame word = do
    let hiddenWord = getHiddenWord $ text word
    (lives, hintsUsed) <- runGame word hiddenWord [] 7 0
    return $ getScore word lives hintsUsed

runGame :: Main.Word -> String -> [Char] -> Int -> Int -> IO (Int, Int)
runGame originalWord hiddenWord guesses lives hintsUsed = do
    showHangman lives
    putStrLn $ "Tema: " ++ theme originalWord
    putStrLn $ "Palavra: " ++ hiddenWord
    putStrLn $ "Letras já usadas: " ++ showGuesses guesses
    putStrLn $ "Dicas usadas: " ++ show hintsUsed

    (letter, hintsUsed') <- guessLetter hintsUsed originalWord guesses
    let hiddenWord' = revealLetter letter (text originalWord) hiddenWord
    let guesses' = guesses ++ [letter]
    let lives' = getLives hiddenWord hiddenWord' lives

    if hiddenWord' == text originalWord then do
        showVictoryMessage
        revealWord originalWord
        return (lives', hintsUsed')
    else if lives' > 0 then do
        runGame originalWord hiddenWord' guesses' lives' hintsUsed'
    else do
        showGameOverMessage
        revealWord originalWord
        return (0, hintsUsed')

getLives :: String -> String -> Int -> Int
getLives word word' currentLives
    | word == word' = currentLives - 1
    | otherwise = currentLives

getHiddenWord :: String -> String
getHiddenWord [] = []
getHiddenWord (' ':tail) = [' '] ++ getHiddenWord tail
getHiddenWord (head:tail) = ['_'] ++ getHiddenWord tail

getScore :: Main.Word -> Int -> Int -> Int
getScore word 0 hintsUsed = 0
getScore word lives hintsUsed = wordLength * wordLevel * lives + 50 * wordLevel - 25 * hintsUsed
    where wordLength = length $ text word
          wordLevel = level word

showGuesses :: [Char] -> String
showGuesses [] = []
showGuesses (head:[]) = [head]
showGuesses (head:tail) = [head] ++ [' '] ++ showGuesses tail

revealLetter :: Char -> String -> String -> String
revealLetter letter [] [] = []
revealLetter letter (head:tail) (head':tail')
    | letter == head = [letter] ++ revealLetter letter tail tail'
    | otherwise = [head'] ++ revealLetter letter tail tail'
    
getHint :: Int -> Main.Word -> [Char] -> IO (Char, Int)
getHint hintsUsed word guesses =  do 
    currTimestamp <- getCurrentTimestamp
    let value = text word
    let index = currTimestamp `mod` (length value)
    let hint = value !! index

    if level word > hintsUsed then do 
        if hint `elem` guesses || hint == ' ' then do
                getHint hintsUsed word guesses
        else do
            return (hint, hintsUsed + 1)
    else do
        showHintLimitExceeded (level word)
        (letter, hintsUsed') <- guessLetter hintsUsed word guesses
        return (letter, hintsUsed')

getLetter :: IO Char
getLetter = do
    letter <- getChar
    _ <- getChar
    return (toUpper letter)
    
toUpper' :: String -> String
toUpper' s = map toUpper s

guessLetter :: Int -> Main.Word -> [Char] -> IO (Char, Int)
guessLetter hintsUsed word guesses = do
    putStrLn "\nDigite uma letra ou # para dica: "
    letter <- getLetter
    (letter, hintsUsed') <- guessLetter' hintsUsed word guesses letter
    return (letter, hintsUsed')

guessLetter' :: Int -> Main.Word -> [Char] -> Char -> IO (Char, Int)
guessLetter' hintsUsed word guesses letter 
    | letter == '#' = do
        (hint, hintsUsed') <- getHint hintsUsed word guesses
        return (hint, hintsUsed')
    | isLetter letter && not(letter `elem` guesses) = do 
        return (letter, hintsUsed)
    | not (isLetter letter) = do
        putStrLn "Uma letra, meu anjo..."
        guessLetter hintsUsed word guesses
    | otherwise = do
        putStrLn "Essa letra já foi sugerida. Tente outra!"
        guessLetter hintsUsed word guesses

showHangman :: Int -> IO()
showHangman lives = do
    clearScreen
    putStrLn "                                 ###############"
    putStrLn "                                 #### FORCA ####"
    putStrLn "                                 ###############"
    putStrLn "                                 #      |      #"
    
    showHangmanBody lives
    
    putStrLn "                                 ###############"
    putStrLn "                                  /\\         /\\"
    putStrLn "                                 /  \\       /  \\ \n"

showHangmanBody :: Int -> IO()
showHangmanBody 7 = do
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"

showHangmanBody 6 = do
    putStrLn "                                 #    ('-')    #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"

showHangmanBody 5 = do
    putStrLn "                                 #    ('-')__  #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"

showHangmanBody 4 = do
    putStrLn "                                 #  __('-')__  #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"

showHangmanBody 3 = do
    putStrLn "                                 #  __('-')__  #"
    putStrLn "                                 #      |      #"
    putStrLn "                                 #             #"
    putStrLn "                                 #             #"

showHangmanBody 2 = do
    putStrLn "                                 #  __('-')__  #"
    putStrLn "                                 #      |      #"
    putStrLn "                                 #     /       #"
    putStrLn "                                 #             #"

showHangmanBody 1 = do
    putStrLn "                                 #  __('-')__  #"
    putStrLn "                                 #      |      #"
    putStrLn "                                 #     / \\     #"
    putStrLn "                                 #             #"
    
showHangmanBody 0 = do
    putStrLn "                                 #      |      #"
    putStrLn "                                 #    (-.-)    #"
    putStrLn "                                 #     /|\\     #"
    putStrLn "                                 #     / \\     #"
    
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

showDefeatHangman :: IO()
showDefeatHangman = do
    putStrLn "                                 ###############"
    putStrLn "                                 #### FORCA ####"
    putStrLn "                                 ###############"
    putStrLn "                                 #      |      #"
    putStrLn "                                 #      |      #"
    putStrLn "                                 #    (-.-)    #"
    putStrLn "                                 #     /|\\     #"
    putStrLn "                                 #     / \\     #"
    putStrLn "                                 ###############"
    putStrLn "                                  /\\         /\\"
    putStrLn "                                 /  \\       /  \\ \n"

showRules :: IO()
showRules = do 
    clearScreen
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
    _ <- getLine
    return ()

showHintLimitExceeded::Int -> IO()
showHintLimitExceeded level =  
    putStrLn $ "\n                    O limite de dicas para essa palavra é: " ++(show level)++ ".\n"

showVictoryMessage :: IO()
showVictoryMessage = do
    clearScreen
    putStrLn "                     Parabéns, você acaba de salvar uma vida!\n\n"
    showVictoryHangman

showGameOverMessage :: IO()
showGameOverMessage = do
    clearScreen
    putStrLn "                       É realmente uma pena, fim de jogo...\n\n"
    showDefeatHangman

showChampionshipScore :: Int -> IO ()
showChampionshipScore totalScore = do    
    putStrLn $ "Você fez " ++ (show totalScore) ++ " pontos no total.\n\n"
    putStrLn "                     [ Pressione ENTER para voltar ao jogo ]\n\n"
    _ <- getChar 
    return ()

revealWord :: Main.Word -> IO()
revealWord word = do
    putStrLn $ "\nA palavra era: "++ (text word) ++".\n\n"
    putStr "                         [ Pressione ENTER para voltar ]"
    _ <- getLine
    return ()
    
getSpaces :: Int -> String
getSpaces 0 = ""
getSpaces n = " " ++ getSpaces (n - 1)

getLengthSpacing :: Int -> Int -> String
getLengthSpacing length scoreLength = do
    if (scoreLength > 2) then getSpaces (length - (scoreLength - 2)) else getSpaces length
    
showPlayers :: [Player] -> Int -> String
showPlayers [] 11 = ""
showPlayers [] 10 = "                       10º ------------       ---------\n"
showPlayers [] i = "                        " ++ show i ++ "º ------------       ---------\n" 
                   ++ showPlayers [] (i + 1)
showPlayers (head:tail) i = "                        " ++ show i ++ "º " ++ name head
                            ++ getLengthSpacing (23 - length (name head)) (length (show(score head)))
                            ++ show (score head) ++ "\n" ++ showPlayers tail (i + 1)

sortByScore :: [Player] -> [Player]
sortByScore = sortBy $ flip $ comparing score

showRanking :: IO()
showRanking = do
    clearScreen
    putStrLn "\n--------------------------------     RANKING     -------------------------------\n\n\n"
    putStrLn "                             Jogador          Pontuação\n"
    
    players <- setUpPlayers
    
    putStrLn (showPlayers (sortByScore players) 1)
    
    putStrLn "\n                         [ Pressione ENTER para voltar ]\n\n"
    _ <- getLine
    return()

getWordData :: IO()
getWordData = do
    clearScreen
    putStrLn "\n---------------------------     CADASTRAR PALAVRA     --------------------------\n\n\n"
    putStr "          Informe a nova palavra: "
    text <- getLine

    putStrLn "\n                              Temas já cadastrados:"
    showThemes

    putStr "\n          Informe o tema da palavra (por extenso): "
    theme <- getLine

    registered <- isAlreadyRegistered text theme
    if not registered
        then getWordDataSuccess text theme
        else getWordDataFailure

getWordDataSuccess :: String -> String -> IO()
getWordDataSuccess text theme = do
    writeWord text theme
    putStrLn "\n\n                         Palavra cadastrada com sucesso!\n"
    putStrLn "                                   Aguarde...\n\n"
    sleep3s

getWordDataFailure :: IO()
getWordDataFailure = do
    putStrLn "\n\n                       Opa, essa palavra já foi cadastrada...\n"
    putStrLn "                      [ Pressione ENTER para tentar novamente ]\n\n"
    _ <- getLine
    getWordData

quit :: IO()
quit = do
    clearScreen
    putStrLn "\n\n                                 Até a próxima!"
    putStrLn "\n\n             Paradigmas de Linguagem de Programação - 2018.1 - UFCG"
    putStrLn "\n\n                                DESENVOLVIDO POR:\n"
    putStrLn "                              Fanny Batista Vieira"
    putStrLn "                       José Robson da Silva Araujo Junior" 
    putStrLn "                            Matheus Alves dos Santos" 
    putStrLn "                         Misael Augusto Silva da Costa" 
    putStrLn "                            Paulo José Bastos Leitão\n\n"
    sleep3s

main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    showOpening
    showMenu
