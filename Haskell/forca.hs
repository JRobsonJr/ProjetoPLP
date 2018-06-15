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
    selectMenuOption (read option :: Int)
    
getOption :: IO String
getOption = do
    putStrLn "\n\n                    Informe o número da opção desejada: "
    option <- getLine
    return option

selectMenuOption :: Int -> IO()
selectMenuOption 1 = do showGameModes
selectMenuOption 2 = do showRules
selectMenuOption 3 = do showRanking
selectMenuOption 4 = do getWordData
selectMenuOption 5 = do quit
selectMenuOption n = do showInvalidOptionMessage

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