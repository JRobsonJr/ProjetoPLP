#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cstdlib>
#include <utility>

using namespace std;

struct Word {
    string text;
    string theme;
    int level;
};

struct Player {
    string name;
    int score;
};

const char DELIMITER = ',';
const string FILENAME_WORDS = "words.csv";
const string FILENAME_PLAYERS = "players.csv";
const char HELP_KEY = '#';

vector<Word> words;
vector<Player> players;
int tipsUsed;
// Utilizar Pair<int, Player> para o ranking. Vai poder aproveitar o comparador natural para ordenar.

bool isFileValid(ios &file);
void setUpWords();
void writeWords();

void pause();
void clearScreen();

void showOpening();
void showMenu();
int getOption();
void selectMenuOption(int option);

void showGameModes();
void selectGameMode(int option);

void fastMatchMode();
void selectFastMatchType(int option);

void themedFastMatch();
void showThemes();
vector<string> getThemes();
vector<Word> filterByTheme(string theme);

void leveledFastMatch();
void showLevels();
vector<Word> filterByLevel(int level);

void randomFastMatch();
Word getRandomWord(vector<Word> words);

void championshipMode();
vector<Word> getRandomOrderWords();
string getPlayerData();

int startGame(Word word);
string getHiddenWord(string word);
int getScore(Word word, int lives);

int runGame(Word originalWord, string hiddenWord, vector<char> guesses, int lives);
void showHangman(int lives);
void showVictoryHangman();
void showGuesses(vector<char> guesses);
char getTip(Word word, vector<char> guesses);
char guessLetter(Word originalWord, string hiddenWord, vector<char> guesses);
string revealLetter(char letter, string originalWord, string hiddenWord);
void showVictoryMessage();
void showGameOverMessage();
void showLimitTipExcedeed(int level);
void revealWord(string word);

void showRules();

string getSpaces(int length);
void showRanking();

void getWordData();
void registerNewPlayer(string nickname);
void registerNewWord(string text, string theme);
string toUpper(string word);

void quit();

bool isFileValid (ios &file) {
    if (!file.good()) {
        cerr << "Não foi possivel ler o arquivo." << endl;
        exit(1);
    }

    return true;
}

void setUpWords() {
    Word data;
    string currentLine;
    ifstream wordsFile(FILENAME_WORDS.c_str());

    if (isFileValid(wordsFile)) {
        while (getline(wordsFile, currentLine)) {
            stringstream ss(currentLine);

            getline(ss, data.text, DELIMITER);
            getline(ss, data.theme, DELIMITER);
            ss >> data.level;

            words.push_back(data);
        }

        wordsFile.close();
    }
}

void writeWords() {
    ofstream newWordsFile(FILENAME_WORDS.c_str());

    if (isFileValid(newWordsFile)) {
        for (int i = 0; i < words.size(); i++) {
            newWordsFile << words[i].text << DELIMITER
                         << words[i].theme << DELIMITER
                         << words[i].level << endl;
        }

        newWordsFile.close();
    }
}

void setUpPlayers() {
    Player player;
    string currentLine;
    ifstream playersFile(FILENAME_PLAYERS.c_str());

    if (isFileValid(playersFile)) {
        while (getline(playersFile, currentLine)) {
            stringstream ss(currentLine);

            getline(ss, player.name, DELIMITER);
            ss >> player.score;

            players.push_back(player);
        }

        playersFile.close();
    }
}

void writePlayers() {
    ofstream newPlayersFile(FILENAME_PLAYERS.c_str());

    if (isFileValid(newPlayersFile)) {
        for (int i = 0; i < players.size(); i++) {
            newPlayersFile << players[i].name << DELIMITER
                           << players[i].score << endl;
        }

        newPlayersFile.close();
    }
}

void pause() {
    cin.ignore();
    getchar();
}

void clearScreen() {
    system("clear");
}

void showOpening() {
    clearScreen();

    cout << "      ____________..___________                                                 " << endl;
    cout << "     | .___________))__________|                                                " << endl;
    cout << "     | | / /       ||                                                           " << endl;
    cout << "     | |/ /        ||                          _                                " << endl;
    cout << "     | | /         ||.-''.                    | |                               " << endl;
    cout << "     | |/          |/  _  \\                   | | ___   __ _  ___              " << endl;
    cout << "     | |           ||  `/,|               _   | |/ _ \\ / _` |/ _ \\            " << endl;
    cout << "     | |           (\\\\`_.'               | |__| | (_) | (_| | (_) |           " << endl;
    cout << "     | |          .-`--'.                 \\____/ \\___/ \\___ |\\___/          " << endl;
    cout << "     | |         /Y . . Y\\                              __/ |                  " << endl;
    cout << "     | |        // |   | \\\\                            |___/                  " << endl;
    cout << "     | |       //  | . |  \\\\                                                  " << endl;
    cout << "     | |      ')   | _ |   (`         _           ______                        " << endl;
    cout << "     | |           || ||             | |         |  ____|                       " << endl;
    cout << "     | |           || ||           __| | __ _    | |__ ___  _ __ ___ __ _       " << endl;
    cout << "     | |           || ||          / _` |/ _` |   |  __/ _ \\| '__/ __/ _` |     " << endl;
    cout << "     | |           || ||         | (_| | (_| |   | | | (_) | | | (_| (_| |      " << endl;
    cout << "     | |          / | | \\         \\____|\\____|   |_|  \\___/|_|  \\___\\____|" << endl;
    cout << "     | |          `-' `-'                                                       " << endl;
    cout << "     |_|                                                                        " << endl;
    cout << "                                   Aguarde...                                   " << endl;

   system("sleep 2s");
}

void showMenu() {
    clearScreen();

    cout << endl;
    cout << "---------------------------------     MENU     ---------------------------------";
    cout << endl << endl;

    cout << "                                1  -  Jogar" << endl;
    cout << "                                2  -  Regras" << endl;
    cout << "                                3  -  Ranking" << endl;
    cout << "                                4  -  Nova Palavra" << endl;
    cout << "                                5  -  Sair" << endl;

    selectMenuOption(getOption());
}

int getOption() {
    int option;

    cout << endl << endl;
    cout << "                    Informe o número da opção desejada: ";
    cin >> option;

    return option;
}

void selectMenuOption(int option) {
    switch (option) {
        case 1:
            showGameModes();
            break;
        case 2:
            showRules();
            break;
        case 3:
            showRanking();
            break;
        case 4:
            getWordData();
            break;
        case 5:
            quit();
            break;
        default:
            // INVÁLIDA
            break;
    }
}

void showGameModes()  {
    clearScreen();

    cout << endl;
    cout << "-----------------------------     MODO DE JOGO     -----------------------------";
    cout << endl << endl;

    cout << "                                1  -  Jogo Rápido" << endl;
    cout << "                                2  -  Modo Campeonato" << endl;
    cout << "                                3  -  Voltar" << endl;

    selectGameMode(getOption());
}

void selectGameMode(int option) {
    switch (option) {
        case 1:
            fastMatchMode();
            break;
        case 2:
            championshipMode();
            break;
        case 3:
            break;
        default:
            // INVÁLIDA
            break;
    }
}

void fastMatchMode()  {
    clearScreen();

    cout << endl;
    cout << "-----------------------------     JOGO RÁPIDO     ------------------------------";
    cout << endl << endl;

    cout << "                      Como sua palavra deve ser escolhida?" << endl << endl;

    cout << "                              1  -  Por Tema" << endl;
    cout << "                              2  -  Por Dificuldade" << endl;
    cout << "                              3  -  Aleatoriamente" << endl;
    cout << "                              4  -  Voltar" << endl;

    selectFastMatchType(getOption());
}

void selectFastMatchType(int option) {
    switch (option) {
        case 1:
            themedFastMatch();
            break;
        case 2:
            leveledFastMatch();
            break;
        case 3:
            randomFastMatch();
            break;
        case 4:
            break;
        default:
            // INVÁLIDA
            break;
    }
}

void themedFastMatch() {
    showThemes();

    vector<string> themes = getThemes();
    int option = getOption();
    string theme = themes[option - 1];
    vector<Word> words = filterByTheme(theme);
    Word randomWord = getRandomWord(words);

    startGame(randomWord);

}


void showThemes() {
    clearScreen();

    cout << endl;
    cout << "----------------------------     SELECIONAR TEMA     ---------------------------";
    cout << endl << endl;

    vector<string> themes = getThemes();

    for (int i = 0; i < themes.size(); i++) {
        cout << "                              " << i + 1 << "  -  " <<  themes[i] << endl;
    }
}

vector<string> getThemes() {
    vector<string> themes;

    for (int i = 0; i < words.size(); i++) {
        if (find(themes.begin(), themes.end(), words[i].theme) == themes.end()) {
            themes.push_back(words[i].theme);
        }
    }

    return themes;
}

vector<Word> filterByTheme(string theme) {
    vector<Word> filteredWords;

    for (int i = 0; i < words.size(); i++) {
        if (words[i].theme == theme) {
            filteredWords.push_back(words[i]);
        }
    }

    return filteredWords;
}

void leveledFastMatch() {
    showLevels();

    int option = getOption();
    vector<Word> words = filterByLevel(option);
    Word randomWord = getRandomWord(words);

    startGame(randomWord);
}

void showLevels() {
    clearScreen();

    cout << endl;
    cout << "------------------------     SELECIONAR DIFICULDADE     ------------------------";
    cout << endl << endl;

    cout << "                              1  -  Fácil" << endl;
    cout << "                              2  -  Médio" << endl;
    cout << "                              3  -  Difícil" << endl;
}

vector<Word> filterByLevel(int level) {
    vector<Word> filteredWords;

    for (int i = 0; i < words.size(); i++) {
        if (words[i].level == level) {
            filteredWords.push_back(words[i]);
        }
    }

    return filteredWords;
}

void randomFastMatch() {
    Word randomWord = getRandomWord(words);
    startGame(randomWord);
}

Word getRandomWord(vector<Word> words) {
    srand(time(NULL));
    int randomIndex = rand() % words.size();
    return words[randomIndex];
}

void setScoreChampionShipMode(string nickname, int score) {
    for (Player& player : players) {
        if (player.name == nickname) {
            player.score = (player.score < score)? score:player.score;
            break;
        }
    }

    writePlayers();
}

void championshipMode() {
    string nickname = getPlayerData();
    vector<Word> words = getRandomOrderWords();

    int index = 0;
    int highscore = 0;

    while (index < words.size()) {
        int score = startGame(words[index]);

        if (score > 0) {
            highscore += score;
        } else {
            break;
        }

        index++;
    }

    setScoreChampionShipMode(nickname, highscore);

    cout << endl << nickname << ", você jogou por " << index + 1 << " partida(s)." << endl;
}

vector<Word> getRandomOrderWords() {
    vector<Word> randomOrderWords;
    srand(unsigned(time(0)));

    for (int level = 1; level <= 3; level++) {
        vector<Word> currentLevelWords = filterByLevel(level);
        random_shuffle(currentLevelWords.begin(), currentLevelWords.end());
        randomOrderWords.insert(randomOrderWords.end(), currentLevelWords.begin(), currentLevelWords.end());
    }

    return randomOrderWords;
}

string getPlayerData() {
    clearScreen();

    string nickname;

    cout << endl;
    cout << "---------------------------     MODO CAMPEONATO     ----------------------------";
    cout << endl << endl;

    // Como capturar um ESC?
    cout << "                         (Pressione ESC para voltar...)";
    cout << endl << endl << endl;

    cout << "                              Insira o seu nick:" << endl << endl << endl;
    cout << "                                   ";
    cin >> nickname;

    registerNewPlayer(nickname);

    cout << endl << endl;
    cout << "                         Jogador cadastrado com sucesso!" << endl << endl;
    cout << "                                   Aguarde..." << endl << endl;

    system("sleep 1s");

    return nickname;
}

int startGame(Word word) {
    vector<char> guesses;

    int lives = 7;
    tipsUsed = 0;

    string hiddenWord = getHiddenWord(word.text);
    lives = runGame(word, hiddenWord, guesses, lives);

    return getScore(word, lives);
}

int getScore(Word word, int lives){
    int level = word.level;
    int lengthWord = word.text.size();
    int score = (lengthWord * level * lives) + (50 * level);

    return (lives == 0)? 0:score;
}

string getHiddenWord(string word) {
    string hiddenWord;

    for (int i = 0; i < word.length(); i++) {
        char c = word[i];

        if (isspace(c)) {
            hiddenWord += " ";
        } else {
            hiddenWord += "_";
        }
    }

    return hiddenWord;
}

int runGame(Word originalWord, string hiddenWord, vector<char> guesses, int lives) {
    clearScreen();

    showHangman(lives);
    cout << endl << "Palavra: " << hiddenWord << endl;
    showGuesses(guesses);

    char letter = guessLetter(originalWord, hiddenWord, guesses);
    string newHiddenWord = revealLetter(letter, originalWord.text, hiddenWord);

    //Se a string é vazia ele estorou o lim. de dicas.
    if(letter != '\0'){
        guesses.push_back(letter);
        if (hiddenWord.compare(newHiddenWord) == 0) {
            lives--;
        }
    }
    if (newHiddenWord.compare(originalWord.text) == 0) {
        showVictoryMessage();
        revealWord(originalWord.text);
        return lives;
    } else if (lives > 0) {
        return runGame(originalWord, newHiddenWord, guesses, lives);
    } else {
        showGameOverMessage();
        revealWord(originalWord.text);
        return lives;
    }
}

void showHangman(int lives) {

    cout << "                                 ###############" << endl;
    cout << "                                 #### FORCA ####" << endl;
    cout << "                                 ###############" << endl;
    cout << "                                 #      |      #" << endl;

    switch (lives) {

        case 7:
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            break;

        case 6:
            cout << "                                 #    ('-')    #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            break;

        case 5:
            cout << "                                 #    ('-')__  #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            break;

        case 4:
            cout << "                                 #  __('-')__  #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            break;

        case 3:
            cout << "                                 #  __('-')__  #" << endl;
            cout << "                                 #      |      #" << endl;
            cout << "                                 #             #" << endl;
            cout << "                                 #             #" << endl;
            break;

        case 2:
            cout << "                                 #  __('-')__  #" << endl;
            cout << "                                 #      |      #" << endl;
            cout << "                                 #     /       #" << endl;
            cout << "                                 #             #" << endl;
            break;

        case 1:
            cout << "                                 #  __('-')__  #" << endl;
            cout << "                                 #      |      #" << endl;
            cout << "                                 #     / \\     #" << endl;
            cout << "                                 #             #" << endl;
            break;

        default:
            cout << "                                 #      |      #" << endl;
            cout << "                                 #    (-.-)    #" << endl;
            cout << "                                 #     /|\\     #" << endl;
            cout << "                                 #     / \\     #" << endl;
            break;

    }

    cout << "                                 ###############" << endl;
    cout << "                                  /\\         /\\" << endl;
    cout << "                                 /  \\       /  \\" << endl;
}

void showVictoryHangman() {
    cout << "                                 ###############" << endl;
    cout << "                                 #### FORCA ####" << endl;
    cout << "                                 ###############" << endl;
    cout << "                                 #      |      #" << endl;
    cout << "                                 #             #" << endl;
    cout << "                                 #             #" << endl;
    cout << "                                 #             #" << endl;
    cout << "                                 #             #" << endl;
    cout << "                                 ###############     \\('◡')/" << endl;
    cout << "                                  /\\         /\\         |" << endl;
    cout << "                                 /  \\       /  \\       / \\ " << endl << endl;
}

void showGuesses(vector<char> guesses) {
    if (guesses.size() > 0) {
        cout << "Letras já usadas: ";

        for (int i = 0; i < guesses.size(); i++) {
            cout << guesses[i] << " ";
        }

        cout << endl;
    }
}


char getTip(Word word, vector<char> guesses){

    int random_index = rand() % word.text.size();

    string originalWord = word.text;
    char letter =  originalWord[random_index];

    if(tipsUsed < word.level){
        if(find(guesses.begin(), guesses.end(), letter) != guesses.end()){
            letter = getTip(word, guesses);
        }
    }else{
        showLimitTipExcedeed(word.level);
        letter = '\0';
    }
    return letter;
}

char guessLetter(Word originalWord, string hiddenWord, vector<char> guesses) {
    char letter;
    cout << "Digite uma letra ou # para dica: ";
    cin >> letter;

    letter = toupper(letter);
    if(letter != HELP_KEY){
        if (find(guesses.begin(), guesses.end(), letter) != guesses.end()) {
            cout << "Essa letra já foi dita anteriormente. Tente outra!" << endl;
            return guessLetter(originalWord, hiddenWord, guesses);
        } else if (!isalpha(letter)) {
            cout << "Uma letra, meu anjo." << endl;
            return guessLetter(originalWord, hiddenWord, guesses);
        }
    }else{
        letter = getTip(originalWord, guesses);
        tipsUsed += 1;
    }

    return letter;
}

string revealLetter(char letter, string originalWord, string hiddenWord) {
    for (int i = 0; i < originalWord.length(); i++) {
        char c = originalWord[i];

        if (c == letter) {
            hiddenWord[i] = letter;
        }
    }

    return hiddenWord;
}

void showVictoryMessage() {
    clearScreen();
    cout << "                    Parabéns, você acaba de salvar uma vida!" << endl << endl;
    showVictoryHangman();
}

void showGameOverMessage() {
    clearScreen();
    cout << endl << "                       É realmente uma pena, fim de jogo..." << endl << endl;
    showHangman(0);
}

void showLimitTipExcedeed(int level) {
    cout << endl << "O limite de dicas para essa palavra e: " << level << "." << endl << endl << endl;
    cout << "                         [ Pressione ENTER para voltar ]" ;
    pause();
}


void revealWord(string word) {
    cout << endl << "A palavra era: " << word << "." << endl << endl << endl;
    cout << "                         [ Pressione ENTER para voltar ]" ;
    pause();
}


void showRules() {
    clearScreen();

    cout << endl;
    cout << "--------------------------------     REGRAS     --------------------------------";
    cout << endl << endl;

    cout << "    No jogo da forca, o jogador deve acertar a palavra que lhe foi proposta a pa"
         << "rtir do número de letras dessa palavra e do tema ligado a ela. A cada rodada, o "
         << "jogador deve sugerir uma letra. Se a palavra proposta contiver a letra sugerida,"
         << " todas as ocorrências dessa letra são reveladas. Caso contrário, uma parte do en"
         << "forcado será desenhada. Para vencer, o jogador deve ser capaz de revelar a palav"
         << "ra por completo antes que todo o corpo do enforcado seja desenhado." << endl;
    cout << "    O jogador poderá se desafiar em dois modos distintos: o rápido e o campeonat"
         << "o. No modo rápido, o jogador enfrentará apenas uma palavra, podendo especificar "
         << "a dificuldade ou o tema dela, se assim desejar. Já no modo campeonato, o jogador"
         << " enfrentará diversas palavras em sequência e acumulará pontos a cada palavra rev"
         << "elada. Nesse modo, o jogo segue até que o jogador perca uma partida ou adivinhe "
         << "todas as palavras possíveis. Em qualquer um dos casos, seu desempenho será regis"
         << "trado no ranking." << endl << endl << endl;

         cout << "                         [ Pressione ENTER para voltar ]" << endl << endl;

    pause();

}

string getSpaces(int length) {
    string spaces = "";

    for (int j = 0; j < length; j++) {
        spaces += " ";
    }

    return spaces;
}

void showRanking() {
    clearScreen();

    cout << endl;
    cout << "--------------------------------     RANKING     -------------------------------";
    cout << endl << endl << endl;
    cout << "                             Jogador          Pontuação" << endl << endl;


    for (int i = 0; i < 9; i++) {
        if (i < players.size()) {
            string spaces = getSpaces(22 - players[i].name.size());

            cout << "                        " << i + 1 << "º " << players[i].name << spaces << players[i].score << endl;
        } else {
            cout << "                        " << i + 1 << "º ------------       --------" << endl;
        }

    }

    if (9 < players.size()) {
        string spaces = getSpaces(22 - players[9].name.size());

        cout << "                        10º " << players[9].name << spaces << players[9].score << endl;
    } else {
        cout << "                       10º ------------       --------" << endl;
    }

    cout << endl;
    cout << "                         [ Pressione ENTER para voltar ]" << endl << endl;

    pause();
}

void getWordData() {
    clearScreen();

    string text, theme;

    cout << endl;
    cout << "---------------------------     CADASTRAR PALAVRA     --------------------------";

    cout << endl << endl;
    cout << "          Informe a nova palavra: ";
    cin.ignore();
    getline(cin, text);

    cout << endl;
    cout << "          Informe o tema da palavra: ";
    getline(cin, theme);

    registerNewWord(text, theme);

    cout << endl << endl;
    cout << "                         Palavra cadastrada com sucesso!" << endl << endl;
    cout << "                                   Aguarde..." << endl << endl;

    system("sleep 1s");
}

bool validNickName(string nickname) {
    for (Player& player : players) {
        if (player.name == nickname) {
            return true;
        }
    }

    return false;
}

void registerNewPlayer(string nickname) {
   if (validNickName(nickname)) {
        Player newPlayer;

        newPlayer.name = nickname;
        newPlayer.score = 0;

        players.push_back(newPlayer);

        writePlayers();
    } else {
        getPlayerData();
    }
}

void registerNewWord(string text, string theme) {
    Word newWord;

    newWord.text = toUpper(text);
    newWord.theme = toUpper(theme);

    if (text.length() < 6) {
        newWord.level = 1;
    } else if (text.length() < 10) {
        newWord.level = 2;
    } else {
        newWord.level = 3;
    }

    words.push_back(newWord);
    writeWords();
}

string toUpper(string word) {
    for (int i = 0; i < word.length(); i++) {
        word[i] = toupper(word[i]);
    }

    return word;
}

void quit() {
    clearScreen();

    cout << endl << endl;

    cout << "                                 Até a próxima!";
    cout << endl << endl << endl;

    cout << "             Paradigmas de Linguagem de Programação - 2018.1 - UFCG";
    cout << endl << endl << endl;

    cout << "                                DESENVOLVIDO POR:";
    cout << endl << endl;

    cout << "                              Fanny Batista Vieira" << endl;
    cout << "                            Matheus Alves dos Santos" << endl;
    cout << "                         Misael Augusto Silva da Costa" << endl;
    cout << "                            Paulo José Bastos Leitão" << endl;
    cout << "                       José Robson da Silva Araujo Junior" << endl;
    cout << endl << endl;


    system("sleep 3s");
    exit(1);
}

int main() {
    setUpWords();

    showOpening();

    while (true) {
        showMenu();
    }

    for (int i = 0; i < words.size(); i++) {
        cout << words[i].text << " " << words[i].theme << " " << words[i].level << endl;
    }

    cout << endl;

    writeWords();

    quit();
}
