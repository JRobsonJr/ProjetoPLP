#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

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

vector<Word> words;
// Utilizar Pair<int, Player> para o ranking. Vai poder aproveitar o comparador natural para ordenar.


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

void writeWords(){

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

void pause() {
    cin.ignore();
    getchar();
}

void clearScreen() {
    system("clear");
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

vector<Word> filterByLevel(int level) {
    vector<Word> filteredWords;

    for (int i = 0; i < words.size(); i++) {
        if (words[i].level == level) {
            filteredWords.push_back(words[i]);
        }
    }

    return filteredWords;
}

string toUpper(string word) {
    for (int i = 0; i < word.length(); i++) {
        word[i] = toupper(word[i]);
    }

    return word;
}

int getOption() {
    int option;

    cout << endl << endl;
    cout << "                    Informe o número da opção desejada: ";
    cin >> option;

    return option;
}

Word getRandomWord(vector<Word> words) {
    srand(time(NULL));
    int randomIndex = rand() % words.size();
    return words[randomIndex];
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

string revealLetter(char letter, string originalWord, string hiddenWord) {
    for (int i = 0; i < originalWord.length(); i++) {
        char c = originalWord[i];

        if (c == letter) {
            hiddenWord[i] = letter;
        }
    }

    return hiddenWord;
}

char guessLetter(string originalWord, string hiddenWord, vector<char> guesses) {
    char letter;
    cout << "Digite uma letra: ";
    cin >> letter;
    letter = toupper(letter);

    if (find(guesses.begin(), guesses.end(), letter) != guesses.end()) {
        cout << "Essa letra já foi dita anteriormente. Tente outra!" << endl;
        return guessLetter(originalWord, hiddenWord, guesses);
    } else if (!isalpha(letter)) {
        cout << "Uma letra, meu anjo." << endl;
        return guessLetter(originalWord, hiddenWord, guesses);
    }

    return letter;
}

void runGame(string originalWord, string hiddenWord, vector<char> guesses, int lives) {
    clearScreen();

    showHangman(lives);
    cout << endl << "Palavra: " << hiddenWord << endl;
    showGuesses(guesses);

    char letter = guessLetter(originalWord, hiddenWord, guesses);
    guesses.push_back(letter);


    string newHiddenWord = revealLetter(letter, originalWord, hiddenWord);

    // Essa comparação é para verificar se o jogador adivinhou alguma letra mesmo.
    if (hiddenWord.compare(newHiddenWord) == 0) {
        lives--;
    }

    if (newHiddenWord.compare(originalWord) == 0) {
        clearScreen();
        cout << "                    Parabéns, você acaba de salvar uma vida!" << endl << endl;
        showVictoryHangman();
        cout << endl << "A palavra era: " << originalWord << endl << endl << endl;
        cout << "                         [ Pressione ENTER para voltar ]" ;
        pause();
    } else if (lives > 0) {
        runGame(originalWord, newHiddenWord, guesses, lives);
    } else {
        clearScreen();
        showHangman(0);
        cout << endl << "Fim de jogo... A palavra era " << originalWord << "." << endl;
        pause();
    }
}

void startGame(Word word) {
    vector<char> guesses;
    int lives = 7;
    string originalWord = word.text;
    string hiddenWord = getHiddenWord(originalWord);
    runGame(originalWord, hiddenWord, guesses, lives);
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

void showLevels() {
    clearScreen();

    cout << endl;
    cout << "------------------------     SELECIONAR DIFICULDADE     ------------------------";
    cout << endl << endl;

    cout << "                              1  -  Fácil" << endl;
    cout << "                              2  -  Médio" << endl;
    cout << "                              3  -  Difícil" << endl;
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

void leveledFastMatch() {
    showLevels();

    int option = getOption();
    vector<Word> words = filterByLevel(option);
    Word randomWord = getRandomWord(words);

    startGame(randomWord);
}

void randomFastMatch() {
    Word randomWord = getRandomWord(words);
    startGame(randomWord);
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

void getPlayerData() {
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

    // CADASTRO DO JOGADOR

    // INICIAR PARTIDA
}

void selectGameMode(int option) {
    switch (option) {
        case 1:
            fastMatchMode();
            break;
        case 2:
            getPlayerData();
            break;
        case 3:
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

// Precisa implementação do Ranking
void showRanking() {
    clearScreen();

    cout << endl;
    cout << "--------------------------------     RANKING     -------------------------------";
    cout << endl << endl << endl;

    cout << "                         [ Pressione ENTER para voltar ]" << endl << endl;

    pause();
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
            quit();
            break;
        default:
            // INVÁLIDA
            break;
    }
}

void showMenu() {
    clearScreen();

    cout << endl;
    cout << "---------------------------------     MENU     ---------------------------------";
    cout << endl << endl;

    cout << "                                1  -  Jogar" << endl;
    cout << "                                2  -  Regras" << endl;
    cout << "                                3  -  Ranking" << endl;
    cout << "                                4  -  Sair" << endl;

    selectMenuOption(getOption());
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

int main() {

    setUpWords();

    showOpening();

    while(true) {
        showMenu();
    }

    for (int i = 0; i < words.size(); i++) {
        cout << words[i].text << " " << words[i].theme << " " << words[i].level << endl;
    }
    cout << endl;

    writeWords();

    quit();

}
