#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

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
    system("sleep 5s");
}



int getOption() {
    int option;

    cout << endl << endl;
    cout << "                    Informe o número da opção desejada: ";
    cin >> option;

    return option;
}

void showThemes() {
    system("clear");

    cout << endl;
    cout << "----------------------------     SELECIONAR TEMA     ---------------------------";
    cout << endl << endl;

    // LISTAR TEMAS

    // INICIAR PARTIDA

    pause();
}

void showLevels() {
    system("clear");

    cout << endl;
    cout << "------------------------     SELECIONAR DIFICULDADE     ------------------------";
    cout << endl << endl;

    // LISTAR DIFICULDADES

    // INICIAR PARTIDA

    pause();
}

void selectFastMatchType(int option) {
    switch (option) {
        case 1:
            showThemes();
            break;
        case 2:
            showLevels();
            break;
        case 3:
            // INICIAR PARTIDA
            break;
        case 4:
            break;
        default:
            // INVÁLIDA
            break;
    }
}

void fastMatchType()  {
    system("clear");

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
    system("clear");

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
            fastMatchType();
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
    system("clear");

    cout << endl;
    cout << "-----------------------------     MODO DE JOGO     -----------------------------";
    cout << endl << endl;

    cout << "                                1  -  Jogo Rápido" << endl;
    cout << "                                2  -  Modo Campeonato" << endl;
    cout << "                                3  -  Voltar" << endl;

    selectGameMode(getOption());
}

void showRules() {
    system("clear");

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
         << "trado no ranking." << endl << endl;

    pause();

}

// Precisa implementação do Ranking
void showRanking() {
    system("clear");

    cout << endl;
    cout << "--------------------------------     RANKING     -------------------------------";
    cout << endl << endl;

    pause();
}

void quit() {
    system("clear");

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
    system("clear");

    cout << endl;
    cout << "---------------------------------     MENU     ---------------------------------";
    cout << endl << endl;

    cout << "                                1  -  Jogar" << endl;
    cout << "                                2  -  Regras" << endl;
    cout << "                                3  -  Ranking" << endl;
    cout << "                                4  -  Sair" << endl;

    selectMenuOption(getOption());
}

// Esperando a veia artística
void showOpening() {
    system("clear");
}


int main() {

    setUpWords();

    while(true) {
        showMenu();
    }

    for (int i = 0; i < words.size(); i++) {
        cout << words[i].text << " " << words[i].theme << " " << words[i].level << endl;
    }
    cout << endl;

    words.push_back({"Teste", "musica", 2});
    writeWords();

    quit();

}
