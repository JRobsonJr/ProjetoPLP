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
    ifstream wordsFile(FILENAME_WORDS); // Impedindo compilação

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

    ofstream newWordsFile(FILENAME_WORDS); // Impedindo compilação
    if (isFileValid(newWordsFile)) {
        for (int i = 0; i < words.size(); i++) {

            newWordsFile << words[i].text << DELIMITER
                         << words[i].theme << DELIMITER
                         << words[i].level << endl;

        }

        newWordsFile.close();
    }
}

void normalGameOptions();
void selectTheme();
void selectLevel();

void competitiveGame();
void getPlayerData();

int getOption() {
    int option;

    cout << endl << endl;
    cout << "Informe o número da opção desejada: ";
    cin >> option;

    return option;
}

void selectOptionGameMode(int option) {
    switch (option) {
        case 1:
            // normalGameOptions();
            break;
        case 2:
            // competitiveGame();
            break;
        case 3:
            break;
        default:
            // INVÁLIDA;
            break;
    }
}

void selectGameMode()  {
    system("clear");

    cout << endl << endl;
    cout << "----- MODO DE JOGO -----" << endl << endl;
    cout << "1 - Jogo Rápido" << endl;
    cout << "2 - Modo Campeonato" << endl;
    cout << "3 - Voltar" << endl;

    selectOptionGameMode(getOption());
}

void showRules() {
    system("clear");

    cout << endl << endl;
    cout << "----- REGRAS -----" << endl << endl;
    cout << "    No jogo da forca, o jogador deve acertar a palavra que lhe foi proposta a partir d";
    cout << "o número de letras dessa palavra e do tema ligado a ela. A cada rodada, o jogador deve";
    cout << " sugerir uma letra. Se a palavra proposta contiver a letra sugerida, todas as ocorrênc";
    cout << "ias dessa letra são reveladas. Caso contrário, uma parte do enforcado será desenhada. ";
    cout << "Para vencer, o jogador deve ser capaz de revelar a palavra por completo antes que todo";
    cout << " o corpo do enforcado seja desenhado." << endl << "    O jogador poderá se desafiar em";
    cout << " dois modos distintos: o rápido e o campeonato. No modo rápido, o jogador enfrentará a";
    cout << "penas uma palavra, podendo especificar a dificuldade ou o tema dela, se assim desejar.";
    cout << " Já no modo campeonato, o jogador enfrentará diversas palavras em sequência e acumular";
    cout << "á pontos a cada palavra revelada. Nesse modo, o jogo segue até que o jogador perca uma";
    cout << " partida ou adivinhe todas as palavras possíveis. Em qualquer um dos casos, seu desemp";
    cout << "enho será registrado no ranking.";

    getchar();

}

// Precisa implementação do Ranking
void showRanking() {
    system("clear");

    cout << endl << endl;
    cout << "----- RANKING -----" << endl << endl;

    getchar();
}

void quit() {
    system("clear");

    cout << endl << endl;

    cout << "               Até a próxima!" << endl << endl;

    cout << "--------------- DEVELOPED BY ---------------" << endl << endl;
    cout << "                Fanny Vieira                " << endl;
    cout << "          Matheus Alves dos Santos          " << endl;
    cout << "               Misael Augusto               " << endl;
    cout << "                Paulo Leitão                " << endl;
    cout << "               Robson Junior                " << endl;
    cout << endl << endl;

    system("sleep 3s");
    exit(1);
}


void selectOptionMenu(int option) {
    switch (option) {
        case 1:
            // selectGameMode();
            break;
        case 2:
            // showRules();
            break;
        case 3:
            // showRanking();
            break;
        case 4:
            // quit();
            break;
        default:
            // INVÁLIDA;
            break;
    }
}

void showMenu() {
    system("clear");

    cout << endl << endl;
    cout << "----- MENU -----" << endl << endl;
    cout << "1 - Jogar" << endl;
    cout << "2 - Regras" << endl;
    cout << "3 - Ranking" << endl;
    cout << "4 - Sair" << endl;

    selectOptionMenu(getOption());
}

// Esperando a veia artística
void start() {
    system("clear");
}


int main() {

    setUpWords();

    for (int i = 0; i < words.size(); i++) {
        cout << words[i].text << " " << words[i].theme << " " << words[i].level << endl;
    }
    cout << endl;

    words.push_back({"Teste", "musica", 2});
    writeWords();

    quit();

}
