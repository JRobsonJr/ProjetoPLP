#include <iostream>
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

vector<Word> words;
// Utilizar Pair<int, Player> para o ranking. Vai poder aproveitar o comparador natural para ordenar.

void setupWords() {
    Word data;

    ifstream wordsFile;
    wordsFile.open("palavras.csv");

    string currentLine;
    const char delimiter = ',';
    while(getline(wordsFile, currentLine)){

        stringstream ss(currentLine);

        getline(ss, data.text, delimiter);
        getline(ss, data.theme, delimiter);
        ss >> data.level;

        words.push_back(data);

    }
    wordsFile.close();
}

void showOpening();
void showMenu();
void showRules();
void showRanking();

void selectMode();
void normalGameOptions();
void selectTheme();
void selectLevel();

void competitiveGame();
void getPlayerData();

void quit();


int main() {

    setupWords();

    for (int i = 0; i < words.size(); i++) {
        cout << words[i].text << " " << words[i].theme << " " << words[i].level << endl;
    }
    cout << "\n";


    return 0;
}
