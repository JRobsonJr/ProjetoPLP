#include <iostream>
#include <fstream>
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

void setUpWords() {
    words.push_back({"-", "-", 1});
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
    FILE *file = fopen("words.txt", "w");
    setUpWords();

    for (int i = 0; i < words.size(); i++) {
        cout << words[i].text << " " << words[i].theme << " " << words[i].level << endl;
    }
    cout << "\n";

    fclose(file);
    return 0;
}
