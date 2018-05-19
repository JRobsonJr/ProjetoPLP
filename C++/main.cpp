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

const char DELIMITER = ',';
const string FILENAME_WORDS = "words.csv";
vector<Word> words;
// Utilizar Pair<int, Player> para o ranking. Vai poder aproveitar o comparador natural para ordenar.


bool isFileValid(ios &file){
    if(!file.good()){
        cerr << "Não foi possivel ler o arquivo." << endl;
        exit(1);
    }
    return true;
}

void setupWords() {

    Word data;
    string currentLine;
    ifstream wordsFile(FILENAME_WORDS);

    if(isFileValid(wordsFile)){
        while(getline(wordsFile, currentLine)){

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
    ofstream newWordsFile (FILENAME_WORDS);
    if(isFileValid(newWordsFile)){
        for (int i = 0; i < words.size(); i++) {

            newWordsFile << words[i].text << DELIMITER
                         << words[i].theme << DELIMITER
                         << words[i].level << endl;

        }
        newWordsFile.close();
    }
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

    words.push_back({"Teste", "musica", 2});
    writeWords();


    return 0;
}
