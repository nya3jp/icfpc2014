#include <iostream>
#include <vector>
#include <string>
#include <fstream>
using namespace std;

typedef vector<string> Mat;

int countVacancy(char c){
  return (c=='#' ? 0 : 1);
}

void dump(const Mat &tbl) {
  double n = 0;
  double nnbd[5] = {0};
  for (int y=0;y<tbl.size();++y) {
    for (int x=0;x<tbl[0].size();++x) {
      char c = tbl[y][x];
      if (c!='#') {
	int nadj = countVacancy(tbl[y][x-1])
	  + countVacancy(tbl[y][x+1])
	  + countVacancy(tbl[y-1][x])
	  + countVacancy(tbl[y+1][x]);
	++nnbd[nadj];
      }
      ++n;
    }
  }

  cout << "den1: " << (nnbd[1]/n) << endl;
  cout << "den2: " << (nnbd[2]/n) << endl;
  cout << "den3: " << (nnbd[3]/n) << endl;
  cout << "den4: " << (nnbd[4]/n) << endl;
}

int main (int argc, char **argv) {
  ifstream ifs(argv[1]);
  Mat tbl;
  while(!ifs.eof()) {
    string str;
    getline(ifs,str);
    if (str.size()>0)
      tbl.push_back(str);
  }

  dump(tbl);
}
