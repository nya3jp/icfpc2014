#include <iostream>
#include <cstdlib>
#include <sstream>
#include <vector>
#include <map>
#include <complex>
#include <string>
#include <fstream>
#include <cmath>
using namespace std;

typedef vector<string> Mat;
typedef complex<int> pt;

int W,H;
Mat theMat;
map<double, pt> vacantPts;


int countVacancy(char c){
  return (c=='#' ? 0 : 1);
}

double drand(){
  return (rand())/double(RAND_MAX);
}

int irand(int lo,int hi) {
  return floor(lo+drand()*(hi-lo));
}

pt vrand() {
  double r = drand();
  if (r<0.25) return pt(0,-1);
  if (r<0.5) return pt(1,0);
  if (r<0.75) return pt(0,1);
  return pt(-1,0);
}

double probPenetrate = 0.0;//1;
double probTurn = 0.25;
double probParity = 0.5;
void setParam(){
  //probPenetrate = drand();
  probTurn = 1/exp(drand()*log(W+H));
  if(rand()< 0.3)probParity = drand();
}

void dump() {
  double n = 0;
  double nnbd[5] = {0};
  for (int y=0;y<theMat.size();++y) {
    cout << theMat[y] << endl;
  }
  for (int y=0;y<theMat.size();++y) {
    for (int x=0;x<theMat[0].size();++x) {
      char c = theMat[y][x];
      if (c!='#') {
	int nadj = countVacancy(theMat[y][x-1])
	  + countVacancy(theMat[y][x+1])
	  + countVacancy(theMat[y-1][x])
	  + countVacancy(theMat[y+1][x]);
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

bool isVacant(pt o) {
  return theMat[o.imag()][o.real()]==' ';
}
bool isAlmostVacant(pt o) {
  return theMat[o.imag()][o.real()]==' ' || theMat[o.imag()][o.real()]=='.';
}
bool isPassable(pt o) {
  return theMat[o.imag()][o.real()]!='#';
}


bool drillable(pt o) {
  if(o.real()<=0) return false;
  if(o.real()>=W-1) return false;
  if(o.imag()<=0) return false;
  if(o.imag()>=H-1) return false;
  if(isPassable(o+pt(0,-1)) && isPassable(o+pt(-1,0)) && isPassable(o+pt(-1,-1)))
    return false;
  if(isPassable(o+pt(0, 1)) && isPassable(o+pt(-1,0)) && isPassable(o+pt( 1,-1)))
    return false;
  if(isPassable(o+pt(0, 1)) && isPassable(o+pt(-1,0)) && isPassable(o+pt(-1, 1)))
    return false;
  if(isPassable(o+pt(0, 1)) && isPassable(o+pt( 1,0)) && isPassable(o+pt( 1, 1)))
    return false;
  return true;
}

bool doesPenetrate(pt o) {
  int n = 0;
  if(isPassable(o+pt(0,-1))) ++n;
  if(isPassable(o+pt(1,0))) ++n;
  if(isPassable(o+pt(0,1))) ++n;
  if(isPassable(o+pt(-1,0))) ++n;
  return n>=2;
}

int drillFailCombo = 0;
void drill(pair<double,pt> po, pt v){
  pt o = po.second;
  for(;;){
    o+=v;
    if(!drillable(o)) {
      break;
    }
    if(doesPenetrate(o)){
      if(!(drand() < probPenetrate))
	break;
    }
    if(!  isPassable(o) ) drillFailCombo = 0;
    theMat[o.imag()][o.real()] = ' ';
    vacantPts.insert(make_pair(1+drand(),o));
    //vacantPts.push_back(o);
    if(drand()<probTurn) {
      if(drand()<probParity){
	v *= pt(0,1);
      } else {
	v *= pt(0,-1);
      }
    }
  }

  ++drillFailCombo; 
  vacantPts.erase(po.first);
  po.first*=(2+drand());
  vacantPts.insert(po);

  return;
}



void genMat() {
  theMat = Mat(H, string(W,'#'));
  int ox=irand(1,W-1), oy=irand(1,H-1);
  pt o(ox,oy);
  vacantPts=map<double,pt>();
  vacantPts.insert(make_pair(0,o));
   while(!vacantPts.empty() && drillFailCombo < W*H){
     pair<double,pt> po=*(vacantPts.begin());
     drill(po,vrand());    
  }

  return;
}

int main (int argc, char **argv) {
  int seed = time(NULL);
  ifstream ifs("/dev/urandom");
  for(char c; ifs>>c;){
    seed = (seed << 8)/17 + c;
    if(c=='7') break;
  }
  srand(seed);
  istringstream iss1(argv[1]);
  istringstream iss2(argv[2]);
  iss1 >> W;
  iss2 >> H;

  setParam();

  genMat();

  dump();
}
