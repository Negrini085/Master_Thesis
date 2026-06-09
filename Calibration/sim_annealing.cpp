#include <iostream>
#include <string>
#include "sim_annealing.h"

using namespace std;

int main(int argc, char* argv[]) {

    if (argc != 2) {
        cout << "Wrong code usage! Actual code usage is: ./" << argv[0] << " <input file>" << endl;
        return 1;
    }

    string fname = argv[1];
    
    SimAnnealing prova; 
    prova.SA(fname);

    return 0;
}