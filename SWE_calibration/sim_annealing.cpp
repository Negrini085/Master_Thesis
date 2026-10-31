#include <iostream>
#include <random>
#include <string>
#include "sim_annealing.h"

using namespace std;

int main(int argc, char* argv[]) {

    if (argc != 2) {
        cout << "Wrong code usage! Actual code usage is: ./" << argv[0] << " <input file>" << endl;
        return 1;
    }

    string fname = argv[1];    
    
    // Random generator to start SA procedure
    mt19937 gen(10);
    uniform_real_distribution<double> dis_th(-4.0, 8.0);
    uniform_real_distribution<double> dis_ddfav(1, 9);
    uniform_real_distribution<double> dis_ddfam(0.5, 7.5);
    
    
    // Cycle to have multiple SA runs
    for (int i = 1; i <= 4; i++) {
        unsigned int sa_seed = gen();
        double th = dis_th(gen), ddfav, ddfam;
        do { ddfav = dis_ddfav(gen); ddfam = dis_ddfam(gen); } while (ddfav <= ddfam);

        if (i <= 3) continue;
        
        SimAnnealing prova(5, 5e-6, 2, sa_seed);
        prova.SA(fname, i, th, ddfav, ddfam, 0.5, 0);
   }

    return 0;
}
