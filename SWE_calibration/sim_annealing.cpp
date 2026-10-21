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
    mt19937 gen(126);
    uniform_real_distribution<double> dis_th(-1.0, 3.0);
    uniform_real_distribution<double> dis_ddfav(2, 4);
    uniform_real_distribution<double> dis_ddfam(0.5, 1.5);
    uniform_real_distribution<double> dis_expfact(0.2, 2);
    uniform_real_distribution<double> dis_tmelt(-2, 4);

    
    
    // Cycle to have multiple SA runs
    for(int i = 1; i <= 2; i++){
        
        unsigned int sa_seed = gen();
        SimAnnealing prova(1, 1e-6, 0.5, sa_seed); 
        prova.SA(fname, i, dis_th(gen), dis_ddfav(gen), dis_ddfam(gen), dis_expfact(gen), dis_tmelt(gen));
    }

    return 0;
}
