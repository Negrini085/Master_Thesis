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
    double appo_ave, appo_amp;
    
    
    // Random generator to start SA procedure
    mt19937 gen(120);
    uniform_real_distribution<double> dis_th(-2.0, 2.0);
    uniform_real_distribution<double> dis_ddfav(1.0, 3.0);
    uniform_real_distribution<double> dis_ddfam(0.1, 2.0);
    uniform_real_distribution<double> dis_expfact(0.1, 1.0);
    
    
    // Cycle to have multiple SA runs
    for(int i = 1; i < 5; i++){
        
        do{
            appo_ave = dis_ddfav(gen);
            appo_amp = dis_ddfam(gen);
        }while((appo_ave - appo_amp) <= 0);
        
        unsigned int sa_seed = gen();
        SimAnnealing prova(10, 1e-6, 0.4, sa_seed); 
        prova.SA(fname, i, dis_th(gen), appo_ave, appo_amp, dis_expfact(gen));
    }

    return 0;
}