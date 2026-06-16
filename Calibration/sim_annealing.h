#include <iostream>
#include <cstdlib>
#include <fstream>
#include <vector>
#include <string>
#include <random>

using namespace std;


#ifndef __SA__
#define __SA__


double findCost(string fname){
    ifstream filein;
    double appo;

    // Opening file stream
    filein.open(fname);
        if (!filein.is_open()) {
        cerr << "Error: Could not find loss function value!" << endl;
        exit(1);
    }

    filein >> appo;
    return appo;
}


void saveInput(double appo_th, double appo_ddfm, double appo_ddfM, string fname) {

    ofstream appo_out(fname);
    if (!appo_out.is_open()) {
        cerr << "Error: Could not create the parameters file!" << endl;
        exit(1);
    }

    appo_out << "tlim\tddf_min\tddf_max\n";
    appo_out << appo_th << "\t" << appo_ddfm << "\t" << appo_ddfM << "\n";
    appo_out.close();
}



class SimAnnealing{

    public:
    SimAnnealing()
        : T_in(10), T_fin(1e-5), m_beta(0.0), m_new(0.0), m_old(0.0), m_delta(0.4),
          gen(41), dis_prob(0.0, 1.0), dis_move(-0.5, 0.5), m_th(2.0), m_ddfm(0.5), m_ddfM(4.0)
    {}
    SimAnnealing(double tin, double tfin, double delta, unsigned int seed = 0) 
        : T_in(tin), T_fin(tfin), m_beta(0.0), m_new(0.0), m_old(0.0), m_delta(delta),
          gen(seed == 0 ? rd() : seed), dis_prob(0.0, 1.0), dis_move(-0.5, 0.5), m_th(2.0), m_ddfm(0.5), m_ddfM(4.0)
    {}
    ~SimAnnealing() = default;

    //Get methods
    double GetTin() const { return T_in; }
    double GetTfin() const { return T_fin; }

    //Set methods
    void SetTin(double tin) { T_in = tin; }
    void SetTfin(double tfin) { T_fin = tfin; }

    void SA(string fname) {

        double p = 0;   //Probabilità di accettare la mossa
        int acce = 0;   //Numero di mosse accettato
        int totali = 0;   //Numero di mosse totali
        double T = T_in;    //Temperatura di partenza SA
        double appo_th, appo_ddfm, appo_ddfM, acc_rate, factor, peso; //Variabili di appoggio per le mosse
    

        ofstream fileout;   //Canale di output
        ofstream file_out;   //Canale di output
        fileout.open("SimAnnealing.dat");
        file_out.open("moves.dat");

        // First simulation
        saveInput(m_th, m_ddfm, m_ddfM, fname);
        system("Rscript model.R");
        system("Rscript convert_swe_to_hydro.R");
        system("rm Results/raw/*");
        system("Rscript compute_loss.R");

        // First performance evaluation
        m_old = findCost("appo_loss.dat");
        system("rm Results/hydro/*");

        fileout << m_old << "   " << T << endl;
        file_out << m_th << "   " << m_ddfm << "   " << m_ddfM << endl;

        while(T >= T_fin){

            acce = 0;   //Re-setto acce a zero
            totali = 0;
            m_beta = 1/T;   //Calcolo parametro beta
            
            //Voglio accettare almeno 10 mosse
            while(acce < 10){

                //Propongo una nuova mossa
                totali++;
                do{
                    appo_th = m_th + m_delta * dis_move(gen);
                }while(appo_th < 1.5 || appo_th > 2.5);
                do{
                    appo_ddfm = m_ddfm + m_delta * dis_move(gen);
                }while(appo_ddfm < 0.3 || appo_ddfm > 1.3);
                do{
                    appo_ddfM = m_ddfM + m_delta * dis_move(gen);
                }while(appo_ddfM > 4.5 || appo_ddfM < 3.5);
                saveInput(appo_th, appo_ddfm, appo_ddfM, fname);

                // Making calculations
                system("Rscript model.R");
                system("Rscript convert_swe_to_hydro.R");
                system("rm Results/raw/*");
                system("Rscript compute_loss.R");

                // First performance evaluation
                m_new = findCost("appo_loss.dat");
                system("rm Results/hydro/*");
                peso = -m_beta * (m_new - m_old);
                p = exp(peso); //Probabilità accettazione mossa

                if(dis_prob(gen) < p) { //Cambio effettivamente oppure no?
                    acce++;
                    m_old = m_new;
                    fileout << m_old << "   " << T << endl;

                    m_th = appo_th;
                    m_ddfm = appo_ddfm;
                    m_ddfM = appo_ddfM;
                    
                    file_out << m_th << "   " << m_ddfm << "   " << m_ddfM << endl;
                    cout << "T_th = " << m_th << "     ddf_min = " << m_ddfm << "     ddf_max = " << m_ddfM << "     loss = " << m_new << "     weight = " << peso << endl;
                }

                if(totali == 500) break;
            }

            acc_rate = static_cast<double>(acce)/static_cast<double>(totali) * 100;
            cout << "-------------------------------------------------" << endl;
            cout << "                T = " << T << "                  " << endl;
            cout << "               AR = " << acc_rate << " %         " << endl;
            cout << "            Delta = " << m_delta  << "           " << endl;
            if(totali == 500)   cout << "            Exceeded 500 attempts" << endl;
            cout << "-------------------------------------------------" << endl;

            T = 0.95 * T;

            factor = exp(acc_rate/100 - 0.3);
            factor = max(0.75, min(1.25, factor));

            m_delta = m_delta * factor;
            m_delta = max(0.001, min(m_delta, 1.0)); 
        }
	
        fileout.close();
        file_out.close();

    }


    private:
    double T_in, T_fin, m_beta; //Data membri per temperatura
    double m_old, m_new, m_delta;   //Data membri per gestione gap

    double m_th, m_ddfm, m_ddfM;

    random_device rd; mt19937 gen;
    uniform_real_distribution<double> dis_prob;
    uniform_real_distribution<double> dis_move;
};

#endif
