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


void saveInput(double appo_th, double appo_ddfav, double appo_ddfam, double appo_expfact, string fname) {

    ofstream appo_out(fname);
    if (!appo_out.is_open()) {
        cerr << "Error: Could not create the parameters file!" << endl;
        exit(1);
    }

    appo_out << "tlim\tddf_ave\tddf_ampl\texpfact\n";
    appo_out << appo_th << "\t" << appo_ddfav << "\t" << appo_ddfam << "\t" << appo_expfact << "\n";
    appo_out.close();
}



class SimAnnealing{

    public:
    SimAnnealing()
        : T_in(10), T_fin(1e-6), m_beta(0.0), m_new(0.0), m_old(0.0), m_delta(0.4),
          gen(41), dis_prob(0.0, 1.0), dis_move(-0.5, 0.5), m_th(2.0), m_ddfav(2.2), m_ddfam(1.7), m_expfact(0.5)
    {}
    SimAnnealing(double tmax, double tmin, double delta, unsigned int seed = 0)
        : T_in(tmax), T_fin(tmin), m_beta(0.0), m_new(0.0), m_old(0.0), m_delta(delta),
          gen(seed == 0 ? rd() : seed), dis_prob(0.0, 1.0), dis_move(-0.5, 0.5), m_th(2.0), m_ddfav(2.2), m_ddfam(1.7), m_expfact(0.5)
    {}
    ~SimAnnealing() = default;

    //Get methods
    double GetTin() const { return T_in; }
    double GetTfin() const { return T_fin; }

    //Set methods
    void SetTin(double tin) { T_in = tin; }
    void SetTfin(double tfin) { T_fin = tfin; }

    void SA(string fname, int num_run, double th_prec, double ddf_ave, double ddf_amp, double exp_fact) {

        double p = 0;   //Probabilità di accettare la mossa
        int acce = 0;   //Numero di mosse accettato
        int totali = 0;   //Numero di mosse totali
        double T = T_in;    //Temperatura di partenza SA
        double appo_th, appo_ddfav, appo_ddfam, appo_expfact, acc_rate, factor, peso; //Variabili di appoggio per le mosse

        m_th = th_prec;
        m_ddfav = ddf_ave;
        m_ddfam = ddf_amp;
        m_expfact = exp_fact;
    

        ofstream fileout;   //Canale di output
        ofstream file_out;   //Canale di output
        fileout.open("loss_evo_" + to_string(num_run) + ".dat");
        file_out.open("param_evo_" + to_string(num_run) + ".dat");

        // First simulation
        saveInput(m_th, m_ddfav, m_ddfam, m_expfact, fname);
        system("Rscript model.R");
        system("Rscript convert_swe_to_hydro.R");
        system("rm Results/raw/*");
        system("Rscript compute_loss.R");

        // First performance evaluation
        m_old = findCost("appo_loss.dat");
        system("rm Results/hydro/*");

        fileout << m_old << "   " << T << endl;
        file_out << m_th << "   " << m_ddfav << "   " << m_ddfam << "   " << m_expfact << endl;

        while(T >= T_fin){

            acce = 0;   //Re-setto acce a zero
            totali = 0;
            m_beta = 1/T;   //Calcolo parametro beta
            
            //Voglio accettare almeno 5 mosse
            while(acce < 10){

                //Propongo una nuova mossa
                totali++;
                appo_th = m_th + m_delta * dis_move(gen);
                do{
                   appo_expfact = m_expfact + 0.3 * m_delta * dis_move(gen);
                }while(appo_expfact <= 0.0);
                do{
                    appo_ddfam = m_ddfam + m_delta * dis_move(gen);
                    appo_ddfav = m_ddfav + m_delta * dis_move(gen);
                }while(appo_ddfav < 0 || appo_ddfam < 0 || (appo_ddfav - appo_ddfam) < 0);
                saveInput(appo_th, appo_ddfav, appo_ddfam, appo_expfact, fname);

                // Making calculations
                system("Rscript model.R");
                system("Rscript convert_swe_to_hydro.R");
                system("rm Results/raw/*");
                system("Rscript compute_loss.R");

                // First performance evaluation
                m_new = findCost("appo_loss.dat");
                system("rm Results/hydro/*");
                peso = -m_beta * (m_new - m_old);

                if (peso >= 0 || dis_prob(gen) < exp(peso)) { //Cambio effettivamente oppure no?
                    acce++;
                    m_old = m_new;
                    fileout << m_old << "   " << T << endl;

                    m_th = appo_th;
                    m_ddfav = appo_ddfav;
                    m_ddfam = appo_ddfam;
                    m_expfact = appo_expfact;
                    
                    file_out << m_th << "   " << m_ddfav << "   " << m_ddfam << "   " << m_expfact << endl;
                    cout << "T_th = " << m_th << "     ddf_ave = " << m_ddfav << "     ddf_amp = " << m_ddfam <<  "     expfact = " << m_expfact << "     loss = " << m_new << "     weight = " << peso << endl;
                }

                if((totali % 200 == 0) && (totali > 0)) m_delta = max(m_delta/5, 1e-4);
                if(totali == 750) break;
            }

            acc_rate = static_cast<double>(acce)/static_cast<double>(totali) * 100;
            cout << "-------------------------------------------------" << endl;
            cout << "                T = " << T << "                  " << endl;
            cout << "               AR = " << acc_rate << " %         " << endl;
            cout << "            Delta = " << m_delta  << "           " << endl;
            if(totali == 750)   cout << "            Exceeded 750 attempts" << endl;
            cout << "-------------------------------------------------" << endl;

            T = 0.95 * T;

            factor = exp(acc_rate/100 - 0.4);
            factor = max(0.6, min(1.4, factor));

            m_delta = m_delta * factor;
            m_delta = max(0.0001, min(m_delta, 1.0)); 
        }
	
        fileout.close();
        file_out.close();

    }


    private:
    double T_in, T_fin, m_beta; //Data membri per temperatura
    double m_old, m_new, m_delta;   //Data membri per gestione gap

    double m_th, m_ddfav, m_ddfam, m_expfact;

    random_device rd; mt19937 gen;
    uniform_real_distribution<double> dis_prob;
    uniform_real_distribution<double> dis_move;
};

#endif
