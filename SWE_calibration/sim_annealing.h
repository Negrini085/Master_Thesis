#include <iostream>
#include <cstdlib>
#include <fstream>
#include <vector>
#include <string>
#include <random>
#include <cstdio>
#include <array>

using namespace std;


#ifndef __SA__
#define __SA__


void runOrDie(const string& cmd) {
    int ret = system(cmd.c_str());
    if (ret != 0) { cerr << "Errore: " << cmd << " -> " << ret << endl; exit(1); }
}



array<double, 4> findCosts(const string& fname) {
    ifstream in(fname);
    array<double, 4> v;
    for (double& x : v) {
        if (!(in >> x) || !isfinite(x)) {
            cerr << "Error: invalid loss in " << fname << endl;
            exit(1);
        }
    }
    return v;
}



void saveInput(double appo_th, double appo_ddfav, double appo_ddfam, double appo_expfact, double appo_tmelt, string fname) {

    ofstream appo_out(fname);
    if (!appo_out.is_open()) {
        cerr << "Error: Could not create the parameters file!" << endl;
        exit(1);
    }

    appo_out << "tlim\tddf_ave\tddf_ampl\texpfact\ttmelt\n";
    appo_out << appo_th << "\t" << appo_ddfav << "\t" << appo_ddfam << "\t" << appo_expfact << "\t" << appo_tmelt << "\n";
    appo_out.close();
}



class SimAnnealing{

    public:
    SimAnnealing()
        : T_in(1), T_fin(1e-6), m_beta(0.0), m_new(0.0), m_old(0.0), m_delta(0.4),
          gen(41), dis_prob(0.0, 1.0), dis_move(-0.5, 0.5), m_th(2.0), m_ddfav(2.2), m_ddfam(1.7), m_expfact(0.5), m_melt(0)
    {}
    SimAnnealing(double tmax, double tmin, double delta, unsigned int seed = 0)
        : T_in(tmax), T_fin(tmin), m_beta(0.0), m_new(0.0), m_old(0.0), m_delta(delta),
          gen(seed == 0 ? rd() : seed), dis_prob(0.0, 1.0), dis_move(-0.5, 0.5), m_th(2.0), m_ddfav(2.2), m_ddfam(1.7), m_expfact(0.5), m_melt(0)
    {}
    ~SimAnnealing() = default;

    //Get methods
    double GetTin() const { return T_in; }
    double GetTfin() const { return T_fin; }

    //Set methods
    void SetTin(double tin) { T_in = tin; }
    void SetTfin(double tfin) { T_fin = tfin; }

    void SA(string fname, int num_run, double th_prec, double ddf_ave, double ddf_amp, double exp_fact, double t_melt) {

        int acce = 0;   //Numero di mosse accettato
        int totali = 0;   //Numero di mosse totali
        double T = T_in;    //Temperatura di partenza SA
        array<double, 4> acc_metrics;
        double appo_th, appo_ddfav, appo_ddfam, appo_expfact, appo_tmelt, acc_rate, factor, peso; //Variabili di appoggio per le mosse

        m_th = th_prec;
        m_ddfav = ddf_ave;
        m_ddfam = ddf_amp;
        m_expfact = exp_fact;
        m_melt = t_melt;


        ofstream file_loss;   //Canale di output
        ofstream file_param;   //Canale di output
        ofstream file_accrate;
        file_loss.open("loss_evo_" + to_string(num_run) + ".dat");
        file_param.open("param_evo_" + to_string(num_run) + ".dat");
        file_accrate.open("accrate_evo_" + to_string(num_run) + ".dat");


        // First simulation
        saveInput(m_th, m_ddfav, m_ddfam, m_expfact, m_melt, fname);
        runOrDie("Rscript model.R");
        runOrDie("Rscript convert_swe_to_hydro.R");
        system("rm -f Results/raw/*");
        runOrDie("Rscript compute_loss.R");

        // First performance evaluation
        acc_metrics = findCosts("appo_loss.dat");
        m_old = acc_metrics[0];
        system("rm Results/hydro/*");

        file_loss << acc_metrics[0] << "    " << acc_metrics[1] << "    " << acc_metrics[2] << "    " << acc_metrics[3] << "   " << T << endl;
        file_param << m_th << "   " << m_ddfav << "   " << m_ddfam << "   " << m_expfact << "    " << m_melt << endl;

        while(T >= T_fin){

            acce = 0;   //Re-setto acce a zero
            totali = 0;
            m_beta = 1/T;   //Calcolo parametro beta
            
            //Voglio accettare almeno 5 mosse
            while(acce < 5){

                //Propongo una nuova mossa
                totali++;
                do{ appo_th = m_th + m_delta * dis_move(gen); }while(appo_th < -1 || appo_th > 3);
                do{ appo_ddfav = m_ddfav + m_delta * dis_move(gen); }while(appo_ddfav < 1 || appo_ddfav > 5);
                do{ appo_ddfam = m_ddfam + m_delta * dis_move(gen); }while(appo_ddfam < 0.5 || appo_ddfam > 3.5);                
                do{ appo_expfact = m_expfact + 0.4 * m_delta * dis_move(gen); }while(appo_expfact < 0.2 || appo_expfact > 2);
                do{ appo_tmelt = m_melt + m_delta * dis_move(gen); }while(appo_tmelt < -2 || appo_tmelt > 4);


                saveInput(appo_th, appo_ddfav, appo_ddfam, appo_expfact, appo_tmelt, fname);

                // Making calculations
                std::remove("appo_loss.dat");
                runOrDie("Rscript model.R");
                runOrDie("Rscript convert_swe_to_hydro.R");
                system("rm -f Results/raw/*");
                runOrDie("Rscript compute_loss.R");

                // First performance evaluation
                acc_metrics = findCosts("appo_loss.dat");
                m_new = acc_metrics[0];
                system("rm Results/hydro/*");
                peso = -m_beta * (m_new - m_old);

                if (peso >= 0 || dis_prob(gen) < exp(peso)) { //Cambio effettivamente oppure no?
                    acce++;
                    m_old = m_new;
                    file_loss << acc_metrics[0] << "    " << acc_metrics[1] << "    " << acc_metrics[2] << "    " << acc_metrics[3] << "   " << T << endl;

                    m_th = appo_th;
                    m_ddfav = appo_ddfav;
                    m_ddfam = appo_ddfam;
                    m_expfact = appo_expfact;
                    m_melt = appo_tmelt;
                    
                    file_param << m_th << "   " << m_ddfav << "   " << m_ddfam << "   " << m_expfact << "   " << m_melt << endl;
                    cout << "T_th = " << m_th << "     ddf_ave = " << m_ddfav << "     ddf_amp = " << m_ddfam <<  "     expfact = " << m_expfact << "     tmelt = " << m_melt << "     loss = " << m_new << "     weight = " << peso << endl;
                }

                if((totali % 200 == 0) && (totali > 0)) m_delta = max(m_delta/2, 1e-5);
                if(totali == 750) break;
            }

            acc_rate = static_cast<double>(acce)/static_cast<double>(totali) * 100;
            file_accrate << acc_rate << "   " << m_delta << "   " << T << endl;
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
            m_delta = max(1e-5, min(m_delta, 0.5)); 
        }
	
        file_loss.close();
        file_param.close();
        file_accrate.close();

    }


    private:
    double T_in, T_fin, m_beta; //Data membri per temperatura
    double m_old, m_new, m_delta;   //Data membri per gestione gap

    double m_th, m_ddfav, m_ddfam, m_expfact, m_melt;

    random_device rd; mt19937 gen;
    uniform_real_distribution<double> dis_prob;
    uniform_real_distribution<double> dis_move;
};

#endif
