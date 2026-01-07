#!/bin/bash

year=(2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021)
month=(9 10 11 12 1 2 3 4 5 6 7 8)                     

recordSWE=()
datfile="appo.dat"
outF="recordSWE.dat"

#----------------------------------------------------------------------------#
#       Ciclo sulle annate e sui mesi di cui sono disponibili dei dati       #
#----------------------------------------------------------------------------#

# Ciclo che consente di creare l'intera serie
for y in "${year[@]}"; do
    for m in "${month[@]}"; do

        if (( $m >= 9 )); then
            appo=$(printf "%d%02d" $((y-1)) "$m")
        else
            appo=$(printf "%d%02d" $((y)) "$m")
        fi

        appo="y${y}/ITSNOW_SWE_${appo}.nc"
        python3 recordSWE.py "$appo"


        # Leggo il file prodotto dall'eseguibile python precedente
        if [[ -f "$datfile" ]]; then
            mapfile -t tmp < "$datfile"

            for v in "${tmp[@]}"; do
              [[ -n "$v" ]] && recordSWE+=("$v")
            done
        fi

        rm "$datfile"
    done
done

printf "%s\n" "${recordSWE[@]}" > "$outF"
echo "Salvato in $outF"

echo "Studio dell'evoluzione dello SWE terminato."
