#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Input and output filename
INPUT_FILE="$SCRIPT_DIR/input.dat"
LOSS_FILE="$SCRIPT_DIR/appo_loss.dat"
OUTPUT_FILE="$SCRIPT_DIR/uniform_calib.dat"
cd "$SCRIPT_DIR" || exit 1

# Checking input file existence
if [[ ! -f "$INPUT_FILE" ]]; then
    echo "Errore: $INPUT_FILE non trovato."
    exit 1
fi

# Saving input file
echo -e "t_th\tddf_ave\tddf_amp\texpfact\tloss" > "$OUTPUT_FILE"

# Selecting parameters
EXPFACT_VALS=( $(LC_ALL=C seq 0.1 0.3 1.9) )
DDFAVE_VALS=( $(LC_ALL=C seq 1 0.3 3.1) )
DDFAMP_VALS=( $(LC_ALL=C seq 1 0.3 2.2) )
TTH_VALS=( $(LC_ALL=C seq -1 0.3 2) )

for expfact in "${EXPFACT_VALS[@]}"; do
    for ddf_ave in "${DDFAVE_VALS[@]}"; do
        for ddf_amp in "${DDFAMP_VALS[@]}"; do
            for t_th in "${TTH_VALS[@]}"; do

            # Changing input file content
            printf "tlim\tddf_ave\tddf_ampl\texpfact\n%s\t%s\t%s\t%s\n" \
                "$t_th" "$ddf_ave" "$ddf_amp" "$expfact" > "$INPUT_FILE"

            Rscript model.R
            if [[ $? -ne 0 ]]; then
                echo "Error in model.R"
                exit 1
            fi


            # Converting to pseudo-hydrological format
            Rscript convert_swe_to_hydro.R
            if [[ $? -ne 0 ]]; then
                echo "Error during convert_swe_to_hydro.R"
                exit 1
            fi


            # Computing loss
            Rscript compute_loss.R
            if [[ $? -ne 0 ]]; then
                echo "Error during compute_loss.R"
                exit 1
            fi


            # Checking appo_loss.dat existence
            if [[ ! -f "$LOSS_FILE" ]]; then
                echo "Error: $LOSS_FILE not found"
                exit 1
            fi


            # Reading loss value
            loss=$(cat "$LOSS_FILE")
            loss=$(echo "$loss" | xargs)
            
            printf "%s\t%s\t%s\t%s\t%s\n" \
               "$t_th" "$ddf_ave" "$ddf_amp" "$expfact" "$loss" >> "$OUTPUT_FILE"
            echo "t_th=$t_th ddf_ave=$ddf_ave ddf_amp=$ddf_amp expfact=$expfact loss=$loss"
            done
        done
    done
done


echo "========================================"
echo "Test completed."
echo "Results saved in:"
echo "$OUTPUT_FILE"
echo "========================================"
