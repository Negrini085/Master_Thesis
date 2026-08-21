#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Input and output filename
INPUT_FILE="$SCRIPT_DIR/input.dat"
LOSS_FILE="$SCRIPT_DIR/appo_loss.dat"
OUTPUT_FILE="$SCRIPT_DIR/test_temp_ddf_magnusson.dat"

# Checking input file existence
if [[ ! -f "$INPUT_FILE" ]]; then
    echo "Errore: $INPUT_FILE non trovato."
    exit 1
fi

# Saving input file
cp "$INPUT_FILE" "${INPUT_FILE}.backup"
echo -e "temperature\tloss" > "$OUTPUT_FILE"

# Cycle over temperatures
for i in $(seq 0 1500); do

    # Temperature computation
    temperature=$(awk -v i="$i" 'BEGIN {printf "%.3f", 0.001 + i/500}')


    # Changing input.dat accordingly
    awk -v temp="$temperature" '
        NR == 2 {
            $1 = temp
        }
        {print}
    ' "$INPUT_FILE" > "${INPUT_FILE}.tmp" && mv "${INPUT_FILE}.tmp" "$INPUT_FILE"

    
    # Computing SWE series
    cd "$SCRIPT_DIR" || exit 1
    Rscript model.R
    if [[ $? -ne 0 ]]; then
        echo "Error in model.R at temperature $temperature"
        exit 1
    fi


    # Converting to pseudo-hydrological format
    Rscript convert_swe_to_hydro.R
    if [[ $? -ne 0 ]]; then
        echo "Error during convert_swe_to_hydro.R at temperature $temperature"
        exit 1
    fi


    # Computing loss
    Rscript compute_loss.R
    if [[ $? -ne 0 ]]; then
        echo "Error during compute_loss.R at temperature $temperature"
        exit 1
    fi


    # Checking appo_loss.dat existence
    if [[ ! -f "$LOSS_FILE" ]]; then
        echo "Error: $LOSS_FILE not found at temperature $temperature"
        exit 1
    fi


    # Reading loss value
    loss=$(cat "$LOSS_FILE")
    loss=$(echo "$loss" | xargs)

    echo -e "${temperature}\t${loss}" >> "$OUTPUT_FILE"

    echo "Temperature = $temperature, Loss = $loss"

done

mv "${INPUT_FILE}.backup" "$INPUT_FILE"

echo "========================================"
echo "Test completed."
echo "Results saved in:"
echo "$OUTPUT_FILE"
echo "========================================"
