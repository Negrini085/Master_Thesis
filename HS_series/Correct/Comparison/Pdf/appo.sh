#!/usr/bin/env bash
# =============================================================================
# merge_hsd_pdfs.sh
#
# Unisce PDF HSD in un unico file:
#   - ordine alfabetico per stazione
#   - ordine cronologico all'interno della stazione
#   - pagina bianca tra stazioni
#
# Compatibile con file tipo:
#
#   HSD_TAA_TN_PASSO_VALLES_2019_to_2020.pdf
#   HSD_IT_VEN_BL_MONTE_PIANA_394_2021_to_2022.pdf
#   HSD_LOM_SO_APRICA_08017_2020_to_2021.pdf
#
# Dipendenze:
#   - qpdf
#   - ghostscript (gs)
#
# Uso:
#   chmod +x merge_hsd_pdfs.sh
#   ./merge_hsd_pdfs.sh [dir_input] [output.pdf]
#
# Esempi:
#   ./merge_hsd_pdfs.sh
#   ./merge_hsd_pdfs.sh ./dati merged.pdf
# =============================================================================

set -euo pipefail

INPUT_DIR="${1:-.}"
OUTPUT="${2:-merged_output.pdf}"

TMPDIR_WORK=$(mktemp -d)
trap 'rm -rf "$TMPDIR_WORK"' EXIT

# -----------------------------------------------------------------------------
# Controllo dipendenze
# -----------------------------------------------------------------------------
for cmd in qpdf gs; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Errore: comando '$cmd' non trovato." >&2
        echo "" >&2
        echo "Installazione:" >&2
        echo "  macOS : brew install qpdf ghostscript" >&2
        echo "  Ubuntu: sudo apt install qpdf ghostscript" >&2
        exit 1
    fi
done

# -----------------------------------------------------------------------------
# Crea pagina bianca A4
# -----------------------------------------------------------------------------
BLANK_PAGE="$TMPDIR_WORK/blank.pdf"

gs -q -dNOPAUSE -dBATCH \
   -sDEVICE=pdfwrite \
   -dDEVICEWIDTHPOINTS=595 \
   -dDEVICEHEIGHTPOINTS=842 \
   -sOutputFile="$BLANK_PAGE" \
   -c "showpage"

# -----------------------------------------------------------------------------
# Raccolta file
#
# Pattern accettato:
#
#   QUALSIASI_COSA_YYYY_to_YYYY.pdf
#
# Esempi:
#   HSD_TAA_TN_PASSO_VALLES_2019_to_2020.pdf
#   HSD_IT_VEN_BL_MONTE_PIANA_394_2021_to_2022.pdf
#
# station   = tutto prima dell'anno
# year_from = primo anno
# -----------------------------------------------------------------------------

declare -A STATION_FILES

while IFS= read -r -d '' filepath; do

    filename=$(basename "$filepath")

    # Regex generica
    if [[ "$filename" =~ ^(.+)_([0-9]{4})_to_([0-9]{4})\.pdf$ ]]; then

        station="${BASH_REMATCH[1]}"
        year_from="${BASH_REMATCH[2]}"

        # Salva:
        # anno<TAB>filepath
        STATION_FILES["$station"]+="${year_from}"$'\t'"${filepath}"$'\n'

    else
        echo "Avviso: file ignorato (nome non valido): $filename" >&2
    fi

done < <(
    find "$INPUT_DIR" \
        -maxdepth 1 \
        -type f \
        -name "HSD_*.pdf" \
        -print0
)

# -----------------------------------------------------------------------------
# Controllo presenza file
# -----------------------------------------------------------------------------
if [[ ${#STATION_FILES[@]} -eq 0 ]]; then
    echo "Nessun file HSD trovato in '$INPUT_DIR'." >&2
    exit 1
fi

# -----------------------------------------------------------------------------
# Costruzione lista merge
# -----------------------------------------------------------------------------
MERGE_LIST=()

FIRST_STATION=true

echo ""
echo "========================================"
echo "PDF trovati:"
echo "========================================"

# Ordina alfabeticamente le stazioni
while IFS= read -r station; do

    # Inserisce pagina bianca tra stazioni
    if [[ "$FIRST_STATION" == true ]]; then
        FIRST_STATION=false
    else
        MERGE_LIST+=("$BLANK_PAGE")
        echo "  [pagina bianca]"
    fi

    echo ""
    echo "Stazione: $station"

    # Ordina cronologicamente
    while IFS=$'\t' read -r year file; do

        [[ -z "${file:-}" ]] && continue

        MERGE_LIST+=("$file")

        echo "  $(basename "$file")"

    done < <(
        printf '%s' "${STATION_FILES[$station]}" | sort -k1,1n
    )

done < <(
    printf '%s\n' "${!STATION_FILES[@]}" | sort
)

# -----------------------------------------------------------------------------
# Merge finale
# -----------------------------------------------------------------------------
echo ""
echo "========================================"
echo "Creazione PDF finale..."
echo "========================================"

qpdf --empty --pages "${MERGE_LIST[@]}" -- "$OUTPUT"

echo ""
echo "Fatto!"
echo "Output : $OUTPUT"
echo "Pagine : $(qpdf --show-npages "$OUTPUT")"
