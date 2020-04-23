#!/usr/bin/env bash

set -Eumo pipefail

if (( ${BASH_VERSION%%.*} < 4 )); then echo "ERROR: [bash] version >= 4.0 required!" ; exit -1 ; fi

ROOT="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)/.."

COMPARE_MASKS="scripts/compare_masks.py"
EXTRACT_MASK="scripts/extract_mask.py"
RECOVER_MASK="scripts/recover_mask.py"

DATA_DIR=$(realpath "$1")


TABLE_RANGES_FILE="$DATA_DIR/table_ranges.csv"
if [ ! -f "$TABLE_RANGES_FILE" ]; then
  echo "$TABLE_RANGES_FILE file not found!"
  exit 1
fi

EVALUATED_CSVS_DIR="$DATA_DIR/evaluated_csvs"
if [ ! -d "$EVALUATED_CSVS_DIR" ]; then
  echo "$EVALUATED_CSVS_DIR directory not found!"
  exit 1
fi

FORMULA_CSVS_DIR="$DATA_DIR/formula_csvs"
if [ ! -d "$FORMULA_CSVS_DIR" ]; then
  echo "$FORMULA_CSVS_DIR directory not found!"
  exit 1
fi


EXTRACTED_MASKS_DIR="$DATA_DIR/extracted_masks"
mkdir -p "$EXTRACTED_MASKS_DIR"

RECOVERED_MASKS_DIR="$DATA_DIR/recovered_masks"
mkdir -p "$RECOVERED_MASKS_DIR"

FULL_COMPARISON_MAKS_DIR="$DATA_DIR/comparison_masks/full"
mkdir -p "$FULL_COMPARISON_MAKS_DIR"

TABLE_COMPARISON_MAKS_DIR="$DATA_DIR/comparison_masks/in-table"
mkdir -p "$TABLE_COMPARISON_MAKS_DIR"


recreate_dir() {
  rm -rf "$1"
  mkdir -p "$1"
}


cd "$ROOT"

"$EXTRACT_MASK" -i "$FORMULA_CSVS_DIR" -o "$EXTRACTED_MASKS_DIR"

TABLE_RANGES_HEADER=$(head -n 1 "$TABLE_RANGES_FILE")
IFS=',' read -ra TABLE_SOURCES <<< "$TABLE_RANGES_HEADER"
TABLE_SOURCES=("${TABLE_SOURCES[@]:1}")

recreate_dir "$FULL_COMPARISON_MAKS_DIR/Baseline"
recreate_dir "$TABLE_COMPARISON_MAKS_DIR/Baseline"
recreate_dir "$RECOVERED_MASKS_DIR/Baseline"

"$RECOVER_MASK" -e "$EVALUATED_CSVS_DIR" \
                -o "$RECOVERED_MASKS_DIR/Baseline" \
                -c "-1" \
                "$TABLE_RANGES_FILE"
"$COMPARE_MASKS" -g "$EXTRACTED_MASKS_DIR" \
                 -p "$RECOVERED_MASKS_DIR/Baseline" \
                 -o "$FULL_COMPARISON_MAKS_DIR/Baseline" \
  | tee "$DATA_DIR/full_formula_recovery_Baseline.csv"

for i in "${!TABLE_SOURCES[@]}"; do
  TABLE_SOURCE=$(echo "${TABLE_SOURCES[$i]}" | sed 's/ *$//g')

  recreate_dir "$FULL_COMPARISON_MAKS_DIR/$TABLE_SOURCE"
  recreate_dir "$TABLE_COMPARISON_MAKS_DIR/$TABLE_SOURCE"
  recreate_dir "$RECOVERED_MASKS_DIR/$TABLE_SOURCE"

  "$RECOVER_MASK" -e "$EVALUATED_CSVS_DIR" \
                  -o "$RECOVERED_MASKS_DIR/$TABLE_SOURCE" \
                  -c $((i+1)) \
                  "$TABLE_RANGES_FILE"
  "$COMPARE_MASKS" -g "$EXTRACTED_MASKS_DIR" \
                   -p "$RECOVERED_MASKS_DIR/$TABLE_SOURCE" \
                   -o "$FULL_COMPARISON_MAKS_DIR/$TABLE_SOURCE" \
  | tee "$DATA_DIR/full_formula_recovery_$TABLE_SOURCE.csv"
  "$COMPARE_MASKS" -g "$EXTRACTED_MASKS_DIR" \
                   -p "$RECOVERED_MASKS_DIR/$TABLE_SOURCE" \
                   -o "$TABLE_COMPARISON_MAKS_DIR/$TABLE_SOURCE" \
                   -c $((i+1)) \
                   -t "$TABLE_RANGES_FILE" \
  | tee "$DATA_DIR/table_formula_recovery_$TABLE_SOURCE.csv"
done
