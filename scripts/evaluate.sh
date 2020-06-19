#!/usr/bin/env bash

set -Eumo pipefail

if (( ${BASH_VERSION%%.*} < 4 )); then echo "ERROR: [bash] version >= 4.0 required!" ; exit -1 ; fi

ROOT="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)/.."

COMPARE_MASKS="scripts/compare_masks.py"
EXTRACT_MASK="scripts/extract_mask.py"
RECOVER_MASK="scripts/recover_mask.py"

ONLY_COMPARE="no"


recreate_dir() {
  rm -rf "$1"
  mkdir -p "$1"
}

verify () {
  if [ ! -"${1::1}" "$2" ]; then
    echo "'$2' $1 not found!"
    exit 1
  fi
}

usage() {
  if [ -n "$1" ]; then echo -e "\nERROR: $1" >&2 ; fi
  echo -en "
Usage: $0 [options] <path/to/data>
Flags:
    [--only-compare, -c]
" >&2 ; exit -1
}


for opt in "$@"; do
  shift
  case "$opt" in
    "--only-compare")   set -- "$@" "-c" ;;

    "--")               set -- "$@" "--" ;;
    "--"*)              usage "Unrecognized option: $opt." ;;
    *)                  set -- "$@" "$opt"
  esac
done

OPTIND=1
while getopts ':c' OPTION ; do
  case "$OPTION" in
    "c" ) ONLY_COMPARE="yes" ;;

      * ) usage "Unrecognized option: -$OPTARG." ;;
  esac
done
shift $(($OPTIND -1))


DATA_DIR=$(realpath "$1")


TABLE_RANGES_FILE="$DATA_DIR/table_ranges.csv"
verify file "$TABLE_RANGES_FILE"

EVALUATED_CSVS_DIR="$DATA_DIR/evaluated_csvs"
verify dir "$EVALUATED_CSVS_DIR"

FORMULA_CSVS_DIR="$DATA_DIR/formula_csvs"
verify dir "$FORMULA_CSVS_DIR"


EXTRACTED_MASKS_DIR="$DATA_DIR/extracted_masks"
RECOVERED_MASKS_DIR="$DATA_DIR/recovered_masks"
FULL_COMPARISON_MASKS_DIR="$DATA_DIR/comparison_masks/full"
TABLE_COMPARISON_MASKS_DIR="$DATA_DIR/comparison_masks/in-table"

if [ "$ONLY_COMPARE" == "yes" ]; then
  verify dir "$RECOVERED_MASKS_DIR"
else
  mkdir -p "$RECOVERED_MASKS_DIR"
fi

mkdir -p "$FULL_COMPARISON_MASKS_DIR"
mkdir -p "$TABLE_COMPARISON_MASKS_DIR"


cd "$ROOT"

TABLE_RANGES_HEADER=$(head -n 1 "$TABLE_RANGES_FILE")
IFS=',' read -ra TABLE_SOURCES <<< "$TABLE_RANGES_HEADER"

GT_COL=""
for ts in "${!TABLE_SOURCES[@]}"; do
  TABLE_SOURCE=$(echo "${TABLE_SOURCES[$ts]}" | sed 's/ *$//g')
  if [ "$TABLE_SOURCE" == "GT_Table" ]; then
    GT_COL="$ts"
    break
  fi
done

if [ -z "$GT_COL" ]; then
  echo "Ground truth tables column could not be located!"
  echo "Could not find column header 'GT_Table' in '$TABLE_RANGES_FILE'."
  exit 1
fi

TABLE_SOURCES=("${TABLE_SOURCES[@]:1}")

recreate_dir "$EXTRACTED_MASKS_DIR"

"$EXTRACT_MASK" -i "$FORMULA_CSVS_DIR" \
                -o "$EXTRACTED_MASKS_DIR" \
                -f "$DATA_DIR/filtered.txt" \
                -p "$DATA_DIR/formula_freq.png" \
  |& tee "$DATA_DIR/extraction.log"

recreate_dir "$FULL_COMPARISON_MASKS_DIR/Baseline"

if [ "$ONLY_COMPARE" != "yes" ]; then
  recreate_dir "$RECOVERED_MASKS_DIR/Baseline"

  "$RECOVER_MASK" -e "$EVALUATED_CSVS_DIR" \
                  -o "$RECOVERED_MASKS_DIR/Baseline" \
                  -f "$DATA_DIR/filtered.txt" \
                  -c "-1" \
                  "$TABLE_RANGES_FILE" \
    |& tee "$DATA_DIR/formula_recovery_Baseline.log"
fi

"$COMPARE_MASKS" -g "$EXTRACTED_MASKS_DIR" \
                 -p "$RECOVERED_MASKS_DIR/Baseline" \
                 -o "$FULL_COMPARISON_MASKS_DIR/Baseline" \
                 -r "$GT_COL" \
                 -t "$TABLE_RANGES_FILE" \
  | tee "$DATA_DIR/full_comparison_Baseline.csv"

for i in "${!TABLE_SOURCES[@]}"; do
  TABLE_SOURCE=$(echo "${TABLE_SOURCES[$i]}" | sed 's/ *$//g')

  recreate_dir "$FULL_COMPARISON_MASKS_DIR/$TABLE_SOURCE"
  recreate_dir "$TABLE_COMPARISON_MASKS_DIR/$TABLE_SOURCE"

  if [ "$ONLY_COMPARE" != "yes" ]; then
    recreate_dir "$RECOVERED_MASKS_DIR/$TABLE_SOURCE"

    "$RECOVER_MASK" -e "$EVALUATED_CSVS_DIR" \
                    -o "$RECOVERED_MASKS_DIR/$TABLE_SOURCE" \
                    -f "$DATA_DIR/filtered.txt" \
                    -c $((i+1)) \
                    "$TABLE_RANGES_FILE" \
      |& tee "$DATA_DIR/formula_recovery_$TABLE_SOURCE.log"
  fi

  "$COMPARE_MASKS" -g "$EXTRACTED_MASKS_DIR" \
                   -p "$RECOVERED_MASKS_DIR/$TABLE_SOURCE" \
                   -o "$FULL_COMPARISON_MASKS_DIR/$TABLE_SOURCE" \
                   -r "$GT_COL" \
                   -t "$TABLE_RANGES_FILE" \
    | tee "$DATA_DIR/full_comparison_$TABLE_SOURCE.csv"
  "$COMPARE_MASKS" -g "$EXTRACTED_MASKS_DIR" \
                   -p "$RECOVERED_MASKS_DIR/$TABLE_SOURCE" \
                   -o "$TABLE_COMPARISON_MASKS_DIR/$TABLE_SOURCE" \
                   -c $((i+1)) \
                   -r "$GT_COL" \
                   -t "$TABLE_RANGES_FILE" \
    | tee "$DATA_DIR/in-table_comparison_$TABLE_SOURCE.csv"
done
