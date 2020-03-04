ExcelSynth
<a href="https://microbadger.com/images/padhi/excelsynth"><img align="right" src="https://img.shields.io/microbadger/image-size/padhi/excelsynth.svg?style=flat&label=docker"></img></a>
==========

[![](https://img.shields.io/travis/SaswatPadhi/ExcelSynth/master.svg?logo=travis&style=popout&label=Travis+Build)][travis]
&nbsp;
[![](https://img.shields.io/docker/cloud/build/padhi/excelsynth.svg?logo=docker&style=popout&label=Docker+Image)][docker-hub]

A simple enumerative synthesizer for recovering Excel formulas from CSVs.

----

## Installation

0. [Get `docker` for your OS](https://docs.docker.com/install).
1. Pull the docker image<sup>[#](#note_1)</sup>: `docker pull padhi/excelsynth`.
2. Run a container over the image: `docker run -it padhi/excelsynth`.<br>
   This would give you a `bash` shell within ExcelSynth directory.
3. To run ExcelSynth on `samples/unit_test.csv`, execute: `dune exec bin/App.exe -- samples/unit_test.csv`
4. To run the unit tests, execute: `dune runtest`

<a name="note_1"><sup>#</sup></a> Alternatively, you could also build the Docker image locally:

```bash
docker build -t padhi/excelsynth github.com/SaswatPadhi/ExcelSynth
```

## Usage

### Formula Synthesis from CSV

```bash
$ dune exec bin/App.exe -- -h
Synthesize Excel formulas for a CSV file.

  App.exe FILENAME

=== flags ===

  [-check-last-col-aggregations BOOLEAN]     synthesize aggregation formulas for
                                             cells in the last column
  [-check-last-row-aggregations BOOLEAN]     synthesize aggregation formulas for
                                             cells in the last row
  [-check-pointwise-col-operations BOOLEAN]  synthesize pointwise
                                             transformations for columns
  [-check-pointwise-row-operations BOOLEAN]  synthesize pointwise
                                             transformations for rows
  [-constant FLOAT] ...                      additional Boolean/numeric/string
                                             constants
  [-disable-constant-solutions BOOLEAN]      disable constant formulas (e.g.
                                             =0.0) for cells
  [-enable-2d-aggregation BOOLEAN]           use 2D ranges in aggregation
                                             operations
  [-log-path FILENAME]                       enable logging and output to the
                                             specified path
  [-mask-path FILENAME]                      a known formula mask for the CSV
                                             file
  [-max-expr-size INTEGER]                   maximum cost (AST size) of
                                             expressions to explore
  [-max-threads INTEGER]                     maximum number of threads to create
  [-range STRING]                            a range (in RC:R'C' format) that
                                             bounds the synthesis space
  [-restrict-to-top-left-data BOOLEAN]       only use data to the top left of a
                                             cell in formulas
  [-type-error-threshold FLOAT]              maximum fraction of cells that may
                                             be ignored due to type errors
```

### Bulk Processing

#### Extract A Formula Masks from CSVs

```bash
$ python3 scripts/extract_mask.py -h
usage: extract_mask.py [-h] -i INPUT_DIR -o OUTPUT_DIR

optional arguments:
  -h, --help            show this help message and exit
  -i INPUT_DIR, --input-dir INPUT_DIR
  -o OUTPUT_DIR, --output-dir OUTPUT_DIR
```

#### Recover Formula Masks from CSVs

```bash
$ python3 scripts/recover_mask.py -h
usage: recover_mask.py [-h] -e EVAL_CSV_DIR -o OUTPUT_DIR -c
                       TABLE_RANGE_COLUMN
                       tables_data_csv

positional arguments:
  tables_data_csv

optional arguments:
  -h, --help            show this help message and exit
  -e EVAL_CSV_DIR, --eval-csv-dir EVAL_CSV_DIR
  -o OUTPUT_DIR, --output-dir OUTPUT_DIR
  -c TABLE_RANGE_COLUMN, --table-range-column TABLE_RANGE_COLUMN
```

`tables_data_csv` has extracted table ranges in `TABLE_RANGE_COLUMN`.

#### Compare Formula Masks

```bash
$ python3 scripts/compare_masks.py -h
usage: compare_masks.py [-h] -g GROUND_TRUTH_DIR -p PREDICTION_DIR

optional arguments:
  -h, --help            show this help message and exit
  -g GROUND_TRUTH_DIR, --ground-truth-dir GROUND_TRUTH_DIR
  -p PREDICTION_DIR, --prediction-dir PREDICTION_DIR
```

[docker-hub]:         https://hub.docker.com/r/padhi/excelsynth
[travis]:             https://travis-ci.org/SaswatPadhi/ExcelSynth