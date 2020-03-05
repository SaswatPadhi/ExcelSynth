ExcelSynth
<a href="https://microbadger.com/images/padhi/excelsynth"><img align="right" src="https://img.shields.io/microbadger/image-size/padhi/excelsynth.svg?style=flat&label=docker"></img></a>
==========

[![](https://img.shields.io/travis/SaswatPadhi/ExcelSynth/master.svg?logo=travis&style=popout&label=Travis+Build)][travis]
&nbsp;
[![](https://img.shields.io/docker/cloud/build/padhi/excelsynth.svg?logo=docker&style=popout&label=Docker+Image)][docker-hub]

An enumerative synthesizer for recovering Excel formulas from CSVs.

<table>
   <thead>
   <tr>
      <th align='center'>Input: CSV File</th>
      <th align='center'>Output: Formula Mask</th>
   </tr>
   </thead>
   <tbody>
      <tr>
         <td>
            <sub><pre lang='text'>
                <code>
Col 1  ,  Col 2  ,  Col 3  ,  Col 4  ,  Col 5
Row 2  ,  1.     ,  10.    ,  9.5    ,  24.
Row 3  ,  23.    ,  12.    ,  0.5    ,  35.
Row 4  ,  22.    ,  2.     ,  -9.    ,  24.
Row 5  ,  -1.    ,  6.     ,  6.5    ,   5.
Row 6  ,  59.    ,  0.     ,  -29.5  ,  41.
Row 7  ,  11.    ,  -2.    ,  -7.5   ,   9.
Row 8  ,  115.   ,  14.    ,  -43.5  ,  23.
                </code>
            </pre></sub>
         </td>
         <td>
            <sub><pre lang='text'>
                <code>
 ,             ,                       ,                   ,
 ,             ,                       , =(C2-(B2/(1.+1.)) ,
 ,             ,                       , =(C3-(B3/(1.+1.)) ,
 ,             ,                       , =(C4-(B4/(1.+1.)) ,
 ,             ,                       , =(C5-(B5/(1.+1.)) ,
 ,             ,                       , =(C6-(B6/(1.+1.)) ,
 ,             ,                       , =(C7-(B7/(1.+1.)) ,
 , =SUM(B2:B7) , =(SUM(C2:C7)/(1.+1.)) , =(C8-(B8/(1.+1.)) , =AVERAGE(E2:E7)
                </code>
            </pre></sub>
         </td>
      </tr>
   </tbody>
</table>

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

```text
$ dune exec bin/App.exe -- -h
Synthesize Excel formulas for a CSV file.

  App.exe [flag] ... FILENAME

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
  [-enable-booleans BOOLEAN]                 enable Boolean and conditional
                                             expressions
  [-log-path FILENAME]                       enable logging and output to the
                                             specified path
  [-mask-path FILENAME]                      a known formula mask for the CSV
                                             file
  [-max-expr-size INTEGER]                   maximum cost (AST size) of
                                             expressions to explore
  [-max-threads INTEGER]                     maximum number of threads to create
  [-range STRING]                            a range (in RC:R'C' format) that
                                             bounds the synthesis space
  [-relative-error BOOLEAN]                  synthesize pointwise
                                             transformations for rows
  [-restrict-to-top-left-data BOOLEAN]       only use data to the top left of a
                                             cell in formulas
  [-type-error-threshold FLOAT]              maximum fraction of cells that may
                                             be ignored due to type errors
  [-value-error-threshold FLOAT]             maximum fraction of cells that may
                                             be ignored due to value errors
```

### Bulk Processing

The following directory structure is recommended:

```text
data/
|
+-- table_ranges.csv           <--- Contains table ranges for CSV files
|
+-- evaluated_csvs/
|    |
|    +-- a.csv
|    |
|    `-- b.csv
|
+-- extracted_masks/           <--- Generated by scripts/extract_mask.py
|    |
|    +-- a.csv
|    |
|    `-- b.csv
|
+-- formula_csvs/
|    |
|    +-- a.csv
|    |
|    `-- b.csv
|
+-- fp_diff_masks/             <--- Generated by scripts/compare_mask.py
|    |
|    +-- table_detector_1/
|    |    |
|    |    +-- a.csv
|    |    |
|    |    `-- b.csv
|    |
|    `-- table_detector_2/
|         |
|         +-- a.csv
|         |
|         `-- b.csv
|
`-- recovered_masks/           <--- Generated by scripts/recover_mask.py
     |
     +-- table_detector_1/
     |    |
     |    +-- a.csv
     |    |
     |    `-- b.csv
     |
     `-- table_detector_2/
          |
          +-- a.csv
          |
          `-- b.csv

```

#### Extract A Formula Masks from CSVs

```text
$ python3 scripts/extract_mask.py -h
usage: extract_mask.py [-h] -i INPUT_DIR -o OUTPUT_DIR

optional arguments:
  -h, --help            show this help message and exit

  -i INPUT_DIR, --input-dir INPUT_DIR
  -o OUTPUT_DIR, --output-dir OUTPUT_DIR

$ python3 scripts/extract_mask.py -i data/formula_csvs -o data/extracted_masks
```

#### Recover Formula Masks from CSVs

```text
$ python3 scripts/recover_mask.py -h
usage: recover_mask.py [-h] -e EVAL_CSV_DIR -o OUTPUT_DIR -c TABLE_RANGE_COLUMN
                       tables_data_csv

positional arguments:
  tables_data_csv

optional arguments:
  -h, --help            show this help message and exit

  -e EVAL_CSV_DIR, --eval-csv-dir EVAL_CSV_DIR
  -o OUTPUT_DIR, --output-dir OUTPUT_DIR
  -c TABLE_RANGE_COLUMN, --table-range-column TABLE_RANGE_COLUMN

$ python3 scripts/recover_mask.py -e data/evaluated_csvs -o data/recovered_masks \
                                  -c 1 data/table_ranges.csv
```

`tables_data_csv` has extracted table ranges in `TABLE_RANGE_COLUMN` (0-indexed).

#### Compare Formula Masks

```text
$ python3 scripts/compare_masks.py -h
usage: compare_masks.py [-h] -g GROUND_TRUTH_DIR -p PREDICTION_DIR -o FP_OUTPUT_DIR

optional arguments:
  -h, --help            show this help message and exit

  -g GROUND_TRUTH_DIR, --ground-truth-dir GROUND_TRUTH_DIR
  -p PREDICTION_DIR, --prediction-dir PREDICTION_DIR
  -o FP_OUTPUT_DIR, --false-positive-output-dir FP_OUTPUT_DIR

$ python3 scripts/compare_mask.py -g data/extracted_masks -p data/recovered_masks/table_detector_1 -o data/fp_diff_masks/table_detector_1
```

[docker-hub]:         https://hub.docker.com/r/padhi/excelsynth
[travis]:             https://travis-ci.org/SaswatPadhi/ExcelSynth
