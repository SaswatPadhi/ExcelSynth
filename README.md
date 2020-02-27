ExcelSynth
<a href="https://microbadger.com/images/padhi/excelsynth"><img align="right" src="https://img.shields.io/microbadger/image-size/padhi/excelsynth.svg?style=flat&label=docker"></img></a>
==========

[![](https://img.shields.io/travis/SaswatPadhi/ExcelSynth/master.svg?logo=travis&style=popout&label=Travis+Build)][travis]
&nbsp;
[![](https://img.shields.io/docker/cloud/build/padhi/excelsynth.svg?logo=docker&style=popout&label=Docker+Image)][docker-hub]

A simple enumerative synthesizer for recovering Excel formulas from CSVs.

----

## Instructions

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

```
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
  [-constants FLOAT] ...                     additional Boolean/numeric/string
                                             constants
  [-disable-constant-solutions BOOLEAN]      disable constant formulas (e.g.
                                             =0.0) for cells
  [-enable-2d-aggregation BOOLEAN]           use 2D ranges in aggregation
                                             operations
  [-log-path FILENAME]                       enable logging and output to the
                                             specified path
  [-mask-path FILENAME]                      a known formula mask for the CSV
                                             file
  [-max-threads INTEGER]                     maximum number of threads to create
  [-maximum-expression-cost INTEGER]         maximum cost (AST size) of
                                             expressions to explore
  [-restrict-to-top-left-data BOOLEAN]       only use data to the top left of a
                                             cell in formulas
  [-type-error-threshold FLOAT]              maximum fraction of cells that may
                                             be ignored due to type errors
```

[docker-hub]:         https://hub.docker.com/r/padhi/excelsynth
[travis]:             https://travis-ci.org/SaswatPadhi/ExcelSynth