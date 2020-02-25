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


[docker-hub]:         https://hub.docker.com/r/padhi/excelsynth
[travis]:             https://travis-ci.org/SaswatPadhi/ExcelSynth