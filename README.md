# MONDO-SAM reporting

[![Build Status](https://travis-ci.org/FTSRG/mondo-sam-reporting.svg?branch=master)](https://travis-ci.org/FTSRG/mondo-sam-reporting)

Reporting tools for MONDO-SAM

## Prerequisites

Install R and the required packages.

### Linux

Install R 3.2+. On Ubuntu 15.10+, this is as simple as issuing:

```bash
sudo apt-get install -y r-base r-base-dev
sudo R -f install.R
```

On older Ubuntu systems, follow the [Ubuntu Packages for R](https://cran.r-project.org/bin/linux/ubuntu/README.html) guide. For other Linux systems, refer to the [CRAN Linux pages](https://cran.r-project.org/bin/linux/).

### Windows

Download and install [R](http://cran.r-project.org/bin/windows/base/) and adjust the system environment variables.

In a command prompt with administrator rights, run:

```bash
R -f install.R
```

To edit the script, it is recommended to use [RStudio](https://www.rstudio.com/).

## Usage

To draw a few simple plots, issue the following command:

```bash
./init.sh
./report.sh
```

The results should be the `results.csv` file. For a sample CSV file, open the `sample-results.csv` file with your favorite spreadsheet software or text editor.

The schema attributes are defined in the following list. For each attribute, we list an example from the [Train Benchmark](https://github.com/FTSRG/trainbenchmark) framework :steam_locomotive:.

* **Scenario**
  * defines the workflow of the benchmark
  * :steam_locomotive: the `Batch` scenario defines a workflow with a single read and query operation.
* **Tool**
  * defines the system under benchmark with it's custom settings
  * :steam_locomotive: `EMF-IncQuery (Local Search)`
* **Run**
  * specifies the index of the run
  * :steam_locomotive: there are usually 5 runs, so the **Run** attribute takes a value between `1` and `5`
* **Case**
  * defines the benchmark case
  * :steam_locomotive: the query `PosLength`
* **Artifact**
  * defines the artifact which used by the benchmark
  * :steam_locomotive: the model size used by the queries and transformations.
* **Phase**
  * defines the phase of the benchmark
  * :steam_locomotive: the `Read` phase loads the model
* **Metric**
  * the metric stored by the particular row
  * :steam_locomotive: the benchmark uses two metrics; `Time` (for measuring the execution time in nanoseconds) and `Matches`
* **Iteration**
  * defines the index of the current iteration if a step is executed multiple times
  * :steam_locomotive: for the `Transformation` and `Recheck` phases in the `Repair` scenario, the `Iteration` attribute takes a value between `1` and `10`
* **Value**
  * The value of the metric.
  * :steam_locomotive: for `Time`, a valid value is `456123123` (for an approximately half-second operation), for `Matches`, a valid value is `5`

## Related projects

* [MONDO SAM](https://github.com/FTSRG/mondo-sam)
* [Train Benchmark](https://github.com/FTSRG/trainbenchmark)
