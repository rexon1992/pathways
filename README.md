
<!-- README.md is generated from README.Rmd. Please edit that file -->
pathways
========

This package contains functions to process data for pathways. It contains functions to generate transition file and to transition from model indicator names to default indicator names. It also contains other functions to process data initially like delete rows, fill cells, find text etc.

Installation
------------

You can install pathways from github with:

``` r
# install.packages("devtools")
devtools::install_github("rexon1992/pathways")
```

Example
-------

This is a basic example which how you can use the transition function in the package:

``` r
library(pathways)
transition("example_inputs/CPM_raw_data","example_inputs/CMP_TR",notes = 1,"example_inputs/ind_list_production.xlsx")
```

This command will write a datafile into the directory called CPM\_raw\_data\_UL.csv with the model indicator names replaced by default indicator names and as the notes attribute is set to 1 it will also write a file called CPM\_raw\_data\_notes.csv into the directory which can be used to add conversion factors and notes for the indicators.