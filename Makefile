#!/usr/bin/env r

library(remake)
make(remake_file="data.yaml")
make(remake_file="munge.yaml")
make(remake_file="figures_R.yaml")
make(remake_file="layout.yaml")
