#!/bin/bash

R -e "library(testthat);auto_test(\"../R\", getwd())"
