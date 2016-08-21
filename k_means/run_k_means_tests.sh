#!/bin/bash

cd tests

Rscript run_all_k_means_tests.R

cd ..

echo "Unit test execution complete"