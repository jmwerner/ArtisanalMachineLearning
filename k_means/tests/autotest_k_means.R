# This script starts the autotesting capability

# This script is executed in the /tests directory and launched by run_autotest_k_means.sh

library(testthat)

test_path = getwd()
code_path = paste0(test_path, "/..")

auto_test(code_path = code_path, test_path = test_path)
