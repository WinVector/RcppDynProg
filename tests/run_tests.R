
# Use Runit for tests.
# GPL3, based on https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R


# For all files with names of the form "^test_.+\\.R$" in the package directory unit_tests
# run all functions with names of the form "^test_.+$" as RUnit tests.

# name of the package we are testing
pkg <- "RcppDynProg"

if(requireNamespace("RUnit", quietly = TRUE) && requireNamespace(pkg, quietly = TRUE)) {
  library("RUnit")
  library(pkg, character.only = TRUE)
  set.seed(2019)  # try to make things a bit more deterministic
  print(paste("RUnit testing package", pkg, "version", packageVersion(pkg)))
  test_suite <- defineTestSuite(name = paste(pkg, "unit tests"),
                                dirs = system.file("unit_tests", package = pkg, mustWork = TRUE),
                                testFileRegexp = "^test_.+\\.R$",
                                testFuncRegexp = "^test_.+$")
  test_results <- runTestSuite(test_suite)
  printTextProtocol(test_results,
                    separateFailureList = TRUE,
                    showDetails = FALSE)
  # stop if errors for R CMD CHECK
  test_errors <- getErrors(test_results)
  if(test_errors$nDeactivated>0) {
    warning(paste("package", pkg, "has deactivated tests"))
  }
  if((test_errors$nFail>0) || (test_errors$nErr>0)) {
    stop(paste("package", pkg, "had test failures/errors"))
  }
  if(test_errors$nTestFunc<=0) { # catch packge test problem
    stop(paste("package", pkg, "had no tests"))
  }
}