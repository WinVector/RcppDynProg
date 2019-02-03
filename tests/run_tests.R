
if(requireNamespace("RUnit", quietly = TRUE)) {
  library("RUnit")
  library("RcppDynProg")
  # TODO: switch to wrapr version
  run_package_tests("RcppDynProg", verbose = TRUE)
}
