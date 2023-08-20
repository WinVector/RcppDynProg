check_reverse_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "RcppDynProg"
packageVersion(package)
```

    ## [1] '0.2.1'

``` r
date()
```

    ## [1] "Sat Aug 19 17:16:44 2023"

``` r
parallelCluster <- NULL
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
#ncores <- parallel::detectCores()
#parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/RcppDynProg/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//RtmpouQ3oU"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)
```

    ## Error: No dependencies for RcppDynProg

``` r
mk_fn <- function(package, directory) {
  force(package)
  force(directory)
  function(i) {
    library("prrd")
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## Error: no such table: metadata

``` r
summariseQueue(package=package, directory=td)
```

    ## Error: no such table: metadata

``` r
setwd(orig_dir)
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
}
```
