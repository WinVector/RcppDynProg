Timings
================

``` r
knitr::opts_chunk$set(fig.width=12, fig.height=8) 
library("RcppDynProg")
library("WVPlots")
library("microbenchmark")
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
set.seed(2018)
n <- 500
x <- matrix(runif(n*n), nrow=n, ncol=n)

solve_dynamic_program(x, n)
```

    ## [1]   1 109 230 267 501

``` r
solve_dynamic_program_R(x, n)
```

    ## [1]   1 109 230 267 501

``` r
timings <- microbenchmark(
  solve_dynamic_program(x, n),
  solve_dynamic_program_R(x, n),
  times = 5L)

print(timings)
```

    ## Unit: milliseconds
    ##                           expr         min          lq        mean
    ##    solve_dynamic_program(x, n)    59.68229    59.99435    60.17219
    ##  solve_dynamic_program_R(x, n) 20892.04221 20939.13869 21056.85710
    ##       median         uq         max neval
    ##     59.99515    60.1898    60.99935     5
    ##  21067.49815 21112.6715 21272.93493     5

``` r
p <- data.frame(timings)
p$seconds <- p$time/1e+9
p$method <- as.factor(p$expr)
p$method <- reorder(p$method, p$seconds)

summary <- p %.>%
  project(., 
          mean_seconds = mean(seconds),
          groupby = "method")
print(summary)
```

    ##                           method mean_seconds
    ## 1: solve_dynamic_program_R(x, n)  21.05685710
    ## 2:   solve_dynamic_program(x, n)   0.06017219

``` r
ratio <- max(summary$mean_seconds)/min(summary$mean_seconds)
print(ratio)
```

    ## [1] 349.9433

``` r
WVPlots::ScatterBoxPlotH(p, 
                         "seconds", "method", 
                         "performance of same dynamic programming code in R and Rcpp (C++)")
```

![](Timings_files/figure-markdown_github/unnamed-chunk-1-1.png)
