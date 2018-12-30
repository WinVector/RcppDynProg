Time xlin\_fits
================

``` r
library("RcppDynProg")
library("WVPlots")
library("ggplot2")
library("microbenchmark")
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
set.seed(2018)

# data
mk_data <- function(g) {
  d <- data.frame(
    x = 0.05*(1:(3*g))) # ordered in x
  n <- nrow(d)
  d$y_ideal <- sin((0.3*d$x)^2)
  d$y_observed <- d$y_ideal + 0.25*rnorm(n)
  d
}

d <- mk_data(100)
n <- nrow(d)
w <- 1 + numeric(n)


timings <- microbenchmark(
  Rcpp = xlin_fits(d$x, d$y_observed, w, 0, n-1),
  R_lm = xlin_fits_lm(d$x, d$y_observed, w),
  R_rowwise = xlin_fits_R(d$x, d$y_observed, w),
  R_vectorized = xlin_fits_V(d$x, d$y_observed, w),
  times = 10L)

print(timings)
```

    ## Unit: microseconds
    ##          expr        min         lq        mean      median         uq
    ##          Rcpp     36.282     37.419     54.8018     44.2390     52.788
    ##          R_lm 273342.048 276354.603 277932.4433 277006.5190 279049.091
    ##     R_rowwise    342.044    348.123    430.8201    352.7515    362.012
    ##  R_vectorized     34.362     34.997     66.2965     39.5390     46.536
    ##         max neval
    ##     149.662    10
    ##  285984.639    10
    ##    1133.750    10
    ##     302.478    10

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

    ##          method mean_seconds
    ## 1:    R_rowwise 0.0004308201
    ## 2:         R_lm 0.2779324433
    ## 3:         Rcpp 0.0000548018
    ## 4: R_vectorized 0.0000662965

``` r
WVPlots::ScatterBoxPlotH(
  p, 
  "seconds", "method", 
  paste0("performance of PRESS statistic in R and Rcpp (n=", n, ")"))
```

<img src="Time_xlin_fits_files/figure-markdown_github/r2-1.png" style="display: block; margin: auto;" />

``` r
WVPlots::ScatterBoxPlotH(
  p, 
  "seconds", "method", 
  paste0("performance of PRESS statistic in R and Rcpp (n=", n, ")")) + 
  scale_y_log10()
```

<img src="Time_xlin_fits_files/figure-markdown_github/r2-2.png" style="display: block; margin: auto;" />

``` r
d <- mk_data(1000000)
n <- nrow(d)
w <- 1 + numeric(n)

# make data available for Python version
write.csv(d, file = gzfile("xlin_data.csv.gz"), row.names = FALSE, quote = FALSE)

timings <- microbenchmark(
  Rcpp = xlin_fits(d$x, d$y_observed, w, 0, n-1),
  R_rowwise = xlin_fits_R(d$x, d$y_observed, w),
  R_vectorized = xlin_fits_V(d$x, d$y_observed, w),
  times = 10L)

print(timings)
```

    ## Unit: milliseconds
    ##          expr       min        lq      mean    median        uq       max
    ##          Rcpp  234.5341  238.4055  255.8447  241.5019  243.9430  392.3449
    ##     R_rowwise 3312.1789 3333.1662 3368.6003 3366.9828 3378.1413 3456.8543
    ##  R_vectorized  187.8401  191.2530  348.7635  331.5121  369.0965  817.9080
    ##  neval
    ##     10
    ##     10
    ##     10

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

    ##          method mean_seconds
    ## 1:    R_rowwise    3.3686003
    ## 2:         Rcpp    0.2558447
    ## 3: R_vectorized    0.3487635

``` r
WVPlots::ScatterBoxPlotH(
  p, 
  "seconds", "method", 
  paste0("performance of PRESS statistic in R and Rcpp (n=", n, ")"))
```

<img src="Time_xlin_fits_files/figure-markdown_github/r2-3.png" style="display: block; margin: auto;" />

``` r
WVPlots::ScatterBoxPlotH(
  p, 
  "seconds", "method", 
  paste0("performance of PRESS statistic in R and Rcpp (n=", n, ")")) + 
  scale_y_log10()
```

<img src="Time_xlin_fits_files/figure-markdown_github/r2-4.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

Timings on a 2018 Dell XPS-13 laptop, 16 Gib RAM, LPDDR3, 2133 MT/s, Intel(R) Core(TM) i5-8250U CPU @ 1.60GHz (8 cores reported), idle, charged, and plugged into power supply. Ubuntu 18.04.1 LTS.

``` r
R.version.string
```

    ## [1] "R version 3.5.1 (2018-07-02)"

``` r
R.version
```

    ##                _                           
    ## platform       x86_64-pc-linux-gnu         
    ## arch           x86_64                      
    ## os             linux-gnu                   
    ## system         x86_64, linux-gnu           
    ## status                                     
    ## major          3                           
    ## minor          5.1                         
    ## year           2018                        
    ## month          07                          
    ## day            02                          
    ## svn rev        74947                       
    ## language       R                           
    ## version.string R version 3.5.1 (2018-07-02)
    ## nickname       Feather Spray

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 18.04.1 LTS
    ## 
    ## Matrix products: default
    ## BLAS: /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] rqdatatable_1.1.2    rquery_1.2.1         microbenchmark_1.4-6
    ## [4] ggplot2_3.1.0        WVPlots_1.0.7        RcppDynProg_0.1.0   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.0        sigr_1.0.3        pillar_1.3.0     
    ##  [4] compiler_3.5.1    plyr_1.8.4        bindr_0.1.1      
    ##  [7] tools_3.5.1       digest_0.6.18     lattice_0.20-38  
    ## [10] evaluate_0.12     tibble_1.4.2      gtable_0.2.0     
    ## [13] nlme_3.1-137      mgcv_1.8-26       pkgconfig_2.0.2  
    ## [16] rlang_0.3.0.1     Matrix_1.2-15     parallel_3.5.1   
    ## [19] yaml_2.2.0        xfun_0.4          bindrcpp_0.2.2   
    ## [22] gridExtra_2.3     withr_2.1.2       stringr_1.3.1    
    ## [25] dplyr_0.7.8       knitr_1.21        grid_3.5.1       
    ## [28] tidyselect_0.2.5  data.table_1.11.8 glue_1.3.0       
    ## [31] R6_2.3.0          rmarkdown_1.11    wrapr_1.8.2      
    ## [34] purrr_0.2.5       magrittr_1.5      scales_1.0.0     
    ## [37] htmltools_0.3.6   assertthat_0.2.0  colorspace_1.3-2 
    ## [40] labeling_0.3      stringi_1.2.4     lazyeval_0.2.1   
    ## [43] munsell_0.5.0     crayon_1.3.4
