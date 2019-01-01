


## Test Results

### Linix

    R CMD check --as-cran RcppDynProg_0.1.0.tar.gz 
    * using R version 3.5.1 (2018-07-02)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.0’
    * checking CRAN incoming feasibility ...
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    New submission
    * checking compilation flags used ... WARNING
    Compilation used the following non-portable flag(s):
      ‘-Wdate-time’ ‘-Werror=format-security’ ‘-Wformat’
    Status: 1 WARNING, 1 NOTE
    Package is correct: compiler flags part of system configuration, not package configuration.

    rhub::check_for_cran()
    1464#> * using R version 3.4.4 (2018-03-15)
    1465#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1466#> * using session charset: UTF-8
    1467#> * using option ‘--as-cran’
    1468#> * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    1469#> * checking extension type ... Package
    1470#> * this is package ‘RcppDynProg’ version ‘0.1.0’
    1471#> * checking CRAN incoming feasibility ... NOTE
    1472#> Maintainer: ‘John Mount ’
    1473#> New submission
    1528#> Status: 1 NOTE

### OSX

    R CMD check --as-cran RcppDynProg_0.1.0.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.0’
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    New submission
    Status: 1 NOTE

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-12-31 r75935)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'RcppDynProg/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'RcppDynProg' version '0.1.0'
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    New submission
    Status: 1 NOTE
    
    rhub::check_for_cran()
    485#> * using R Under development (unstable) (2018-12-26 r75909)
    486#> * using platform: x86_64-w64-mingw32 (64-bit)
    487#> * using session charset: ISO8859-1
    488#> * using option '--as-cran'
    489#> * checking for file 'RcppDynProg/DESCRIPTION' ... OK
    490#> * checking extension type ... Package
    491#> * this is package 'RcppDynProg' version '0.1.0'
    492#> * checking CRAN incoming feasibility ... NOTE
    493#> Maintainer: 'John Mount '
    494#> New submission
    567#> Status: 1 NOTE

## Downstream dependencies

    New package, no dependent packages.
     
Zumel is not a mis-spelling.

