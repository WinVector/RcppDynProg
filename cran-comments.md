


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
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    New submission
    * checking compilation flags used ... WARNING
    Compilation used the following non-portable flag(s):
      ‘-Wdate-time’ ‘-Werror=format-security’ ‘-Wformat’
    Status: 1 WARNING, 1 NOTE
    
    Package is okay: compiler flags part of system configuration, not package configuration.


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
    * using R Under development (unstable) (2018-12-27 r75912)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'RcppDynProg/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'RcppDynProg' version '0.1.0'
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    New submission
    Possibly mis-spelled words in DESCRIPTION:
       Rcpp (3:8, 11:49)
    Status: 1 NOTE

## Downstream dependencies

    New package, no dependent packages.
     
Zumel is not a mis-spelling.

