


## Test Results

### Linix

    R CMD check --as-cran RcppDynProg_0.1.0.tar.gz 
    * using log directory ‘/home/john/Documents/work/RcppDynProg.Rcheck’
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

 
### Windows

    devtools::build_win()
 

## Downstream dependencies

    New packate, no dependent packages.
     
Zumel is not a mis-spelling.

