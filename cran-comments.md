


## Test Results

### Linux

    R CMD check --as-cran RcppDynProg_0.1.2.tar.gz 
    * using R version 3.5.3 (2019-03-11)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: 1 WARNING
    Non-portable compliation flag part of local check, not part of package.

### OSX

    R CMD check --as-cran RcppDynProg_0.1.2.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::build_win()
 

## Downstream dependencies

    No dependent packages (please see https://github.com/WinVector/RcppDynProg/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.

