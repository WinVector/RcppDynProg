


## Test Results

### Linux

    R CMD check --as-cran RcppDynProg_0.1.1.tar.gz 
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.1’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking compilation flags used ... WARNING
    Compilation used the following non-portable flag(s):
      ‘-Wdate-time’ ‘-Werror=format-security’ ‘-Wformat’
    Status: 1 WARNING
    Non-portalbe compliation flag part of local check, not part of package.

    rhub::check_for_cran()

### OSX

    R CMD check --as-cran RcppDynProg_0.1.1.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.1’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::build_win()
    
    rhub::check_for_cran()

## Downstream dependencies

    No dependent packages.
    devtools::revdep()
    character(0)
     
Zumel is not a mis-spelling.

