


## Test Results


### OSX

    R CMD check --as-cran RcppDynProg_0.1.4.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘RcppDynProg/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘RcppDynProg’ version ‘0.1.4’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    devtools::check_win_devel()


    rhub::check_for_cran()
    431#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    432#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    433#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    434#> setting R_REMOTES_STANDALONE to true
    435#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    436#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    437#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    438#> * using log directory 'C:/Users/USERyiIXrbTSFz/RcppDynProg.Rcheck'
    439#> * using R Under development (unstable) (2020-07-05 r78784)
    440#> * using platform: x86_64-w64-mingw32 (64-bit)
    441#> * using session charset: ISO8859-1
    442#> * using option '--as-cran'
    443#> * checking for file 'RcppDynProg/DESCRIPTION' ... OK
    444#> * checking extension type ... Package
    445#> * this is package 'RcppDynProg' version '0.1.4'
    446#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    447#> Maintainer: 'John Mount '    
    522#> Status: OK
    
## Downstream dependencies

    No dependent packages (please see https://github.com/WinVector/RcppDynProg/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.

