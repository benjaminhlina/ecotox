## New Version Submission - v1.3.3
This is an updated version of 'ecotox'. In this version I have:

* All functions now have `long_output` argument which allows the user to change the   output from being long and including a suite of statistical values or short and     including the most important 7 variables. Default is `TRUE` which will output the   entire long output. 
* Outputs are no longer dataframes but instead are tibbles 
* Tests have been updated to be more comprehensive for 'ecotox'.

## Test environments
* local Windows 7, SP1 install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 


## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
