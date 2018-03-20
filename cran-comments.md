## New Version Submission - v1.3.1.9000
This is an updated version of 'ecotox' . In this version I have:

* LC and LT have been removed
* All functions now have `subset` argument
* `Data` argument can now be just the dataframe of interest 
  no need to use brackets to subset 
* `Weights` is no longer an optional argument and can be specified 
    just by object name. Again no need to add the name of dataframe, 
    bracket to subset `[]` and `$` to select object used for `weights`. 
* Tests have been updated are now functional to testing the functions within 
  'ecotox'.

## Test environments
* local Windows 7, SP1 install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* local macOS 10.12.2, Sierra install, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs. 

* R CMD check returned 1 NOTE regarding an adjustment to the Maintainer's 
  name. I realized I'd like my middle initla, L, to be included which it 
  hasn't been in previous version on CRAN, Thanks. 

## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
