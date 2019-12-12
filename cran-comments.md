## New Version Submission - v1.4.1
This is an updated version of 'ecotox'. In this version I have:

  * Test coverage is now 100% for all functions including `ratio_test()`
  * `LC_` and `LT_` functions now have warnings for not supplying a value for p
  * 'LC_` and `LT_` functions now have error messages for not supplying a variable for the `weights` argument. 
  * `ecotox` no longer relys on `ggplot2` as `ggplot2` was a dependency as the examples for `LC_` and `LT_` functions use `ggplot2` to plot the results. Instead these examples have now been commented. To run this part of the example just uncomment the lines. This was done as `ggplot2` does not need to be a dependency for `ecotox`. `tibble` and `stats` are now the only dependency with `stats` being the only truely necessary dependency for `ecotox`. `tibble` is being used instead of R's default dataframe as tibbles are more useful but not necssary for the anlysis. 
     
## Test environments
* local Windows 7, SP1 install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 


## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
