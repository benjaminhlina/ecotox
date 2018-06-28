## New Version Submission - v1.3.2
This is an updated version of 'ecotox'. In this version I have:

* All functions now have `log_x` argument which allows the user to calculate
  results on whether not the x variable is `log10` transformed. 
  Default is `TRUE` which will output results that have been back transfomred. 
* LT_logit was producing error as variance co-variance matrix was only being
  created if the analysis required a heterogenitity correction factor. 
  This has been fixed and a variance co-variance matrix is produced whether 
  or not a hetrogenity correction factor is needed. 
* Tests have been updated to be more comprehensive for 'ecotox'.

## Test environments
* local Windows 7, SP1 install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 


## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
