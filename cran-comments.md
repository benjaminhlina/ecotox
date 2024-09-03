## New Version Submission - v1.4.4

This is an updated version of 'ecotox'. In this version I have:

  * `LC_` and `LT_` functions now determine both the standard error similar to how `predict()` works
  as well as the covariance of the model to be used by `ratio_test()`
  * `ratio_test()` was discovered to be inaccurately calculating test statistic and p value 
  when `log_x = FALSE`. This has been corrected when determining t by taking the log of each doses
  * citation for the package has been updated as the paper the package was writing for has now been published
 
     
## Test environments
* local Windows 10, R 4.1.1
* ubuntu 14.04 (on travis-ci), R 4.1.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 


## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
