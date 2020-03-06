## New Version Submission - v1.4.2

This is an updated version of 'ecotox'. In this version I have:

  * `LC_` and `LT_` functions now have warnings messages for not supplying a variable for the `weights` argument. This is due to only needing to supply if you are taking the response / total for your response variable within the formula call of `LC_probit`. Otherwise if you use `cbind(response, non-response)` method you do not need to supply weights. If you do the model will be incorrect. If you don't supply `weights` there is a warning that will help you to make sure you are using one method or the other.
  
  * This stackExchange question explains the differences to using `cbind()` vs. `respone / total` method. [cbind() function in R for a logistic regression](https://stats.stackexchange.com/questions/259502/in-using-the-cbind-function-in-r-for-a-logistic-regression-on-a-2-times-2-t)
 
     
## Test environments
* local Windows 10, R 3.6.3
* ubuntu 14.04 (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 


## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
