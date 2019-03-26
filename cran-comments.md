## New Version Submission - v1.4.0
This is an updated version of 'ecotox'. In this version I have:

  *  New function, ratio_test. This function allows the user to caculate                differences between two seperate LC or LT values using a ratio test derived        from Wheeler et al. 2006. To use the function, the user needs to create two 
     `glm()` objects with either a probit or logit link. Model objects then can be      passed to `ratio_test()` along with a `percent` argument which is the same         argument as the `p` argument in `LC_` and `LT_` functions. A `type` argument       is also needed to specify the link function used in the `glm()`. This can be       either `"probit"` or `"logit"`. 
  *  New argument `log_base` for all LC, LT, and ratio_test functions, 
     allows the user to specifcy the base used when transfoming the dose/x              variable. 
  *  All functions now allow for an LC or LT value to be calculated down to the 
     1.0e-16 of a percent however four decmial places is typically appropriate          (e.g. LC75.5). `Tibble` output will usually round the p value to the nearest         whole number but if you insepect the p column or export to excel using             `openxlsx` package you'll see p in the output is the same value supplied in        the function call. 
     
## Test environments
* local Windows 7, SP1 install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 


## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
