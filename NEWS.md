# ecotox v1.4.1


### New Feature as of 04-April-2018
  
  * `type` is now null with default being `"probit"` can be `"logit"` if specified
  * `compare` argument has been added. This adds a compare column to the output letting        the user know what models are being compared. Current default stats `"Model 1 - Model 2"`. However, you can specificy this with a vector of characters like `compare = c("May - June")`

### New Feature as of 26-Mar-2018
  *  New function, ratio_test. This function allows the user to caculate                differences between two seperate LC or LT values using a ratio test derived        from Wheeler et al. 2006. To use the function, the user needs to create two 
     `glm()` objects with either a probit or logit link. Model objects then can be      passed to `ratio_test()` along with a `percent` argument which is the same         argument as the `p` argument in `LC_` and `LT_` functions. A `type` argument       is also needed to specify the link function used in the `glm()`. This can be       either `"probit"` or `"logit"`. 
  *  All functions now output a `tibble` instead of dataframe
  *  New argument `log_base` for all LC, LT, and ratio_test functions. It
     allows the user to specifcy the base used when log transfoming the dose/x          variable. 
  *  All functions now allow for an LC or LT value to be calculated down to the 
     1.0e-16 of a percent however four decmial places is typically appropriate          (e.g. LC75.5). Tibble output will usually round the p value to the nearest         whole number but if you insepect the p column or export to excel using             `openxlsx` package you'll see p in the output is the same value supplied in        the function call. 

### New Feature as of 20-Sep-2018
   * All functions now have `long_output` argument which allows the user to
     decide whether or not a short or long output is displayed. Default is `TRUE`      which will display long output of all 19 variables. If `FALSE` is used only 7      variables will be displayed.  
     
### New Feature as of 24-Jun-2018
  * All functions now have `log_x` argument which allows the user to specify if 
    the x variable was `log10` transformed or not. Default is `TRUE` which will 
    output back transformed results. If `FALSE` results will not be back 
    transformed. 
  * LT_logit was producing error as variance co-variance matrix was only being 
    created if the analysis required a heterogenitity correction factor.
    This has been fixed and a variance co-variance matrix is produced whether 
    or not a hetrogenity correction factor is needed. 
  * Tests have been updated to be more comprehensive for 'ecotox'

### New Feature as of 9-Feb-2018
  * All functions now have `subset` argument
  * `Data` argument can now be just the dataframe of interest 
    no need to use brackets to subset 
  * `Weights` is no longer an optional argument and can be specified 
    just by object name. Again no need to add the name of dataframe, 
    bracket to subset `[]` and `$` to select object used for `weights`. 
  * `LC_probit` and `LC_logit` and `LT_probit` and `LT_logit` might be 
    depreciated as `lc_probit` and `lc_logit` and `lt_probit` and `lt_logit` 
    are simpler function calls for users and are consistent with
    naming conventions. Thoughts?? 
    
    
### Suggested Features 

* `ecotox` has now been up in operation for about 3 months. I've made some recent improvements, e.g. `subset` feature, and am still working on a `spearman_karber` function which I hope to implement within the next month or so. I have three other functions I want to implement: 

* 1) Ratio comparison test for comparing LCs and LTs following Wheeler et al. 2006 
* 2) A parallelism test following Robertson et al. 2007
* 3) A equality test following Robertson et al. 2007. 

What features or functions do people want to see? What do people think of current functions and those that are currently under development? 

### New Features as of 2-Dec-2017
  * Probit and logit function name conventions have been modified. To call a
    probit analysis use LC_probit or LT_probit. For logit use LC_logit or 
    LC_probit.
  * Agruments within the functions and objects within the function have been
    renamed based on new naming convention. het.seg is now het_sig and 
    conf.level is now conf_level. 
  * Added tests to package 

### New Features as of 16-Nov-2017
  * Logit function for LC and LT has been added and can be called upon using 
    LClogit or LTlogit
  * LC and LT orignal functions have been renamed to LCprobit and LTprobit and 
    LC and LT function have been depreciated and eventually will be removed 
    from the package.
  
  
### New Features as of 08-Nov-2017
  * 'ecotox' v1.1.0 is now available  on CRAN 
  * het.sig is a new argument added to the both LC and LT functions 
    it allows for the user to adjust the level of signficance that 
    is used to decided whether or not a hetrogenity factor is used when 
    calculating fiducial condfidnece limits. Default is 0.15 which is the
    same default used by SPSS 24 when doing a probit analysis. 
  * 'ecotox' v1.2.0.9000 DESCRIPTION file has been updated so the package now 
     depends on R (>= 3.3.2) instead of  R (>= 3.3.3) as this lead to users 
     who were using OS X Mavericks unable to use 'ecotox' as OS X Mavericks 
     can only run R v3.3.2 and less. 
     
### Development 
  * Spearman-Karber method is in development  
  * Parallism test and equality test is in development 
  * Shiny app 
