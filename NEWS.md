# ecotox v1.3.1

### New Feature as of 9-Feb-2018
  * All functions now have `subset` argument
  * `Data` argument can now be just the dataframe of interest 
    no need to use brackets to subset 
  * `Weights` is no longer an optional argument and can be specified 
    just by object name. Again no need to add name of dataframe, 
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
