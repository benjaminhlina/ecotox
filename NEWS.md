# ecotox 1.2.0.9000

### New Features as of 16 - Nov - 2017
  * Logit function for LC and LT has been added and can be called upon using 
    LClogit or LTlogit
  * LC and LT orignal functions have been renamed to LCprobit and LTprobit and 
    LC and LT function have been depreciated and eventually will be removed 
    from the package.
  
  
### New Features as of 08 - Nov - 2017
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
    * Parallism test is in development 
    * Shiny app 
    * video demo 
