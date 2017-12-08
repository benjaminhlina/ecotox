## New Version Submission - v1.3.0
This is an updated version of 'ecotox' . In this version I have:

* added new functions LC_logit and LT_logit
* LC and LT have been deprecated and are now LC_probit and LT_probit
* updated naming conventions for functions, function arguments, and 
  within functions object names. 
* added AppVeyor 

## Test environments
* local Windows 7, SP1 install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.3
* local macOS 10.12.2, Sierra install, R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies
I have ran R CMD check on downstream dependencies of 'ecotox'
 
All packages that were installed passed. 
