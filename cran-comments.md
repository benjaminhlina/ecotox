## Test environments
* local Windows 7, SP1 install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* local macOS 10.12.2, Sierra install, R 3.4.2
* win-builder (devel and release)


## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTE:

*This is my first submission of package to CRAN.

*Malformed Description field: should contain one or more complete sentences.

The sentences are complete and the note is due to the formating of the Description field within 'DESCRIPTION' and the references I was asked to add into the field. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of ecotox
 
All packages that were installed passed. 
