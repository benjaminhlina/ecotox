
[![Travis Build Status](https://travis-ci.org/benjaminhlina/ecotox.svg?branch=1.0.0

# ecotox

ecotox is a package of functions for the [!R programming language] (https://www.r-project.org/). ecotox was created to calculate lethal concentrations (LC) or times (LT) for ecotoxicology studies using a probit anlysis following methods by Finney (1971). 


## Installation

You can install ecotox from github with:


``` r
# install.packages("devtools")
devtools::install_github("benjaminhlina/ecotox")
```

or 

``` r
source("https://raw.githubusercontent.com/MangoTheCat/remotes/master/install-github.R")$value("mangothecat/remotes")
remotes::install_github("benjaminhlina/ecotox")
```

## Example

This is a basic example which shows you how to solve a common problem:


``` r
## basic example code
head(lampreytox)

 ## within the dataframe used, control dose,
 ## unless produced a value during experimentation,
 ## are adjusted to 0.01, as glm cannot handle values of infinite.

 ## calculate LC50 and LC99 for May

 m <- LC((dead / total) ~ log10(dose), p = c(50, 99),
          weights = lampreytox[c(1:19), ]$total,
          data = lampreytox[c(1:19), ])

 ## view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
 ## to lamprey in 2011

 m
