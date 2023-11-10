# ecotox <img src="man/figures/hex_sticker.png" align="right" width="120" />


[![R-CMD-check](https://github.com/benjaminhlina/ecotox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/benjaminhlina/ecotox/actions/workflows/R-CMD-check.yaml)[![CRAN status](http://www.r-pkg.org/badges/version-ago/ecotox)](https://CRAN.R-project.org/package=ecotox)[![Coverage status](https://codecov.io/gh/benjaminhlina/ecotox/branch/master/graph/badge.svg)](https://codecov.io/github/benjaminhlina/ecotox?branch=master) [![](http://cranlogs.r-pkg.org/badges/last-month/ecotox)](https://CRAN.R-project.org/package=ecotox)

## Overview
[{ecotox}](https://CRAN.R-project.org/package=ecotox) was created as a simple approach to using either a probit or logit analysis to calculate lethal concentration (LC) or time (LT) and the appropriate fiducial confidence limits desired for the selected LC or LT for ecotoxicology studies (Finney 1971; Wheeler et al. 2006; Robertson et al. 2007). The simplicity of [{ecotox}](https://CRAN.R-project.org/package=ecotox) comes from the syntax it implies within its functions which are similar to functions like [glm()](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html) and [lm()](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html). In addition to the simplicity of the syntax, a comprehensive tibble is produced which gives the user a predicted LC or LT value for the desired level and a suite of parameters such as fiducial confidence limits, z-value, and slope.
[{ecotox}](https://CRAN.R-project.org/package=ecotox) was built for and is published in [Hlina et al. 2021.](https://doi.org/10.1016/j.jglr.2021.10.002)


## Installation
You can install the CRAN released version of {ecotox} from CRAN with:

``` r 
install.packages("ecotox")
```


You can install the developer version of {ecotox} from github with:


``` r
install.packages("devtools")
devtools::install_github("benjaminhlina/ecotox")
```

## Example

This is an example which uses the LC_probit function to calculate a LC<sub>50</sub> and LC<sub>99</sub> for a probit analysis: 


``` r
## Calculate LC50 and LC99
head(lamprey_tox)

## within the dataframe used, control dose, unless produced a value
## during experimentation, are removed from the dataframe,
## as glm cannot handle values of infinite. Other statistical programs
## make note of the control dose but do not include within analysis. 

## calculate LC50 and LC99 for May

m <- LC_probit((response / total) ~ log10(dose),
               p = c(50, 99),
               weights = total,
               data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
               subset = c(month == "May"))

## view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
## 3-trifluoromethyl-4-nitrophenol (TFM) to lamprey in 2011

m

## several new features include 1) being able to change the output length 
## 2) you can indicate whether the x variable has been log10 transformed or 
## not if it has the output will take that into consideration

m_2 <- LC_probit((response / total) ~ dose,
                 p = c(50, 99),
                 weights = total,
                 data = lamprey_tox,
                 subset = c(month == "May"), 
                 log_x = FALSE, 
                 long_output = FALSE)
                  
## view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
## 3-trifluoromethyl-4-nitrophenol (TFM) to lamprey in 2011.

m_2

``` 
See StackExchange post about differences in using `cbind()` vs. `response / total` [cbind() function in R for a logistic regression](https://stats.stackexchange.com/questions/259502/in-using-the-cbind-function-in-r-for-a-logistic-regression-on-a-2-times-2-t).

``` r
## Additionally changes have been made to allow for the user 
## to use `cbind()` method when specificying the response variable  

m_3 <- LC_probit(cbind(response, survive) ~ log10(dose),
                 p = c(50, 99),
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "May"))
                  

m_3 

# notice that m and m_3 produce the same results, however m_3 will produce 
# a warning to ensure you have not weighted the model as it is not necessary 


```



Example of using `ratio_test` from [Wheeler et al. 2006](https://setac.onlinelibrary.wiley.com/doi/abs/10.1897/05-320R.1) to determine differences in LC values:


``` r

## A new function `ratio_test` has been added 

# view lamprey_tox data

head(lamprey_tox)

# using glm() to detemine LC values using probit model for May and June

m <- glm((response / total) ~ log10(dose),
         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
         subset = c(month == "May"),
         weights = total,
         family = binomial(link = "probit"))


j <- glm((response / total) ~ log10(dose),
         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
         subset = c(month == "June"),
         weights = total,
         family = binomial(link = "probit"))

# now that both May and June models have been made. use ratio_test to
# compare LC50 values or whatever LC values of interest.

ratios <- ratio_test(model_1 = m, model_2 = j, 
                     percentage = 50, 
                     compare = "May - June")

# view ratio test results

ratios

# you can also use LC_* or LT_* objects to create the models and use ratio test:

m_1 <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "May"))



j_1 <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "June"))



ratios_2 <- ratio_test(model_1 = m_1, model_2 = j_1, 
                       percentage = 50,
                       compare = "May - June", 
                       obj_type = "df")

ratios_2


``` 

## References 

* Finney, D.J., 1971. Probit analysis. Cambridge University Press, Cambridge, England. ISBN: 052108041X
 
* Wheeler, M.W., Park, R.M., and Bailey, A.J. 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests. Environ. Toxic. Chem. 25(5), 1441-1444. [10.1897/05-320R.1](https://setac.onlinelibrary.wiley.com/doi/abs/10.1897/05-320R.1)

* Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press. ISBN: 0849323312


* Version 1.4.4 written by Benjamin L. Hlina, Carleton University, Ottawa, Ontario, Canada. Written in ['Programming Language R'](https://www.r-project.org/), R version 4.1.1 (2021-08-10) -- "Kick Things". Source code is available at [{ecotox}](https://github.com/benjaminhlina/ecotox) or by contacting Benjamin L. Hlina at benjamin.hlina@gmail.com
