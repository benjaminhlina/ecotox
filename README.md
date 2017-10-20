[![Build Status](https://travis-ci.org/benjaminhlina/ecotox.svg?branch=master)](https://travis-ci.org/benjaminhlina/ecotox)

# ecotox
[ecotox](https://github.com/benjaminhlina/ecotox) was created as simple approach to using a probit analysis to calculate lethal concentration (LC) or time (LT) and the appropriate fiducial confidence limits desired for selected LC or LT for ecotoxicology studies (Finney 1971; Wheeler et al. 2006; Robertson et al. 2007). The simplicity of [ecotox](https://github.com/benjaminhlina/ecotox) comes from the syntax it implies within its functions which is similar to functions like [glm()](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html) and [lm()](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html). In addition to the simplicity of the syntax, a comprehensive data frame is produced which gives the user a predicted LC or LT value for the desired level and a suite of important parameters such as fiducial confidence limits and slope.
[ecotox](https://github.com/benjaminhlina/ecotox)  ecotox was built for and is published in Hlina et al. *In Preparation*. 


## Installation

You can install ecotox from github with:


``` r
install.packages("devtools")
devtools::install_github("benjaminhlina/ecotox")
```

or 

``` r
source("https://raw.githubusercontent.com/MangoTheCat/remotes/master/install-github.R")$value("mangothecat/remotes")
remotes::install_github("benjaminhlina/ecotox")
```

## Example

This is an example which uses the LC function to calculate a LC<sub>50</sub> and LC<sub>99</sub>: 


``` r
## Calculate LC50 and LC99
head(lampreytox)

 ## within the dataframe used, control dose,
 ## unless produced a value during experimentation,
 ## are adjusted to 0.01, as glm cannot handle values of infinite.

 ## calculate LC50 and LC99 for May

 m <- LC((dead / total) ~ log10(dose), p = c(50, 99),
          weights = lampreytox[c(1:19), ]$total,
          data = lampreytox[c(1:19), ])

 ## view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
 ## 3-trifluoromethyl-4-nitrophenol (TFM) to lamprey in 2011

 m
```

## References 

Finney, D.J., 1971. Probit Analysis, Cambridge University Press, Cambridge, England. ISBN: 052108041X
 
Wheeler, M.W., Park, R.M., and Bailey, A.J. 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444. [10.1897/05-320R.1](http://onlinelibrary.wiley.com/doi/10.1897/05-320R.1/abstract)

Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press. ISBN: 0849323312

## Citation 

version 1.0.1. Written by Benjamin L. Hlina, Wilfrid Laurier University, Waterloo, Ontario, Canada. Written in [Programming Language R](https://www.r-project.org/), version 3.4.2 (2017-09-28) -- "Short Summer". Run on a PC with Intel(R) Core(TM) I7- Q820 CPU, 1.73 GHz processor, 14.0 GB RAM, and Microsoft Windows 7 Professional operating system, 2009 Service Pack 1. Source code is available at [ecotox](https://github.com/benjaminhlina/ecotox) or by contacting Benjamin L. Hlina at benjamin.hlina@gmail.com

When using this package please cite the following  publication: 

Hlina, B.L., Birceanu, O., Tessier, L.R., Robinson, C.,  Muhametsafina, A., Bragg, L.M, Servos, M.R., Wilkie, M.P., *In Preparation*. Changes in the sensitivity of piscicide in an invasive species. Environmental Science & Technology.
