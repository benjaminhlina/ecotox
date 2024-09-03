# Description of LC_probit ----

#' Lethal Concentration Probit
#' @description Calculates lethal concentration (LC) and
#' its fiducial confidence limits (CL) using a probit analysis
#' according to Finney 1971, Wheeler et al. 2006, and Robertson et al. 2007.
#' @usage LC_probit(formula, data, p = NULL, weights = NULL,
#'           subset = NULL, log_base = NULL, log_x = TRUE,
#'           het_sig = NULL, conf_level = NULL, conf_type = NULL,
#'           long_output = TRUE)
#' @param formula an object of class `formula` or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under Details.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which `LC_probit` is called.
#' @param p Lethal Concentration (LC) values for given p, example will return a LC50 value if p equals 50. If more than one LC value wanted specify by creating a vector. LC values can be calculated down to the 1e-16 of a percentage (e.g. LC99.99). However, the tibble produced can round to nearest whole number.
#' @param weights vector of 'prior weights' to be used in the fitting process. Only needs to be supplied if you are taking the response / total for your response variable within the formula call of `LC_probit`. Otherwise if you use cbind(response, non-response) method you do not need to supply weights. If you do the model will be incorrect. If you don't supply weights there is a warning that will help you to make sure you are using one method or the other. See the following StackExchanges post about differences [cbind() function in R for a logistic regression](https://stats.stackexchange.com/questions/259502/in-using-the-cbind-function-in-r-for-a-logistic-regression-on-a-2-times-2-t) and [input format for binomial glm](https://stats.stackexchange.com/questions/322038/input-format-for-response-in-binomial-glm-in-r).
#' @param subset allows for the data to be subseted if desired. Default set to `NULL`.
#' @param log_base default is `10` and will be used to  calculate results using the anti of `log10()` given that the x variable has been `log10` transformed. If `FALSE` results will not be back transformed.
#' @param log_x default is `TRUE` and will calculate results using the antilog of determined by `log_base` given that the x variable has been `log()` transformed. If `FALSE` results will not be back transformed.
#' @param het_sig significance level from person's chi square goodness-of-fit test (pgof) that is used to decide if a heterogeneity factor is used. `NULL` is set to 0.15.
#' @param conf_level adjust confidence level as necessary or `NULL` set at 0.95.
#' @param conf_type default is `"fl"` which will calculate fudicial confidence limits per Finney 1971. If set to `"dm"` the delta method will be used instead.
#' @param long_output default is `TRUE` which will return a tibble with all 17 variables. If `FALSE` the tibble returned will consist of the p level, n, the predicted LC for given p level, lower and upper confidence limits.
#' @return Returns a tibble with predicted LC for given p level, lower CL (LCL), upper CL (UCL), Pearson's chi square goodness-of-fit test (pgof), slope, intercept, slope and intercept p values and standard error, and LC variance.
#' @references
#'
#' Finney, D.J., 1971. Probit Analysis, Cambridge University Press, Cambridge, England, ISBN: 052108041X
#'
#' Wheeler, M.W., Park, R.M., and Bailey, A.J., 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444.10.1897/05-320R.1
#'
#' Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press. ISBN: 9780849323317

#' @examples head(lamprey_tox)
#'
#' # within the dataframe used, control dose, unless produced a value
#' # during experimentation, are removed from the dataframe,
#' # as glm cannot handle values of infinite. Other statistical programs
#' # make note of the control dose but do not include within analysis
#'
#' # calculate LC50 and LC99
#'
#' m <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'                weights = total,
#'                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'                subset = c(month == "May"))
#' # OR
#'
#' m1 <- LC_probit(cbind(response, survive) ~ log10(dose), p = c(50, 99),
#'                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'                subset = c(month == "May"))
#'
#' # view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
#' # to lamprey in 2011
#'
#' m
#'
#' # these are the same
#'
#' m1
#'
#' # dose-response curve can be plotted using 'ggplot2'
#'
#' # library(ggplot2)
#'
#' # lc_may <- subset(lamprey_tox, month %in% c("May"))
#'
#' # p1 <- ggplot(data = lc_may[lc_may$nominal_dose != 0, ],
#' #              aes(x = log10(dose), y = (response / total))) +
#' #   geom_point() +
#' #   geom_smooth(method = "glm",
#' #               method.args = list(family = binomial(link = "probit")),
#' #               aes(weight = total), colour = "#FF0000", se = TRUE)
#'
#' # p1
#'
#' # calculate LC50s and LC99s for multiple toxicity tests, June, August, and September
#'
#' j <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'         subset = c(month == "June"))
#'
#' a <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'         subset = c(month == "August"))
#'
#' s <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'         subset = c(month == "September"))
#'
#' # group results together in a dataframe to plot with 'ggplot2'
#'
#' results <- rbind(m[, c(1, 3:8, 11)], j[,c(1, 3:8, 11)],
#'                  a[, c(1, 3:8, 11)], s[, c(1, 3:8, 11)])
#' results$month <- factor(c(rep("May", 2), rep("June", 2),
#'                           rep("August", 2), rep("September", 2)),
#'                         levels = c("May", "June", "August", "September"))
#'
#' # p2 <- ggplot(data = results, aes(x = month, y = dose,
#' #                              group = factor(p), fill = factor(p))) +
#' #   geom_col(position = position_dodge(width = 0.9), colour = "#000000") +
#' #   geom_errorbar(aes(ymin = LCL, ymax = UCL),
#' #                 size = 0.4, width = 0.06,
#' #                 position = position_dodge(width = 0.9))
#'
#' # p2
#' @import stats
#' @import tibble
#' @export

# Function  LC_probit ----
LC_probit <- function(formula, data, p = NULL,
                      weights = NULL,
                      subset = NULL, log_base = NULL,
                      log_x = TRUE,
                      het_sig = NULL,
                      conf_level = NULL,
                      conf_type = NULL,
                      long_output = TRUE) {

  model <- do.call("glm", list(formula = formula,
                               family = binomial(link = "probit"),
                               data = data,
                               weights = substitute(weights),
                               subset = substitute(subset)))

  # error message for missing weights argument in function call
  if(missing(weights)) {
    warning ("Are you using cbind(response, non-response) method as your y variable, if so do not weight the model. If you are using (response / total) method, model needs the total of test organisms per dose to weight the model properly",
             call. = FALSE)
  }




  # make p a null object and create warning message if p isn't supplied
  if (is.null(p)) {
    p <- seq(1, 99, 1)
    warning("`p`argument has to be supplied otherwise LC values for 1-99 will be displayed", call. = FALSE)
  }



  chi_square <- sum(residuals.glm(model, type = "pearson") ^ 2)

  df <- df.residual(model)

  pgof <- pchisq(chi_square, df, lower.tail = FALSE)


  # Extract slope and intercept SE, slope and intercept signifcance
  # z-value, & N

  summary <- summary(model)

  # Intercept (b0)

  b0 <- summary$coefficients[1]

  # Slope (b1)

  b1 <- summary$coefficients[2]

  # intercept info

  intercept_se <- summary$coefficients[3]
  intercept_sig <- summary$coefficients[7]

  # slope info

  slope_se <- summary$coefficients[4]
  slope_sig <- summary$coefficients[8]

  # z value
  z_value <- summary$coefficients[6]

  # sample size

  n <- df + 2

  # Calculate m for all LC levels based on probits
  # in est (Robertson et al., 2007, pg. 27; or "m" in Finney, 1971, p. 78)

  est <- qnorm(p / 100)
  m <- (est - b0) / b1

# Calculate heterogeneity to correct confidence intervals
  # according to Finney, 1971, (p.72, eq. 4.27; also called "h")
  # Heterogeneity correction factor is used if
  # pearson's goodness of fit test (pgof) returns a sigficance
  # value less than 0.150 (source: 'SPSS 24')
  if (is.null(het_sig)) {
    het_sig <- 0.150
  }

  if (pgof < het_sig) {
    het <- chi_square / df
  } else {
    het <- 1
  }


  # set confiidewnce limit type for calculating confidence limtis
  if (is.null(conf_type)) {
    conf_type <- c("fl")
  } else {

    conf_type <- c("dm")

  }

  if (conf_type == "fl") {







    # variances have to be adjusted for heterogenity
    # if pgof returns a signfacnce value less than 0.150
    # (Finney 1971 p 72; 'SPSS 24')

    # covariance matrix

    if (pgof < het_sig) {
      vcova <- vcov(model) * het
    } else {
      vcova <- vcov(model)
    }



    # Intercept variance

    var_b0 <- vcova[1, 1]

    # Slope variance

    var_b1 <- vcova[2, 2]

    # Slope & intercept covariance

    cov_b0_b1 <- vcova[1, 2]

    # Adjust distibution depending on heterogeneity (Finney, 1971,  p72,
    # t distubtion used instead of normal distubtion  with appropriate df
    # if pgof returns a signfacnce value less than 0.150
    # (Finney 1971 p 72; 'SPSS 24')

    if (is.null(conf_level)) {
      conf_level <- 0.95
    }

    t <- (1 - conf_level)

    t_2 <- (t / 2)

    if (pgof < het_sig) {
      tdis <- -qt(t_2, df = df)
    } else {
      tdis <- -qnorm(t_2)
    }

    # Calculate g (Finney, 1971, p 78, eq. 4.36) "With almost
    # all good sets of data, g will be substantially smaller
    # than 1.0 and ## seldom greater than 0.4."

    g <- (tdis ^ 2 * var_b1) / (b1 ^ 2)



    # Calculate correction of fiducial limits according to Fieller method
    # (Finney, 1971,# p. 78-79. eq. 4.35)
    # v11 = var_b1 , v22 = var_b0, v12 = cov_b0_b1

    cl_part_1 <- (g / (1 - g)) * (m + (cov_b0_b1 / var_b1))

    cl_part_2 <- (var_b0 + (2 *  cov_b0_b1 * m) + (m ^ 2 * var_b1) -
                    (g * (var_b0 - cov_b0_b1 ^ 2 / var_b1)))

    cl_part_3 <- (tdis / ((1 - g) * abs(b1))) * sqrt(cl_part_2)

    # Calculate the fiducial limit LCL=lower fiducial limit,
    # UCL = upper fiducial limit (Finney, 1971, p. 78-79. eq. 4.35)

    LCL <- (m + (cl_part_1 - cl_part_3))
    UCL <- (m + (cl_part_1 + cl_part_3))

  }

  # calculate standard error
  cf <- -cbind(1, m) / b1

  se_1 <- ((cf %*% vcov(model)) * cf) %*% c(1, 1)

  se_2 <- as.numeric(sqrt(se_1))

  # Calculate variance for m (Robertson et al., 2007, pg. 27)

  var_m <- (1 / (m ^ 2)) * (var_b0 + 2 * m * cov_b0_b1 +
                              m ^ 2 * var_b1)



  if (log_x == TRUE) {

    if(is.null(log_base)) {
      log_base <- 10
    }
    dose <- log_base ^ m
    LCL <- log_base ^ LCL
    UCL <- log_base ^ UCL
    se_2 <- log_base ^ se_2
  }

  if (log_x == FALSE) {
    dose <- m
    LCL <- LCL
    UCL <- UCL
    se_2 <- se_2

  }

  # Make a data frame from the data at all the different values
  if (long_output == TRUE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL,
                    se = se_2,
                    chi_square = chi_square,
                    df = df,
                    pgof_sig = pgof,
                    h = het,
                    slope = b1,
                    slope_se = slope_se,
                    slope_sig = slope_sig,
                    intercept = b0,
                    intercept_se = intercept_se,
                    intercept_sig = intercept_sig,
                    z = z_value,
                    var_m = var_m,
                    covariance = cov_b0_b1)
  }

  if (long_output == FALSE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL)
  }
  return(table)

}

# Description of LC_logit ----
#' Lethal Concentration Logit
#' @description Calculates lethal concentration (LC) and
#' its fiducial confidence limits (CL) using a logit analysis
#' according to Finney 1971, Wheeler et al. 2006, and Robertson et al. 2007.
#' @usage LC_logit(formula, data, p = NULL, weights = NULL,
#'          subset = NULL, log_base = NULL,
#'          log_x = TRUE, het_sig = NULL,
#'          conf_level = NULL, conf_type = NULL,
#'          long_output = TRUE)
#' @param formula an object of class `formula` or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under Details.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which `LC_logit` is called.
#' @param p Lethal Concentration (LC) values for given p, example will return a LC50 value if p equals 50. If more than one LC value wanted specify by creating a vector. LC values can be calculated down to the 1e-16 of a percentage (e.g. LC99.99). However, the tibble produced can round to nearest whole number.
#' @param weights vector of 'prior weights' to be used in the fitting process. Only needs to be supplied if you are taking the response / total for your response variable within the formula call of `LC_probit`. Otherwise if you use cbind(response, non-response) method you do not need to supply weights. If you do the model will be incorrect. If you don't supply weights there is a warning that will help you to make sure you are using one method or the other.See the following StackExchange post about differences [cbind() function in R for a logistic regression](https://stats.stackexchange.com/questions/259502/in-using-the-cbind-function-in-r-for-a-logistic-regression-on-a-2-times-2-t).
#' @param log_base default is `10` and will be used to  calculate results using the anti of `log10()` given that the x variable has been `log10` transformed. If `FALSE` results will not be back transformed.
#' @param log_x default is `TRUE` and will calculate results using the antilog of determined by `log_base` given that the x variable has been `log()` transformed. If `FALSE` results will not be back transformed.
#' @param subset allows for the data to be subseted if desired. Default set to `NULL`.
#' @param het_sig significance level from person's chi square goodness-of-fit test that is used to decide if a heterogeneity factor is used. `NULL` is set to 0.15.
#' @param conf_level adjust confidence level as necessary or `NULL` set at 0.95.
#' @param conf_type default is `"fl"` which will calculate fudicial confidence limits per Finney 1971. If set to `"dm"` the delta method will be used instead.
#' @param long_output default is `TRUE` which will return a tibble with all 17 variables. If `FALSE` the tibble returned will consist of the p level, n, the predicted LC for given p level, lower and upper confidence limits.
#' @return Returns a tibble with predicted LC for given p level, lower CL (LCL), upper CL (UCL), Pearson's chi square goodness-of-fit test (pgof), slope, intercept, slope and intercept p values and standard error, and LC variance.
#' @references
#'
#' Finney, D.J., 1971. Probit Analysis, Cambridge University Press, Cambridge, England, ISBN: 052108041X
#'
#' Wheeler, M.W., Park, R.M., and Bailey, A.J., 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444.10.1897/05-320R.1
#'
#' Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press. ISBN: 9780849323317

#' @examples head(lamprey_tox)
#'
#' # within the dataframe used, control dose, unless produced a value
#' # during experimentation, are removed from the dataframe,
#' # as glm cannot handle values of infinite. Other statistical programs
#' # make note of the control dose but do not include within analysis
#'
#'
#' # calculate LC50 and LC99 for May
#'
#' m <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'          weights = total,
#'          data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'          subset = c(month == "May"))
#'
#'
#' # OR
#'
#' m1 <- LC_logit(cbind(response, survive) ~ log10(dose), p = c(50, 99),
#'                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'                subset = c(month == "May"))
#'
#'
#' # view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
#' # to lamprey in 2011
#'
#' m
#'
#' # they are the same
#'
#' m1
#'
#' # dose-response curve can be plotted using 'ggplot2'
#' # Uncomment the below lines to run create plots
#'
#' # library(ggplot2)
#'
#' # lc_may <- subset(lamprey_tox, month %in% c("May"))
#'
#' # p1 <- ggplot(data = lc_may[lc_may$nominal_dose != 0, ],
#' #              aes(x = log10(dose), y = (response / total))) +
#' # geom_point() +
#' # geom_smooth(method = "glm",
#' #             method.args = list(family = binomial(link = "logit")),
#' #             aes(weight = total), colour = "#FF0000", se = TRUE)
#'
#' # p1
#'
#' # calculate LC50s and LC99s for multiple toxicity tests, June, August, and September
#'
#' j <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'         subset = c(month == "June"))
#'
#' a <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'         subset = c(month == "August"))
#'
#' s <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'         subset = c(month == "September"))
#'
#' # group results together in a dataframe to plot with 'ggplot2'
#'
#' results <- rbind(m[, c(1, 3:8, 11)], j[,c(1, 3:8, 11)],
#'                  a[, c(1, 3:8, 11)], s[, c(1, 3:8, 11)])
#' results$month <- factor(c(rep("May", 2), rep("June", 2),
#'                           rep("August", 2), rep("September", 2)),
#'                         levels = c("May", "June", "August", "September"))
#'
#'
#' # p2 <- ggplot(data = results, aes(x = month, y = dose,
#' #                              group = factor(p), fill = factor(p))) +
#' #   geom_col(position = position_dodge(width = 0.9), colour = "#000000") +
#' #   geom_errorbar(aes(ymin = LCL, ymax = UCL),
#' #                 size = 0.4, width = 0.06,
#' #                 position = position_dodge(width = 0.9))
#'
#' # p2
#' @export

# Function  LC_logit ----
LC_logit <- function(formula, data, p = NULL, weights = NULL,
                     subset = NULL, log_base = NULL,
                     log_x = TRUE,
                     het_sig = NULL, conf_level = NULL,
                     conf_type = NULL,
                     long_output = TRUE) {

  model <- do.call("glm", list(formula = formula,
                               family = binomial(link = "logit"),
                               data = data,
                               weights = substitute(weights),
                               subset = substitute(subset)))

  # error message for missing weights argument in function call
  if(missing(weights)) {
    warning ("Are you using cbind(response, non-response) method as your y variable, if so do not weight the model. If you are using (response / total) method, model needs the total of test organisms per dose to weight the model properly",
             call. = FALSE)
  }



  # make p a null object and create warning message if p isn't supplied
  if (is.null(p)) {
    p <- seq(1, 99, 1)
    warning(call. = FALSE, "`p`argument has to be supplied otherwise LC values for 1-99 will be displayed")
  }
  # Calculate heterogeneity correction to confidence intervals
  # according to Finney, 1971, (p.72, eq. 4.27; also called "h")
  # Heterogeneity correction factor is used if
  # pearson's goodness of fit test (pgof) returns a sigficance
  # value less than 0.150 (source: 'SPSS 24')

  chi_square <- sum(residuals.glm(model, type = "pearson") ^ 2)

  df <- df.residual(model)

  pgof <- pchisq(chi_square, df, lower.tail = FALSE)

  if (is.null(het_sig)) {
    het_sig <- 0.150
  }

  if (pgof < het_sig) {
    het <- chi_square / df
  } else {
    het <- 1
  }

  # Extract slope and intercept SE, slope and intercept signifcance
  # z-value, & N

  summary <- summary(model)

  # Intercept (b0)
  b0 <- summary$coefficients[1]

  # Slope (b1)
  b1 <- summary$coefficients[2]

  # intercept info

  intercept_se <- summary$coefficients[3]
  intercept_sig <- summary$coefficients[7]

  # slope info

  slope_se <- summary$coefficients[4]
  slope_sig <- summary$coefficients[8]

  # z value

  z_value <- summary$coefficients[6]

  # sample size

  n <- df + 2

# Calculate m for all LC levels based on logits
  # in est (Robertson et al., 2007, pg. 27; or "m" in Finney, 1971, p. 78)

  est <- log((p / 100) / (1 - (p / 100)))
  m <- (est - b0) / b1

  # variances have to be adjusted for heterogenity
  # if pgof returns a signfacnce value less than 0.15
  # (Finney 1971 p 72; 'SPSS 24')

  # covariance matrix
  if (pgof < het_sig) {
    vcova <- vcov(model) * het
  } else {
    vcova <- vcov(model)
  }

  # Slope variance

  var_b1 <- vcova[2, 2]

  # Intercept variance

  var_b0 <- vcova[1, 1]

  # Slope & intercept covariance

  cov_b0_b1 <- vcova[1, 2]

  # Adjust distibution depending on heterogeneity (Finney, 1971,  p72,
  # t distubtion used instead of normal distubtion  with appropriate df
  # if pgof returns a signfacnce value less than 0.15
  # (Finney 1971 p 72; 'SPSS 24')

  if (is.null(conf_level)) {
    conf_level <- 0.95
  }

  t <- (1 - conf_level)

  t_2 <- (t / 2)

  if (pgof < het_sig) {
    tdis <- -qt(t_2, df = df)
  } else {
    tdis <- -qnorm(t_2)
  }

  # Calculate g (Finney, 1971, p 78, eq. 4.36) "With almost
  # all good sets of data, g will be substantially smaller
  # than 1.0 and ## seldom greater than 0.4."

  g <- ((tdis ^ 2 * var_b1) / b1 ^ 2)


  # Calculate correction of fiducial limits according to Fieller method
  # (Finney, 1971,# p. 78-79. eq. 4.35)
  # v11 = var_b0 , v22 = var_b1, v12 = cov_b0_b1

  cl_part_1 <- (g / (1 - g)) * (m + (cov_b0_b1 / var_b1))
  cl_part_2 <- (var_b0 + (2  * cov_b0_b1 * m) + (m ^ 2 * var_b1) -
                  (g * (var_b0 - cov_b0_b1 ^ 2 / var_b1)))
  cl_part_3 <- (tdis / ((1 - g) * abs(b1))) * sqrt(cl_part_2)


  # Calculate the fiducial limit LFL=lower fiducial limit,
  # UFL = upper fiducial limit (Finney, 1971, p. 78-79. eq. 4.35)

  LCL <- (m + (cl_part_1 - cl_part_3))
  UCL <- (m + (cl_part_1 + cl_part_3))



  # calculate standard error
  cf <- -cbind(1, m) / b1

  se_1 <- ((cf %*% vcov(model)) * cf) %*% c(1, 1)

  se_2 <- as.numeric(sqrt(se_1))
  # Calculate variance for m (Robertson et al., 2007, pg. 27)

  var_m <- (1 / (m ^ 2)) * (var_b0 + 2 * m * cov_b0_b1 + var_b1 * m ^ 2)



  if (log_x == TRUE) {

    if(is.null(log_base)) {
      log_base <- 10
    }


    dose <- log_base ^ m
    LCL <- log_base ^ LCL
    UCL <- log_base ^ UCL
    se_2 <- log_base ^ se_2
  }

  if (log_x == FALSE) {
    dose <- m
    LCL <- LCL
    UCL <- UCL
  }


  # Make a data frame from the data at all the different values
  if (long_output == TRUE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL,
                    se = se_2,
                    chi_square = chi_square,
                    df = df,
                    pgof_sig = pgof,
                    h = het,
                    slope = b1,
                    slope_se = slope_se,
                    slope_sig = slope_sig,
                    intercept = b0,
                    intercept_se = intercept_se,
                    intercept_sig = intercept_sig,
                    z = z_value,
                    var_m = var_m,
                    covariance = cov_b0_b1)
  }
  if (long_output == FALSE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL)
  }

  return(table)

}



