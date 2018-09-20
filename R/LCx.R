# Descirption of LC_probit ----

#' Lethal Concentration Probit
#' @description Calculates lethal concentration (LC) and
#' its fiducial confidence limits (CL) using a probit analysis
#' according to Finney 1971, Wheeler et al. 2006, and Robertson et al. 2007.
#' @usage LC_probit(formula, data, p = seq(1, 99, 1), weights,
#'           subset = NULL, log_x = TRUE, het_sig = NULL, conf_level = NULL,
#'           long_output = TRUE)
#' @param formula an object of class `formula` or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under Details.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which `LC_probit` is called.
#' @param p Lethal Concentration (LC) value for given p, example will return a LC50 value if p equals 50. If more than one LC value wanted specify by creating a vector.
#' @param weights vector of 'prior weights' to be used in the fitting process. Should be a numeric vector and is required for analysis.
#' @param subset allows for the data to be subseted if desired. Default set to `NULL`.
#' @param log_x default is `TRUE` and will calculate results using the antilog10 given that the x variable has been `log10` tranformed. If `FALSE` results will not be back transformed.
#' @param het_sig significance level from person's chi square goodness-of-fit test (pgof) that is used to decide if a heterogeneity factor is used. `NULL` is set to 0.15.
#' @param conf_level adjust confidence level as necessary or `NULL` set at 0.95.
#' @param long_output default is `TRUE` which will return a tibble with all 19 variabless. If `FALSE` the tibble returned will consist of the p level, n, the predicted LC for given p level, lower and upper confidence limits and their distances.
#' @return Returns a tibble with predicted LC for given p level, lower CL (LCL), upper CL (UCL), LCL and UCL distance away from LC (LCL_dis & UCL_dis; important for creating a plot), Pearson's chi square goodness-of-fit test (pgof), slope, intercept, slope and intercept p values and standard error, and LC variance.
#' @references
#'
#' Finney, D.J., 1971. Probit Analysis, Cambridge University Press, Cambridge, England, ISBN: 052108041X
#'
#' Wheeler, M.W., Park, R.M., and Bailey, A.J., 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444.10.1897/05-320R.1
#'
#' Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press. ISBN: 9780849323317

#' @examples head(lampreytox)
#'
#' # within the dataframe used, control dose, unless produced a value
#' # during experimentation, are removed from the dataframe,
#' # as glm cannot handle values of infinite. Other statistical programs
#' # make note of the control dose but do not include within analysis
#'
#' #calculate LC50 and LC99
#'
#' m <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'          weights = total,
#'          data = lampreytox,
#'          subset = c(month == "May"))
#'
#' #view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
#' #to lamprey in 2011
#'
#' m
#'
#' #dose-response curve can be plotted using 'ggplot2'
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(data = subset(lampreytox, month %in% c("May")),
#'              aes(x = log10(dose), y = (response / total))) +
#'   geom_point() +
#'   geom_smooth(method = "glm",
#'             method.args = list(family = binomial(link = "probit")),
#'             aes(weight = total), colour = "#FF0000", se = TRUE)
#'
#' p1
#'
#' #calculate LC50s and LC99s for multiple toxicity tests, June, August, and September
#'
#' j <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lampreytox,
#'         subset = c(month == "June"))
#'
#' a <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lampreytox,
#'         subset = c(month == "August"))
#'
#' s <- LC_probit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lampreytox,
#'         subset = c(month == "September"))
#'
#' #group results together in a dataframe to plot with 'ggplot2'
#'
#' results <- rbind(m[, c(1, 3:8, 11)], j[,c(1, 3:8, 11)],
#'                  a[, c(1, 3:8, 11)], s[, c(1, 3:8, 11)])
#'
#' results$month <- with(results, factor(c("May", "May", "June", "June",
#'                                         "August", "August", "September",
#'                                         "September"),
#'                                         levels = c("May", "June",
#'                                         "August", "September")))
#'
#' p2 <- ggplot(data = results, aes(x = month, y = dose,
#'                              group = factor(p), fill = factor(p))) +
#'   geom_col(position = position_dodge(width = 0.9), colour = "#000000") +
#'   geom_errorbar(aes(ymin = (dose - LCL_dis), ymax = (dose + UCL_dis)),
#'                 size = 0.4, width = 0.06,
#'                 position = position_dodge(width = 0.9))
#'
#' p2
#' @import ggplot2
#' @import magrittr
#' @import stats
#' @import tibble
#' @export

# Function  LC_probit ----
LC_probit <- function(formula, data, p = seq(1, 99, 1), weights,
                      subset = NULL, log_x = TRUE, het_sig = NULL,
                      conf_level = NULL, long_output = TRUE) {

  model <- do.call("glm", list(formula = formula,
                               family = binomial(link = "probit"),
                               data = data,
                               weights = substitute(weights),
                               subset = substitute(subset)))

  # Calculate heterogeneity to correct confidence intervals
  # according to Finney, 1971, (p.72, eq. 4.27; also called "h")
  # Heterogeneity correction factor is used if
  # pearson's goodness of fit test (pgof) returns a sigficance
  # value less than 0.150 (source: 'SPSS 24')

  chi_square <- residuals.glm(model, type = "pearson") ^ 2 %>%
                  sum()

  df <- df.residual(model)

  pgof <- pchisq(chi_square, df, lower.tail = FALSE)

  if (is.null(het_sig)) {
    het_sig <- 0.150
  }

  if (pgof < het_sig) {
    het <- chi_square / df
  }

  else {
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

  # variances have to be adjusted for heterogenity
  # if pgof returns a signfacnce value less than 0.150
  # (Finney 1971 p 72; 'SPSS 24')

  # covariance matrix

  if (pgof < het_sig) {
    vcova <- vcov(model) * het
  }

  else {
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
  }

  else {
    tdis <- -qnorm(t_2)
  }

  # Calculate g (Finney, 1971, p 78, eq. 4.36) "With almost
  # all good sets of data, g will be substantially smaller
  # than 1.0 and ## seldom greater than 0.4."

  g <- (tdis ^ 2 * var_b1) / (b1 ^ 2)

  # Calculate m for all LC levels based on probits
  # in est (Robertson et al., 2007, pg. 27; or "m" in Finney, 1971, p. 78)

  est <- qnorm(p / 100)
  m <- (est - b0) / b1

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

  # Calculate variance for m (Robertson et al., 2007, pg. 27)

  var_m <- (1 / (m ^ 2)) * (var_b0 + 2 * m * cov_b0_b1 +
                              m ^ 2 * var_b1)

  if (log_x == TRUE) {
    dose <- 10 ^ m
    LCL <- 10 ^ LCL
    UCL <- 10 ^ UCL
    LCL_dis <- dose - LCL
    UCL_dis <- UCL - dose
  }

  if (log_x == FALSE) {
    dose <- m
    LCL <- LCL
    UCL <- UCL
    LCL_dis <-  dose - LCL
    UCL_dis <- UCL - dose
  }

  # Make a data frame from the data at all the different values
  if (long_output == TRUE) {
   table <- tibble(p = p,
                   n = n,
                   dose = dose,
                   LCL = LCL,
                   UCL = UCL,
                   LCL_dis = LCL_dis,
                   UCL_dis = UCL_dis,
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
                   var_m = var_m)
  }
  if (long_output == FALSE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL,
                    LCL_dis = LCL_dis,
                    UCL_dis = UCL_dis)
  }
  return(table)

}

# Descirption of LC_logit ----
#' Lethal Concentration Logit
#' @description Calculates lethal concentration (LC) and
#' its fiducial confidence limits (CL) using a logit analysis
#' according to Finney 1971, Wheeler et al. 2006, and Robertson et al. 2007.
#' @usage LC_logit(formula, data, p = seq(1, 99, 1), weights,
#'          subset = NULL, log_x = TRUE, het_sig = NULL,
#'          conf_level = NULL, long_output = TRUE)
#' @param formula an object of class `formula` or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under Details.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which `LC_logit` is called.
#' @param p Lethal Concentration (LC) values for given p, example will return a LC50 value if p equals 50. If more than one LC value wanted specify by creating a vector.
#' @param weights vector of 'prior weights' to be used in the fitting process. Should be a numeric vector and is required for analysis.
#' @param log_x default is `TRUE` and will calculate results using the antilog10 given that the x variable has been `log10` tranformed. If `FALSE` results will not be back transformed.
#' @param subset allows for the data to be subseted if desired. Default set to `NULL`.
#' @param het_sig significance level from person's chi square goodness-of-fit test that is used to decide if a heterogeneity factor is used. `NULL` is set to 0.15.
#' @param conf_level adjust confidence level as necessary or `NULL` set at 0.95.
#' @param long_output default is `TRUE` which will return a tibble with all 19 variabless. If `FALSE` the tibble returned will consist of the p level, n, the predicted LC for given p level, lower and upper confidence limits and their distances.
#' @return Returns a tibble with predicted LC for given p level, lower CL (LCL), upper CL (UCL), LCL and UCL distance away from LC (LCL_dis & UCL_dis; important for creating a plot), Pearson's chi square goodness-of-fit test (pgof), slope, intercept, slope and intercept p values and standard error, and LC variance.
#' @references
#'
#' Finney, D.J., 1971. Probit Analysis, Cambridge University Press, Cambridge, England, ISBN: 052108041X
#'
#' Wheeler, M.W., Park, R.M., and Bailey, A.J., 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444.10.1897/05-320R.1
#'
#' Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press. ISBN: 9780849323317

#' @examples head(lampreytox)
#'
#' # within the dataframe used, control dose, unless produced a value
#' # during experimentation, are removed from the dataframe,
#' # as glm cannot handle values of infinite. Other statistical programs
#' # make note of the control dose but do not include within analysis
#'
#'
#' #calculate LC50 and LC99 for May
#'
#' m <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'          weights = total,
#'          data = lampreytox,
#'          subset = c(month == "May"))
#'
#' #view calculated LC50 and LC99 for seasonal toxicity of a pisicide,
#' #to lamprey in 2011
#'
#' m
#'
#' #dose-response curve can be plotted using 'ggplot2'
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(data = lampreytox[c(1:19), ],
#'              aes(x = log10(dose), y = (response / total))) +
#'   geom_point() +
#'   geom_smooth(method = "glm",
#'             method.args = list(family = binomial(link = "logit")),
#'             aes(weight = total), colour = "#FF0000", se = TRUE)
#'
#' p1
#'
#' #calculate LC50s and LC99s for multiple toxicity tests, June, August, and September
#'
#' j <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lampreytox,
#'         subset = c(month == "June"))
#'
#' a <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lampreytox,
#'         subset = c(month == "August"))
#'
#' s <- LC_logit((response / total) ~ log10(dose), p = c(50, 99),
#'         weights = total,
#'         data = lampreytox,
#'         subset = c(month == "September"))
#'
#' #group results together in a dataframe to plot with 'ggplot2'
#'
#' results <- rbind(m[, c(1, 3:8, 11)], j[,c(1, 3:8, 11)],
#'                  a[, c(1, 3:8, 11)], s[, c(1, 3:8, 11)])
#'
#' results$month <- with(results, factor(c("May", "May", "June", "June",
#'                                         "August", "August", "September",
#'                                         "September"),
#'                                         levels = c("May", "June",
#'                                         "August", "September")))
#'
#' p2 <- ggplot(data = results, aes(x = month, y = dose,
#'                              group = factor(p), fill = factor(p))) +
#'   geom_col(position = position_dodge(width = 0.9), colour = "#000000") +
#'   geom_errorbar(aes(ymin = (dose - LCL_dis), ymax = (dose + UCL_dis)),
#'                 size = 0.4, width = 0.06,
#'                 position = position_dodge(width = 0.9))
#'
#' p2
#' @export

# Function  LC_logit ----
LC_logit <- function(formula, data, p = seq(1, 99, 1), weights,
                     subset = NULL, log_x = TRUE,
                     het_sig = NULL, conf_level = NULL,
                     long_output = TRUE) {

  model <- do.call("glm", list(formula = formula,
                               family = binomial(link = "logit"),
                               data = data,
                               weights = substitute(weights),
                               subset = substitute(subset)))

  # Calculate heterogeneity correction to confidence intervals
  # according to Finney, 1971, (p.72, eq. 4.27; also called "h")
  # Heterogeneity correction factor is used if
  # pearson's goodness of fit test (pgof) returns a sigficance
  # value less than 0.150 (source: 'SPSS 24')

  chi_square <- residuals.glm(model, type = "pearson") ^ 2 %>%
                  sum()

  df <- df.residual(model)

  pgof <- pchisq(chi_square, df, lower.tail = FALSE)

  if (is.null(het_sig)) {
    het_sig <- 0.150
  }

  if (pgof < het_sig) {
    het <- chi_square / df
  }
  else {
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

  # variances have to be adjusted for heterogenity
  # if pgof returns a signfacnce value less than 0.15
  # (Finney 1971 p 72; 'SPSS 24')

  # covariance matrix
  if (pgof < het_sig) {
    vcova <- vcov(model) * het
  }

  else {
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
  }

  else {
    tdis <- -qnorm(t_2)
  }

  # Calculate g (Finney, 1971, p 78, eq. 4.36) "With almost
  # all good sets of data, g will be substantially smaller
  # than 1.0 and ## seldom greater than 0.4."

  g <- ((tdis ^ 2 * var_b1) / b1 ^ 2)

  # Calculate m for all LC levels based on logits
  # in est (Robertson et al., 2007, pg. 27; or "m" in Finney, 1971, p. 78)

  est <- log((p / 100) / (1 - (p / 100)))
  m <- (est - b0) / b1

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

  # Calculate variance for m (Robertson et al., 2007, pg. 27)

  var_m <- (1 / (m ^ 2)) * (var_b0 + 2 * m * cov_b0_b1 + var_b1 * m ^ 2)

  if (log_x == TRUE) {
    dose <- 10 ^ m
    LCL <- 10 ^ LCL
    UCL <- 10 ^ UCL
    LCL_dis <- dose - LCL
    UCL_dis <- UCL - dose
  }

  if (log_x == FALSE) {
    dose <- m
    LCL <- LCL
    UCL <- UCL
    LCL_dis <-  dose - LCL
    UCL_dis <- UCL - dose
  }


  # Make a data frame from the data at all the different values
  if (long_output == TRUE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL,
                    LCL_dis = LCL_dis,
                    UCL_dis = UCL_dis,
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
                    var_m = var_m)
  }
  if (long_output == FALSE) {
    table <- tibble(p = p,
                    n = n,
                    dose = dose,
                    LCL = LCL,
                    UCL = UCL,
                    LCL_dis = LCL_dis,
                    UCL_dis = UCL_dis)
  }

  return(table)

}
