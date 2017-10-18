#' Lethal Time
#' @description Calculates lethal time (LT) and
#' its fiducial confidence limits (CL) using a probit analysis
#' according to Finney 1971, Wheeler et al. 2006, and Robertson et al. 2007.
#' @usage LT(formula, data, p = seq(1, 99, 1), weights, conf.level = NULL)
#' @param formula an object of class formula or one that can be coerced to that class: a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which LT is called.
#' @param p Lethal time (LT) values for given p, example will return a LT50 value if p equals 50. If more than one LT value desired specify by creating a vector.
#' @param weights vector of 'prior weights' to be used in the fitting process. Should be a numeric vector, if set to NULL weights will not be used.
#' @param conf.level  Adjust confidence level as necessary or NULL set at 0.95.
#' @return Returns a data frame with predicted LT for given p level, lower CL (LCL), upper CL (UCL), LCL and UCL distance away from LT (LCLdis & UCLdis; important for creating a plot), Pearson's goodness-of-fit test, slope, intercept, slope and intercept p values and standard error, and LT variance.
#' @references
#'
#' Finney, D.J., 1971. Probit Analysis, Cambridge University Press, Cambridge, England
#'
#' Wheeler, M.W., Park, R.M., and Bailey, A.J., 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444
#'
#' Robertson, J.L., Savin, N.E., Russell, R.M. and Preisler, H.K., 2007. Bioassays with arthropods. CRC press.
#' @examples head(lampreytime)
#'
#' results <- LT((dead/total) ~ log10(hour), p = c(50, 99),
#' weights = lampreytime[c(1:11), ]$total,
#' data = lampreytime[c(1:11), ])
#'
#' #view calculated LT50 and LT99 for seasonal
#' #toxicity of a piscicide, 3-trifluoromethyl-4-nitrophenol, to lamprey in 2011
#'
#' results
#'
#' #dose-response curve can be plotted using ggplot2
#' @export
#' @import stats






LT <- function(formula, data, p = seq(1, 99, 1),
               weights = NULL, conf.level = NULL) {

  data$weights <- weights
  if(is.null(weights)) {

    model <- glm(formula, family = binomial(link = "probit"), data = data)
  }

  else {model <- glm(formula, family = binomial(link = "probit"),
                     weights = weights, data = data)
  }
  # Calculate heterogeneity correction to confidence intervals
  # according to Finney, 1971, (p.72, eq. 4.27; also called "h")
  # Heterogeneity correction factor is used if
  # pearson's goodness of fit test returns a sigficance
  # value less than 0.150 (source: SPSS 24)

  PGOF <- (1 - pchisq(sum(residuals(model, type = "pearson") ^ 2),
                      df.residual(model)))

  if (PGOF < 0.150) {
    het <- sum(residuals(model, type = "pearson") ^ 2) / (df.residual(model))
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

  # covariance matrix
  vcova <- vcov(model)

  #determine other important statistics

  intercept_se <- summary$coefficients[3]
  intercept_sig <- summary$coefficients[7]
  slope_se <- summary$coefficients[4]
  slope_sig <- summary$coefficients[8]
  z.value <- summary$coefficients[6]
  n <- nrow(data)

  # variances have to be adjusted for heterogenity
  # if PGOF returns a signfacnce value less than 0.15
  # (Finney 1971 p 72; SPSS 24)
  # Intercept variance

  if (het > 1) {
    var_b0 <- het * vcova[2, 2]
  }

  else {
    var_b0 <- vcova[2, 2]
  }

  # Slope variance

  if (het > 1) {
    var_b1 <- het * vcova[1, 1]
  }

  else {
    var_b1 <- vcova[1, 1]
  }

  # Slope & intercept covariance

  if (het > 1) {
    cov_b0_b1 <- het * vcova[1, 2]
  }

  else {
    cov_b0_b1 <- vcova[1, 2]
  }

  # Adjust distibution depending on heterogeneity (Finney, 1971,  p72,
  # t distubtion used instead of normal distubtion  with appropriate df
  # if PGOF returns a signfacnce value less than 0.15
  # (Finney 1971 p 72; SPSS 24)

  if (is.null(conf.level)) {
    conf.level = 0.95
  }

  t <- (1 - conf.level)
  if (het > 1) {
    tdis <- (- qt((t / 2), df = df.residual(model)))
  }

  else {
    tdis <- (- qnorm(t / 2))
  }

  # Calculate g (Finney, 1971, p 78, eq. 4.36) "With almost
  # all good sets of data, g will be substantially smaller
  # than 1.0 and ## seldom greater than 0.4."

  g <- ((tdis ^ 2 * var_b0) / b1 ^ 2)

  # Calculate m for all LC levels based on probits
  # in est (Robertson et al., 2007, pg. 27; or "m" in Finney, 1971, p. 78)

  est <- (qnorm(p / 100))
  m <- (est - b0) / b1

  # Calculate correction of fiducial limits according to Fieller method
  # (Finney, 1971,# p. 78-79. eq. 4.35)
  # v11 = var_b1 , v22 = var_b0, v12 = cov_b0_b1

  fl1 <- (g / (1 - g)) * (m + (cov_b0_b1 / var_b0))
  fl2 <- (tdis / ((1 - g) * b1)) * sqrt((var_b1 + (2 * m * cov_b0_b1) +
          (m ^ 2 * var_b0) - (g * (var_b1 - cov_b0_b1 ^ 2 / var_b0))))

  # Calculate the fiducial limit LFL=lower fiducial limit,
  # UFL = upper fiducial limit (Finney, 1971, p. 78-79. eq. 4.35)

  LFL <- (m + (fl1 - fl2))
  UFL <- (m + (fl1 + fl2))

  # Calculate variance for m (Robertson et al., 2007, pg. 27)

  var_m <- (1 / (m ^ 2)) * (var_b1 + 2 * m * cov_b0_b1 +
                              var_b0 * m ^ 2)

  # Make a data frame from the data at all the different values
  LTtable <- data.frame(
    p = p,
    n = n,
    time = 10 ^ m,
    LCL = 10 ^ LFL,
    UCL = 10 ^ UFL,
    LCLdis = 10 ^ m - 10 ^ LFL,
    UCLdis = 10 ^ UFL - 10 ^ m,
    chisquare = sum(residuals(model, type = "pearson") ^ 2),
    df = df.residual(model),
    PGOF_sig = PGOF,
    h = het,
    slope = b1,
    slope_se = slope_se,
    slope_sig = slope_sig,
    intercept = b0,
    intercept_se = intercept_se,
    intercept_sig = intercept_sig,
    z = z.value,
    var_m = var_m)

  return(LTtable)

}
