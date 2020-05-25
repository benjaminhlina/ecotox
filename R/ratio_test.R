# Documentation of function -----

#' Ratio test
#'@description Calculates a ratio test to compare two LC or LT values from two seperate probit or logit models. This function is based on the ratio test developed in [Wheeler et al. 2006. 10.1897/05-320R.1](http://onlinelibrary.wiley.com/doi/10.1897/05-320R.1/abstract) which has been suggested as a replacement to the common method of comparing confidence intervals to determine differences.
#' @usage ratio_test(model_1, model_2, percentage = NULL,
#'            type = NULL, compare = NULL, log_base = NULL, log_x = TRUE)
#' @param model_1 first model used in the ratio test. Should be an object of either a probit or logit model created using the `glm()` function. See example.
#' @param model_2 second model used in the ratio test. Should be an object of either a probit or logit model created using the `glm()` function. See example.
#' @param percentage either a single value or a vector for given LC or LT percentage desired to compare. Percentage is the same value used for the argument `p` in all `LC_` and `LT_` functions. For example, 50 will return and compare LC50 values for the two models. If more than one LC value is disired specify by creating a vector. LC values can be calculated down to the 1e-16 of a percentage (e.g. LC99.99). However, the tibble produced can and will round to nearest whole number.
#' @param type Link type needs to be specfied to either `"probit"` which is default and will return and used in calculations for a probit model for the desired LCs or LTs. If specified to `"logit"` then `ratio_test` will return and calcuate using a logit model for the desired LCs or LTs.
#' @param compare Supply a character string to be used in the output letting the user know what models the LCs or LTs are being compared. Default output is "Model 1 - Model 2". See example.
#' @param log_base default is `10` and will be used to  calculate results using the anti of `log10()` given that the x variable has been `log10` tranformed. If `FALSE` results will not be back transformed.
#' @param log_x default is `TRUE` and will calculate results using the antilog of determined by `log_base` given that the x variable has been `log()` tranformed. If `FALSE` results will not be back transformed.
#' @return A tibble with `percentage` for the LC or LT value desired for the above percentage argument, `dose_1` and `dose_2` displayed calculated backtransformed or untransformed doses for the desired LC or LT values. Standard Error (`se`), Z test statistic (`test_stat`) and `p_value` detemined using Z test statistic as determined using formulas in [Wheeler et al. 2006](http://onlinelibrary.wiley.com/doi/10.1897/05-320R.1/abstract).
#'.
#'
#' @references
#'
#' Wheeler, M.W., Park, R.M., and Bailey, A.J., 2006. Comparing median lethal concentration values using confidence interval overlap or ratio tests, Environ. Toxic. Chem. 25(5), 1441-1444.[10.1897/05-320R.1](http://onlinelibrary.wiley.com/doi/10.1897/05-320R.1/abstract)
#'
#' @examples
#' # view lamprey_tox data
#'
#' head(lamprey_tox)
#'
#' # using glm() to detemine LC values using probit model for May and June
#'
#' m <- glm((response / total) ~ log10(dose),
#'          data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'          subset = c(month == "May"),
#'          weights = total,
#'          family = binomial(link = "probit"))
#'
#'
#' j <- glm((response / total) ~ log10(dose),
#'          data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
#'          subset = c(month == "June"),
#'          weights = total,
#'          family = binomial(link = "probit"))
#'
#' # now that both May and June models have been made. use ratio_test to
#' # compare LC50 values or whatever LC values of interest.
#'
#' ratios <- ratio_test(model_1 = m, model_2 = j, percentage = 50,
#' compare = "May - June")
#'
#' # view ratio test results
#'
#' ratios
#'
#' @export


# function ------
ratio_test <- function (model_1, model_2, percentage = NULL,
                        type = NULL, compare = NULL,
                        log_base = NULL, log_x = TRUE) {

  if(missing(model_1)) {
    stop("Ratio test needs first `glm()` object to compare LC or LT values",
         call. = FALSE)
  }

  if(missing(model_2)) {
    stop("Ratio test needs second `glm()` object to compare LC or LT values",
         call. = FALSE)
  }

  # if(missing(type)) {
  #   stop('Ratio test needs a link type of either "probit", or "logit" to calculate properly',
  #        call. = FALSE)
  # }



  # create p and warning for p ----
  if (is.null(percentage)) {
    percentage  <- seq(1, 99, 1)
    warning("`percentage`argument has to be supplied otherwise ratio test will use values for 1-99 and will display values for 1-99", call. = FALSE)
  }


  if (is.null(obj_type)) {
    obj_type <- c("list")
  } else {
    obj_type <- c("df")

  }


  if (obj_type == "list") {

    # create summary for models that you are to compare ----
    s_m1 <- summary(model_1)
    s_m2 <- summary(model_2)

    # extract coeffiecnets from model 1-----
    b0_a <- s_m1$coefficients[1]

    # Slope (b1)
    b1_a <- s_m1$coefficients[2]

    # intercept se info

    intercept_se_a <- s_m1$coefficients[3]

    # slope se info

    slope_se_a <- s_m1$coefficients[4]

    # extract coeficencts from the second model -----

    b0_b <- s_m2$coefficients[1]

    # Slope (b1)

    b1_b <- s_m2$coefficients[2]

    # intercept se  info

    intercept_se_b <- s_m2$coefficients[3]


    # slope se info

    slope_se_b <- s_m2$coefficients[4]

    # create varaince and covraince matix for both models----

    # variance co variance matrix for model 1
    vcov_a <- vcov(model_1)

    # extract slope and incercpt co varaince for model 1
    cov_b0_b1_a <- vcov_a[1, 2]


    # variance co variance matrix for model
    vcov_b <- vcov(model_2)

    # extract slope and incercpt co varaince for model 1
    cov_b0_b1_b <- vcov_b[1, 2]

  }

  if (obj_type == "df") {

    s_m1 <- model_1
    s_m2 <- model_2

    # extract coeffiecnets from model 1-----
    b0_a <- unique(s_m1$intercept)

    # Slope (b1)
    b1_a <- unique(s_m1$slope)

    # intercept se info

    intercept_se_a <- unique(s_m1$intercept_se)

    # slope se info

    slope_se_a <- unique(s_m1$slope_se)

    h_a <- unique(s_m1$h)

    # extract coeficencts from the second model -----

    b0_b <- unique(s_m2$intercept)

    # Slope (b1)

    b1_b <- unique(s_m2$slope)

    # intercept se  info

    intercept_se_b <- unique(s_m2$intercept_se)


    # slope se info

    slope_se_b <- unique(s_m2$slope_se)

    h_b <- unique(s_m2$h)
    # create varaince and covraince matix for both models----

    # variance co variance matrix for model 1
    if (h_a > 1) {
      # extract slope and incercpt co varaince for model 1
      cov_b0_b1_a <- unique(s_m1$covariance) / h_a

    } else {
      cov_b0_b1_a <- unique(s_m1$covariance)
    }

    if (h_b > 1) {
      # extract slope and incercpt co varaince for model 2
      cov_b0_b1_b <- unique(s_m2$covariance) / h_b
    } else {
      cov_b0_b1_b <- unique(s_m2$covariance)
    }
  }






  # determine lethal dose for both models ----

  # create estimate from p

  if (is.null(type)) {
    type <- c("probit")
  } else {
    type <- c("logit")

  }

  if (type == "probit") {


    # estimate based on probit
    est <- qnorm(percentage / 100)

    # use slope and intercep of model 1 to determine dose

    model_1_d <- (est - b0_a) / b1_a

    model_2_d <- (est - b0_b) / b1_b

  }

  if (type == "logit") {

    est <- log((percentage / 100) / (1 - (percentage / 100)))

    # use slope and intercep of model 1 to determine dose

    model_1_d <- (est - b0_a) / b1_a

    # use slope and intercep of model 2 to determine dose
    model_2_d <- (est - b0_b) / b1_b

  }


  # creteate se for ratio test for model 1 and model 2 -----

  se_1 <- (intercept_se_a ^ 2 / b0_a ^ 2) + (slope_se_a ^ 2 / b1_a ^ 2) +
    (intercept_se_b ^ 2 / b0_b ^ 2) + (slope_se_b ^ 2 / b1_b ^ 2) -
    ((2 * cov_b0_b1_a) / (b1_a * b0_a)) -
    ((2 * cov_b0_b1_b) / (b1_b * b0_b))

  # square root the se ----
  se_2 <- sqrt(se_1)

  # take the model 1 predicted dose - model 2 prediected dose -----
  t <- abs(model_1_d - model_2_d)


  # Z test/ratio test according to wheeler et al. 2006 take t / se_2 ----
  z <- t / se_2

  p_value <- pnorm(-abs(z)) * 2




  if (log_x == TRUE) {

    if(is.null(log_base)) {
      log_base <- 10

      dose_1 <- log_base ^ model_1_d
      dose_2 <- log_base ^ model_2_d
    }
  }


  if (log_x == FALSE) {

    dose_1 <- model_1_d
    dose_2 <- model_2_d
  }


  if (is.null(compare)) {
    compare <- "Model 1 - Model 2"
  }

  table <- tibble(compare = compare,
                  percentage = percentage,
                  dose_1 =  dose_1,
                  dose_2 = dose_2,
                  se = se_2,
                  test_stat = z,
                  p_value = p_value)
  return(table)
}



