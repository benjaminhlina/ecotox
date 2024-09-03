context("Test ratio_test")


# test if ratio_test works properly for probit -----
test_that("Determine if ratio_test runs properly for probits", {

  s <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "September"),
           weights = total,
           family = binomial(link = "probit"))


  j <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "June"),
           weights = total,
           family = binomial(link = "probit"))


  ratios <- ratio_test(model_1 = j, model_2 = s, percentage = 50)
  ratios

  expect_equal(ratios$dose_1, expected = 2.66, tolerance = 0.001)
  expect_equal(ratios$dose_2, expected = 2.12, tolerance = 0.001)
  expect_equal(ratios$se, expected = 0.0387, tolerance = 0.001)
  expect_equal(ratios$test_stat, expected = 2.544, tolerance = 0.001)
  expect_equal(ratios$p_value, expected =  0.0109, tolerance = 0.0001)

})


# test if ratio_test works for logits ------

test_that("Determine if ratio_test runs properly for logits", {

  s <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "September"),
           weights = total,
           family = binomial(link = "logit"))


  j <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "June"),
           weights = total,
           family = binomial(link = "logit"))


  ratios <- ratio_test(model_1 = j, model_2 = s, percentage = 50,
                       type = "logit")
  ratios

  expect_equal(ratios$dose_1, expected = 2.654, tolerance = 0.001)
  expect_equal(ratios$dose_2, expected = 2.12, tolerance = 0.001)
  expect_equal(ratios$se, expected = 0.0374, tolerance = 0.001)
  expect_equal(ratios$test_stat, expected = 2.621, tolerance = 0.001)
  expect_equal(ratios$p_value, expected =  0.0087, tolerance = 0.0001)

})



# test if ratio_test returns proper values when does isn't log transformed ----

test_that("Determine if ratio_test runs properly
          if dose isn't log transformed", {

  s <- glm((response / total) ~ dose,
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "September"),
           weights = total,
           family = binomial(link = "probit"))


  j <- glm((response / total) ~ dose,
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "June"),
           weights = total,
           family = binomial(link = "probit"))


  ratios <- ratio_test(model_1 = j, model_2 = s,
                       percentage = 50, log_x = FALSE)
  ratios

  expect_equal(ratios$dose_1, expected = 2.666, tolerance = 0.001)
  expect_equal(ratios$dose_2, expected = 2.140, tolerance = 0.001)
  expect_equal(ratios$se, expected = 0.0292, tolerance = 0.001)
  expect_equal(ratios$test_stat, expected =  7.508, tolerance = 0.001)
  expect_equal(ratios$p_value, expected =  5.962518e-14, tolerance = 0.0001)

})

# test errors -----

test_that("Test errors if model 2 isn't supplied", {
  s <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "September"),
           weights = total,
           family = binomial(link = "probit"))


  j <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "June"),
           weights = total,
           family = binomial(link = "probit"))

  expect_error(ratio_test(model_1 = j, percentage = 50))



})

test_that("Test errors if model 1 isn't supplied", {
  s <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "September"),
           weights = total,
           family = binomial(link = "probit"))


  j <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "June"),
           weights = total,
           family = binomial(link = "probit"))

  expect_error(ratio_test(model_2 = s, percentage = 50))



})



# test warning messages -----

test_that("Test warning for when percentage isn't supplied", {
  s <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "September"),
           weights = total,
           family = binomial(link = "probit"))


  j <- glm((response / total) ~ log10(dose),
           data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
           subset = c(month == "June"),
           weights = total,
           family = binomial(link = "probit"))

  expect_warning(ratio_test(model_1 = j, model_2 = s))



})
