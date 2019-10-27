context("Test ratio_test")


# test if ratio_test works properly -----
test_that("Determine if ratio_test runs properly", {

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


