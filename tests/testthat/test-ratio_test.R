context("Test ratio_test")

test_that("Determine if ratio_test runs properly", {

s <- glm((response / total) ~ log10(dose),
         data = lamprey_tox,
         subset = c(month == "September"),
         weights = total,
         family = binomial(link = "probit"))


j <- glm((response / total) ~ log10(dose),
         data = lamprey_tox,
         subset = c(month == "June"),
         weights = total,
         family = binomial(link = "probit"))


ratios <- ratio_test(model_1 = j, model_2 = s, percentage = 50,
                     type = "probit")
ratios

expect_equal(ratios$dose_1, expected = 2.66, tolerance = 0.001)
  expect_equal(ratios$dose_2, expected = 2.12, tolerance = 0.001)
  expect_equal(ratios$se, expected = 0.0387, tolerance = 0.001)
  expect_equal(ratios$test_stat, expected = 2.544, tolerance = 0.001)
  expect_equal(ratios$p, expected =  0.0109, tolerance = 0.0001)

})

