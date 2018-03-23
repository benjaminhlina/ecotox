context("Test LT_logit")

test_that("Determine if LT_logit calculations are correct", {
  m <- LT_logit((response / total) ~ log10(hour),
           p = c(50),
           weights = total,
           data = lampreytime,
           subset = c(month == "May"))

  expect_equal(m$time, expected = 9.967, tolerance = 0.001)
  expect_equal(m$LCL, expected = 8.851, tolerance = 0.001)
  expect_equal(m$UCL, expected = 11.901, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 16.616, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.055, tolerance = 0.001)
})
