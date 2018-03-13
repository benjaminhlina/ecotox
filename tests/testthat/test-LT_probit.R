context("Test LT_probit")

test_that("Determine if LT_probit runs properly", {
  m <- LT_probit((response / total) ~ log10(hour),
            p = c(50),
            weights = total,
            data = lampreytime,
            subset = c(month == "May"))

  expect_equal(m$time, expected = 9.991, tolerance = 0.001)
  expect_equal(m$LCL, expected = 8.934, tolerance = 0.001)
  expect_equal(m$UCL, expected = 11.742, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 13.499, tolerance = 0.001)
  expect_equal(m$PGOF_sig, expected = 0.1412, tolerance = 0.0001)
})
