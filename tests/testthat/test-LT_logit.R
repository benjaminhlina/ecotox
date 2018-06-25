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
  expect_equal(m$LCL_dis, expected = 1.115, tolerance = 0.001)
  expect_equal(m$UCL_dis, expected = 1.934, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 16.616, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.055, tolerance = 0.001)

  j <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lampreytime,
                subset = c(month == "June"))

  expect_equal(j$time, expected = 13.738, tolerance = 0.001)
  expect_equal(j$LCL, expected = 11.523, tolerance = 0.001)
  expect_equal(j$UCL, expected = 19.595, tolerance = 0.001)
  expect_equal(j$LCL_dis, expected = 2.215, tolerance = 0.001)
  expect_equal(j$UCL_dis, expected = 5.857, tolerance = 0.001)
  expect_equal(j$chi_square, expected = 6.327, tolerance = 0.001)
  expect_equal(j$pgof_sig, expected = 0.706, tolerance = 0.001)

  a <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lampreytime,
                subset = c(month == "August"))

  expect_equal(a$time, expected = 8.169, tolerance = 0.001)
  expect_equal(a$LCL, expected = 7.338, tolerance = 0.001)
  expect_equal(a$UCL, expected = 9.280, tolerance = 0.001)
  expect_equal(a$LCL_dis, expected = 0.830, tolerance = 0.001)
  expect_equal(a$UCL_dis, expected = 1.110, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 7.616, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected = 0.573, tolerance = 0.001)

  s <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lampreytime,
                subset = c(month == "September"))

  expect_equal(s$time, expected = 12.445, tolerance = 0.001)
  expect_equal(s$LCL, expected = 10.959, tolerance = 0.001)
  expect_equal(s$UCL, expected = 16.443, tolerance = 0.001)
  expect_equal(s$LCL_dis, expected = 1.486, tolerance = 0.001)
  expect_equal(s$UCL_dis, expected = 3.997, tolerance = 0.001)
  expect_equal(s$chi_square, expected = 3.761, tolerance = 0.001)
  expect_equal(s$pgof_sig, expected = 0.926, tolerance = 0.001)





})
