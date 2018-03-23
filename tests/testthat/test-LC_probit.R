context("Test LC_probit")

test_that("Determine if LC_probit calculations are correct", {
  m <- LC_probit((response / total) ~ log10(dose), p = c(50),
               weights = total,
               data = lampreytox,
               subset = c(month == "May"))
  expect_equal(m$dose, expected = 1.250, tolerance = 0.001)
  expect_equal(m$LCL, expected = 1.184, tolerance = 0.001)
  expect_equal(m$UCL, expected = 1.306, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 14.113, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.590, tolerance = 0.001)




  a <- LC_probit((response / total) ~ log10(dose), p = c(50),
               weights = total,
               data = lampreytox,
               subset = c(month == "August"))

  expect_equal(a$dose, expected = 4.009, tolerance = 0.001)
  expect_equal(a$LCL, expected = 3.651, tolerance = 0.001)
  expect_equal(a$UCL, expected = 4.348, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 21.274, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected = 0.0192, tolerance = 0.0001)



})
