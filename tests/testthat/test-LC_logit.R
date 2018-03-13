context("Test LC_logit")

test_that("Determine if LC_logit calculations are correct", {
  m <- LC_logit((response / total) ~ log10(dose), p = c(50),
                 weights = total,
                 data = lampreytox,
                 subset = c(month == "May"))

  expect_equal(m$dose, expected = 1.256, tolerance = 0.001)
  expect_equal(m$LCL, expected = 1.188, tolerance = 0.001)
  expect_equal(m$UCL, expected = 1.312, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 15.213, tolerance = 0.001)
  expect_equal(m$PGOF_sig, expected = 0.509, tolerance = 0.001)

  a <- LC_logit((response / total) ~ log10(dose), p = c(50),
                 weights = total,
                 data = lampreytox,
                 subset = c(month == "August"))

  expect_equal(a$dose, expected = 4.013, tolerance = 0.001)
  expect_equal(a$LCL, expected = 3.621, tolerance = 0.001)
  expect_equal(a$UCL, expected = 4.385, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 21.908, tolerance = 0.001)
  expect_equal(a$PGOF_sig, expected = 0.0155, tolerance = 0.0001)


})
