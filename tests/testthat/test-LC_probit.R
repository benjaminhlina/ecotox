context("Test LC_probit")

test_that("Determine if LC_probit calculations are correct", {
  m <- LC_probit((response / total) ~ log10(dose), p = c(50),
               weights = total,
               data = lampreytox,
               subset = c(month == "May"))
  expect_equal(m$dose, expected = 1.250, tolerance = 0.001)
  expect_equal(m$LCL, expected = 1.184, tolerance = 0.001)
  expect_equal(m$UCL, expected = 1.306, tolerance = 0.001)
  expect_equal(m$LCL_dis, expected = 0.065, tolerance = 0.001)
  expect_equal(m$UCL_dis, expected = 0.056, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 14.113, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.590, tolerance = 0.001)

  j <- LC_probit((response / total) ~ log10(dose),
                 p = c(50),
                 weights = total,
                 data = lampreytox,
                 subset = c(month == "June"))
  expect_equal(j$dose, expected = 2.659, tolerance = 0.001)
  expect_equal(j$LCL, expected = 2.590, tolerance = 0.001)
  expect_equal(j$UCL, expected = 2.737, tolerance = 0.001)
  expect_equal(j$LCL_dis, expected = 0.069, tolerance = 0.001)
  expect_equal(j$UCL_dis, expected = 0.077, tolerance = 0.001)
  expect_equal(j$chi_square, expected = 22.537, tolerance = 0.001)
  expect_equal(j$pgof_sig, expected = 0.126, tolerance = 0.001)



  a <- LC_probit((response / total) ~ log10(dose), p = c(50),
               weights = total,
               data = lampreytox,
               subset = c(month == "August"))

  expect_equal(a$dose, expected = 4.009, tolerance = 0.001)
  expect_equal(a$LCL, expected = 3.651, tolerance = 0.001)
  expect_equal(a$UCL, expected = 4.348, tolerance = 0.001)
  expect_equal(a$LCL_dis, expected = 0.357, tolerance = 0.001)
  expect_equal(a$UCL_dis, expected = 0.338, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 21.274, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected = 0.0192, tolerance = 0.0001)


  s <- LC_probit((response / total) ~ log10(dose),
                 p = c(50),
                 weights = total,
                 data = lampreytox,
                 subset = c(month == "September"))

  expect_equal(s$dose, expected = 2.120, tolerance = 0.001)
  expect_equal(s$LCL, expected = 1.981, tolerance = 0.001)
  expect_equal(s$UCL, expected = 2.233, tolerance = 0.001)
  expect_equal(s$LCL_dis, expected = 0.138, tolerance = 0.001)
  expect_equal(s$UCL_dis, expected = 0.113, tolerance = 0.001)
  expect_equal(s$chi_square, expected = 7.163, tolerance = 0.001)
  expect_equal(s$pgof_sig, expected = 0.709, tolerance = 0.001)




})
