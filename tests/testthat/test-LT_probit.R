context("Test LT_probit")


# test if LT_probit runs properly -----
test_that("Determine if LT_probit runs properly", {
  m <- LT_probit((response / total) ~ log10(hour),
                 p = c(50),
                 weights = total,
                 data = lamprey_time,
                 subset = c(month == "May"))

  expect_equal(m$time, expected = 9.991, tolerance = 0.001)
  expect_equal(m$LCL, expected = 8.934, tolerance = 0.001)
  expect_equal(m$UCL, expected = 11.742, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 13.499, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.1412, tolerance = 0.0001)

  j <- LT_probit((response / total) ~ log10(hour),
                 p = c(50),
                 weights = total,
                 data = lamprey_time,
                 subset = c(month == "June"))

  expect_equal(j$time, expected = 14.128, tolerance = 0.001)
  expect_equal(j$LCL, expected = 11.664, tolerance = 0.001)
  expect_equal(j$UCL, expected = 20.758, tolerance = 0.001)
  expect_equal(j$chi_square, expected = 5.191, tolerance = 0.001)
  expect_equal(j$pgof_sig, expected =  0.8173, tolerance = 0.0001)

  a <- LT_probit((response / total) ~ log10(hour),
                 p = c(50),
                 weights = total,
                 data = lamprey_time,
                 subset = c(month == "August"))

  expect_equal(a$time, expected = 8.184, tolerance = 0.001)
  expect_equal(a$LCL, expected = 7.357, tolerance = 0.001)
  expect_equal(a$UCL, expected = 9.311, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 6.651, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected =  0.6733, tolerance = 0.0001)

  s <- LT_probit((response / total) ~ log10(hour),
                 p = c(50),
                 weights = total,
                 data = lamprey_time,
                 subset = c(month == "September"))

  expect_equal(s$time, expected = 12.611, tolerance = 0.001)
  expect_equal(s$LCL, expected = 10.981, tolerance = 0.001)
  expect_equal(s$UCL, expected = 17.210, tolerance = 0.001)
  expect_equal(s$chi_square, expected = 3.030, tolerance = 0.001)
  expect_equal(s$pgof_sig, expected = 0.9630, tolerance = 0.0001)

  mm <- LT_probit((response / total) ~ hour, p = c(50),
                  weights = total,
                  data = lamprey_time,
                  log_x = FALSE,
                  subset = c(month == "May"))

  expect_equal(mm$time, expected = 10.071, tolerance = 0.001)
  expect_equal(mm$LCL, expected = 8.898, tolerance = 0.001)
  expect_equal(mm$UCL, expected = 12.048, tolerance = 0.001)
  expect_equal(mm$chi_square, expected = 30.313, tolerance = 0.001)
  expect_equal(mm$pgof_sig, expected = 0.000388, tolerance = 0.000001)

})

# test long and short outputs ----
test_that("Determine if LT_probit runs properly", {

  ma <- LT_probit((response / total) ~ log(hour), p = c(50),
                  weights = total,
                  data = lamprey_time,
                  subset = c(month == "May"),
                  long_output = FALSE)
  expect_equal(ncol(ma), 5)
  expect_equal(nrow(ma), 1)

  may <- LT_probit((response / total) ~ log10(hour), p = c(50),
                   weights = total,
                   data = lamprey_time,
                   subset = c(month == "May"),
                   long_output = TRUE)
  expect_equal(ncol(may), 19)
  expect_equal(nrow(may), 1)


})

# test error when weights are not given -----

test_that("LT_probit throws error when weights are not given", {

  expect_warning(LT_probit((response / total) ~ log10(dose),
                           p = 50,
                           data = lamprey_tox,
                           subset = c(month == "May")))
})


# warning for not suppling p -----
test_that("LT_probit throws warning when p is not supplied", {

  expect_warning(LT_probit((response / total) ~ log10(dose),
                           weights = total,
                           data = lamprey_tox,
                           subset = c(month == "May")))
})
