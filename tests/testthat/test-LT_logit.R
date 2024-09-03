context("Test LT_logit")

# test if LT_logit is caculating things properly -----
test_that("Determine if LT_logit calculations are correct", {
  m <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lamprey_time,
                subset = c(month == "May"))

  expect_equal(m$time, expected = 9.967, tolerance = 0.001)
  expect_equal(m$LCL, expected = 8.851, tolerance = 0.001)
  expect_equal(m$UCL, expected = 11.901, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 16.616, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.055, tolerance = 0.001)

  j <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lamprey_time,
                subset = c(month == "June"))

  expect_equal(j$time, expected = 13.738, tolerance = 0.001)
  expect_equal(j$LCL, expected = 11.523, tolerance = 0.001)
  expect_equal(j$UCL, expected = 19.595, tolerance = 0.001)
  expect_equal(j$chi_square, expected = 6.327, tolerance = 0.001)
  expect_equal(j$pgof_sig, expected = 0.706, tolerance = 0.001)

  a <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lamprey_time,
                subset = c(month == "August"))

  expect_equal(a$time, expected = 8.169, tolerance = 0.001)
  expect_equal(a$LCL, expected = 7.338, tolerance = 0.001)
  expect_equal(a$UCL, expected = 9.280, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 7.616, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected = 0.573, tolerance = 0.001)

  s <- LT_logit((response / total) ~ log10(hour),
                p = c(50),
                weights = total,
                data = lamprey_time,
                subset = c(month == "September"))

  expect_equal(s$time, expected = 12.445, tolerance = 0.001)
  expect_equal(s$LCL, expected = 10.959, tolerance = 0.001)
  expect_equal(s$UCL, expected = 16.443, tolerance = 0.001)
  expect_equal(s$chi_square, expected = 3.761, tolerance = 0.001)
  expect_equal(s$pgof_sig, expected = 0.926, tolerance = 0.001)


  mm <- LT_logit((response / total) ~ hour, p = c(50),
                 weights = total,
                 data = lamprey_time,
                 log_x = FALSE,
                 subset = c(month == "May"))

  expect_equal(mm$time, expected = 10.082, tolerance = 0.001)
  expect_equal(mm$LCL, expected =  8.843, tolerance = 0.001)
  expect_equal(mm$UCL, expected = 12.298, tolerance = 0.001)
  expect_equal(mm$chi_square, expected = 34.577, tolerance = 0.001)
  expect_equal(mm$pgof_sig, expected = 0.0000707, tolerance = 0.000001)

})

# determine if long and short outputs are working properly ------
test_that("Determine if long and short outputs work ", {
  ma <- LT_logit((response / total) ~ log(hour), p = c(50),
                 weights = total,
                 data = lamprey_time,
                 subset = c(month == "May"),
                 long_output = FALSE)
  expect_equal(ncol(ma), 5)
  expect_equal(nrow(ma), 1)

  may <- LT_logit((response / total) ~ log10(hour), p = c(50),
                  weights = total,
                  data = lamprey_time,
                  subset = c(month == "May"),
                  long_output = TRUE)
  expect_equal(ncol(may), 17)
  expect_equal(nrow(may), 1)


})


# test error when weights are not given -----

test_that("LT_logit throws error when weights are not given", {

  expect_warning(LT_logit(cbind(response, survive) ~ log10(dose),
                          p = 50,
                          data = lamprey_tox,
                          subset = c(month == "May")))
})



# warning for not suppling p -----
test_that("LC_logit throws warning when p is not supplied", {

  expect_warning(LT_logit((response / total) ~ log10(dose),
                          weights = total,
                          data = lamprey_tox,
                          subset = c(month == "May")))
})
