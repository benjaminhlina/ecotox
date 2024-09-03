context("Test LC_logit")

# test if caculatins are correct ----
test_that("Determine if LC_logit calculations are correct", {
  m <- LC_logit((response / total) ~ log10(dose), p = c(50),
                weights = total,
                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                subset = c(month == "May"))

  expect_equal(m$dose, expected = 1.256, tolerance = 0.001)
  expect_equal(m$LCL, expected = 1.188, tolerance = 0.001)
  expect_equal(m$UCL, expected = 1.312, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 15.213, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.509, tolerance = 0.001)

  j <- LC_logit((response / total) ~ log10(dose), p = c(50),
                weights = total,
                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                subset = c(month == "June"))

  expect_equal(j$dose, expected = 2.654, tolerance = 0.001)
  expect_equal(j$LCL, expected = 2.603, tolerance = 0.001)
  expect_equal(j$UCL, expected = 2.710, tolerance = 0.001)
  expect_equal(j$chi_square, expected = 20.690, tolerance = 0.001)
  expect_equal(j$pgof_sig, expected = 0.190, tolerance = 0.001)



  a <- LC_logit((response / total) ~ log10(dose), p = c(50),
                weights = total,
                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                subset = c(month == "August"))

  expect_equal(a$dose, expected = 4.013, tolerance = 0.001)
  expect_equal(a$LCL, expected = 3.621, tolerance = 0.001)
  expect_equal(a$UCL, expected = 4.385, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 21.908, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected = 0.0155, tolerance = 0.0001)


  s <- LC_logit((response / total) ~ log10(dose), p = c(50),
                weights = total,
                data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                subset = c(month == "September"))

  expect_equal(s$dose, expected = 2.118, tolerance = 0.001)
  expect_equal(s$LCL, expected = 1.984, tolerance = 0.001)
  expect_equal(s$UCL, expected = 2.230, tolerance = 0.001)
  expect_equal(s$chi_square, expected = 7.717, tolerance = 0.001)
  expect_equal(s$pgof_sig, expected = 0.656, tolerance = 0.001)

  mm <- LC_logit((response / total) ~ dose, p = c(50),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 log_x = FALSE,
                 subset = c(month == "May"))
  expect_equal(mm$dose, expected = 1.279, tolerance = 0.001)
  expect_equal(mm$LCL, expected = 1.215, tolerance = 0.001)
  expect_equal(mm$UCL, expected = 1.335, tolerance = 0.001)
  expect_equal(mm$chi_square, expected = 16.593, tolerance = 0.001)
  expect_equal(mm$pgof_sig, expected =  0.412, tolerance = 0.001)

})

# test if long and short outputs -----
test_that("Determine if long and short outputs work", {
  ma <- LC_logit((response / total) ~ log10(dose), p = c(50),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "May"),
                 long_output = FALSE)
  expect_equal(ncol(ma), 5)
  expect_equal(nrow(ma), 1)

  may <- LC_logit((response / total) ~ log10(dose), p = c(50),
                  weights = total,
                  data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                  subset = c(month == "May"),
                  long_output = TRUE)
  expect_equal(ncol(may), 19)
  expect_equal(nrow(may), 1)

})

# test errors for not supplying weights ----
test_that("test error for not supplying weights", {

  expect_warning(LC_logit(cbind(response, survive) ~ log10(dose),
                          p = 50,
                          data = lamprey_tox,
                          subset = c(month == "May")))

})


# warning for not suppling p -----
test_that("LC_logit throws warning when p is not supplied", {

  expect_warning(LC_logit((response / total) ~ log10(dose),
                          weights = total,
                          data = lamprey_tox,
                          subset = c(month == "May")))
})
