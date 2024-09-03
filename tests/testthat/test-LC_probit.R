context("Test LC_probit")

# test if probits are being calculated correctly ----
test_that("Determine if LC_probit calculations are correct", {
  m <- LC_probit((response / total) ~ log10(dose), p = 50,
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "May"))
  expect_equal(m$dose, expected = 1.250, tolerance = 0.001)
  expect_equal(m$LCL, expected = 1.184, tolerance = 0.001)
  expect_equal(m$UCL, expected = 1.306, tolerance = 0.001)
  expect_equal(m$chi_square, expected = 14.113, tolerance = 0.001)
  expect_equal(m$pgof_sig, expected = 0.590, tolerance = 0.001)

  j <- LC_probit((response / total) ~ log10(dose),
                 p = c(50),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "June"))
  expect_equal(j$dose, expected = 2.659, tolerance = 0.001)
  expect_equal(j$LCL, expected = 2.590, tolerance = 0.001)
  expect_equal(j$UCL, expected = 2.737, tolerance = 0.001)
  expect_equal(j$chi_square, expected = 22.537, tolerance = 0.001)
  expect_equal(j$pgof_sig, expected = 0.126, tolerance = 0.001)



  a <- LC_probit((response / total) ~ log10(dose), p = c(50),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "August"))

  expect_equal(a$dose, expected = 4.009, tolerance = 0.001)
  expect_equal(a$LCL, expected = 3.651, tolerance = 0.001)
  expect_equal(a$UCL, expected = 4.348, tolerance = 0.001)
  expect_equal(a$chi_square, expected = 21.274, tolerance = 0.001)
  expect_equal(a$pgof_sig, expected = 0.0192, tolerance = 0.0001)


  s <- LC_probit((response / total) ~ log10(dose),
                 p = c(50),
                 weights = total,
                 data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                 subset = c(month == "September"))

  expect_equal(s$dose, expected = 2.120, tolerance = 0.001)
  expect_equal(s$LCL, expected = 1.981, tolerance = 0.001)
  expect_equal(s$UCL, expected = 2.233, tolerance = 0.001)
  expect_equal(s$chi_square, expected = 7.163, tolerance = 0.001)
  expect_equal(s$pgof_sig, expected = 0.709, tolerance = 0.001)

  mm <- LC_probit((response / total) ~ dose, p = c(50),
                  weights = total,
                  data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                  log_x = FALSE,
                  subset = c(month == "May"))
  expect_equal(mm$dose, expected = 1.282, tolerance = 0.001)
  expect_equal(mm$LCL, expected = 1.22, tolerance = 0.001)
  expect_equal(mm$UCL, expected = 1.338, tolerance = 0.001)
  expect_equal(mm$chi_square, expected = 16.060, tolerance = 0.001)
  expect_equal(mm$pgof_sig, expected =  0.448, tolerance = 0.001)
})

# test long and short outputs -----
test_that("Determine if long and short outputs work properly", {


  ma <- LC_probit((response / total) ~ log10(dose), p = c(50),
                  weights = total,
                  data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                  subset = c(month == "May"),
                  long_output = FALSE)
  expect_equal(ncol(ma), 5)
  expect_equal(nrow(ma), 1)

  may <- LC_probit((response / total) ~ log10(dose), p = c(50),
                   weights = total,
                   data = lamprey_tox[lamprey_tox$nominal_dose != 0, ],
                   subset = c(month == "May"),
                   long_output = TRUE)
  expect_equal(ncol(may), 19)
  expect_equal(nrow(may), 1)




})

# test error when weights are not given -----

test_that("LC_probit throws error when weights are not given", {

  expect_warning(LC_probit(cbind(response, survive) ~ log10(dose),
                           p = 50,
                           data = lamprey_tox,
                           subset = c(month == "May")))
})


# warning for not suppling p -----
test_that("LC_probit throws warning when p is not supplied", {

  expect_warning(LC_probit((response / total) ~ log10(dose),
                           weights = total,
                           data = lamprey_tox,
                           subset = c(month == "May")))
})
