context("Test LTprobit")

test_that("Determine if LTprobit runs properly", {
  LT_probit((dead / total) ~ log10(hour), p = c(50, 99),
          weights = lampreytime[c(1:19), ]$total, data = lampreytime[c(1:19), ])
})
