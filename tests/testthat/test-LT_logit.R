context("Test LTlogit")

test_that("Determine if LTlogit calculations are correct", {
  LT_logit((dead / total) ~ log10(hour), p = c(50, 99),
          weights = lampreytime[c(1:19), ]$total, data = lampreytime[c(1:19), ])
})
