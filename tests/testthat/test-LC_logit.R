context("Test LClogit")

test_that("Determine if LClogit calculations are correct", {
  LC_logit((dead / total) ~ log10(dose), p = c(50, 99),
          weights = lampreytox[c(1:19), ]$total, data = lampreytox[c(1:19), ])
})
