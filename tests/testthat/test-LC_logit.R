context("Test LClogit")

test_that("Determine if LClogit calculations are correct", {
  LC_logit((dead / total) ~ log10(dose),
           p = c(50, 99),
          weights = total,
          data = lampreytox,
          subset = c(month == "May"))
})
