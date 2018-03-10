context("Test LTlogit")

test_that("Determine if LTlogit calculations are correct", {
  LT_logit((response / total) ~ log10(hour),
           p = c(50, 99),
           weights = total,
           data = lampreytime,
           subset = c(month == "May"))
})
