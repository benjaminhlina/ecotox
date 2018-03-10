context("Test LTprobit")

test_that("Determine if LTprobit runs properly", {
  LT_probit((response / total) ~ log10(hour),
            p = c(50, 99),
            weights = total,
            data = lampreytime,
            subset = c(month == "May"))
})
