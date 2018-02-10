context("test-LCprobit.R")

test_that("Determine if LCprobit calculations are correct", {

LC_probit((dead / total) ~ log10(dose), p = c(50, 99),
               weights = total,
               data = lampreytox,
               subset = c(month == "May"))
})
