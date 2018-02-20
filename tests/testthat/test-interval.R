context("Option type scriptr::interval")

pos_reals <- interval(0, Inf, exclude_lower = TRUE)
logify <- script("Perform a log transformation.") %>%
  argument("x", nargs = 1, type = pos_reals) %>%
  option("--base", type = pos_reals, help = "Logarithmic base.") %>%
  command(function(x, base) print(log(x) / log(base)))

test_that("Values outside of interval throw errors", {
  expect_error(logify(c(1, "--base", -2)))
  expect_error(logify(c(1, "--base", 0)))
})

test_that("Logarithm performs as expected", {
  expect_output(logify(c(10, "--base", 10)), "1")
  expect_output(logify(c(1000, "--base", 10)), "3")
})
