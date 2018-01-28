context("Option type scriptr::interval")

logify <- command("Perform a log transformation.") %>%
  argument('x') %>%
  option('--base', type = scriptr::interval(0, Inf, exclude_lower = TRUE),
         help = "Logarithmic base.") %>%
  script(function(base) log(as.numeric(x)) / log(base))

test_that("Values outside of interval throw errors", {
  expect_error(logify(c(1, '--base', -2)))
  expect_error(logify(c(1, '--base', 0)))
})
