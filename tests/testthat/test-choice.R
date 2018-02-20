context("Option type scriptr::choice")

mathop <- script("Perform simple algebra on two numbers.") %>%
  argument("a", type = "numeric") %>%
  argument("b", type = "numeric") %>%
  option("--op", type = choice(c("add", "sub", "mul", "div")),
         help = "Mathematical operation to perform.") %>%
  command(function(a, b, op) {
    print(
      switch(op,
             add = a + b,
             sub = a - b,
             mul = a * b,
             div = a / b
             )
      )
  })

test_that("Invalid choice throws error", {
  expect_error(mathop(c(12, 3, "--op", "x")))
  expect_error(mathop(c(12, "b", "--op", "add")))
})

test_that("Operates as expected", {
  expect_output(mathop(c(12, 3, "--op", "add")), "15")
  expect_output(mathop(c(12, 3, "--op", "sub")), "9")
  expect_output(mathop(c(12, 3, "--op", "mul")), "36")
  expect_output(mathop(c(12, 3, "--op", "div")), "4")
})
