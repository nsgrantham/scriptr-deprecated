context("Option type scriptr::choice")

mathop <- script("Perform simple algebra on two numbers.") %>%
  argument('a') %>%
  argument('b') %>%
  option('--op', type = scriptr::choice(c('add', 'sub')),
         help = "Mathematical operation to perform.") %>%
  command(function(a, b, op) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    print(switch(op,
           add = a + b,
           sub = a - b))
  })

test_that("Invalid choice throws error", {
  expect_error(mathop(c(12, 3, '--op', 'div')))
  expect_error(mathop(c(12, 3, '--op', 'mul')))
})

test_that("Operates as expected", {
  expect_output(mathop(c(12, 3, '--op', 'add')), '15')
  expect_output(mathop(c(12, 3, '--op', 'sub')), '9')
})
