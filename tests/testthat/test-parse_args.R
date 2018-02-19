context('Verify arguments are parsed correctly')

greet_script <- script('Print a warm greeting.') %>%
  option('--name', '-n', type = 'character', help = 'Name to be greeted.') %>%
  option('--count', '-c', default = 1, help = 'Number of times to greet.') %>%
  option('--lang', '-l', default = 'en', type = choice('en', 'es', 'se', 'jp'),
         help = 'Language to greet in.') %>%
  option('--yell', '-y', flag = TRUE, help = 'Greet with enthusiasm!')

test_that("Script with variety of options is parsed correctly by getopt", {
  expect_equal(length(parse_args(greet_script, c())), 0)
  expect_equal(parse_args(greet_script, c('--name=World'))$name, 'World')
  expect_equal(parse_args(greet_script, c('-n', 'World'))$name, 'World')
  expect_true(parse_args(greet_script, c('--yell'))$yell, TRUE)
  expect_true(parse_args(greet_script, c('-y'))$yell, TRUE)
  p1 <- parse_args(greet_script, c('--name=World', '--count=4'))
  p2 <- parse_args(greet_script, c('-n', 'World', '-c4'))
  p3 <- parse_args(greet_script, c('-n', 'World', '--count=4'))
  expect_equal(p1$name, 'World')
  expect_equal(p2$name, 'World')
  expect_equal(p3$name, 'World')
  expect_equal(p1$count, 4)
  expect_equal(p2$count, 4)
  expect_equal(p3$count, 4)
})

scp <- script("Example #1 with multiple arguments.") %>%
  argument('first', nargs = 3) %>%
  argument('last')

test_that('getopt parses valid arguments correctly for example', {
  o <- parse_args(scp, c("One", "Two", "Three", "Four"))
  expect_equal(o$first, c("One", "Two", "Three"))
  expect_equal(o$last, "Four")
})

scp <- script("Example #1 with multiple arguments.") %>%
  argument("first") %>%
  argument("middle", nargs = Inf) %>%
  argument("last")

test_that('getopt parses valid arguments correctly for example', {
  o <- parse_args(scp, c("One", "Two", "Three", "Four"))
  expect_equal(o$first, "One")
  expect_equal(o$middle, c("Two", "Three"))
  expect_equal(o$last, "Four")
})

scp <- script("Example #2 with mixed arguments and options.") %>%
  option("--myopt", "-m", type = "character", help = "A character option") %>%
  argument("first") %>%
  argument("middle", nargs = Inf) %>%
  argument("last")

test_that('getopt parses valid arguments correctly for example', {
  o <- parse_args(scp, c("--myopt", "String", "One", "Two", "Three", "Four"))
  expect_equal(o$myopt, "String")
  expect_equal(o$first, "One")
  expect_equal(o$middle, c("Two", "Three"))
  expect_equal(o$last, "Four")
})
