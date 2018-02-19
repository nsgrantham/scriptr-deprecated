context("Verify arguments are parsed correctly")

greet_script <- script("Print a warm greeting.") %>%
  option("--name", "-n", type = "character", help = "Name to be greeted.") %>%
  option("--count", "-c", default = 1, help = "Number of times to greet.") %>%
  option("--lang", "-l", default = "en", type = choice("en", "es", "se", "jp"),
         help = "Language to greet in.") %>%
  option("--yell", "-y", flag = TRUE, help = "Greet with enthusiasm!")

test_that("Script with variety of options is parsed correctly by getopt", {
  expect_equal(length(parse_args(greet_script, c())), 0)
  expect_equal(parse_args(greet_script, c("--name=World"))$name, "World")
  expect_equal(parse_args(greet_script, c("-n", "World"))$name, "World")
  expect_true(parse_args(greet_script, c("--yell"))$yell, TRUE)
  expect_true(parse_args(greet_script, c("-y"))$yell, TRUE)
  p1 <- parse_args(greet_script, c("--name=World", "--count=4"))
  p2 <- parse_args(greet_script, c("-n", "World", "-c4"))
  p3 <- parse_args(greet_script, c("-n", "World", "--count", "4"))
  expect_equal(p1$name, "World")
  expect_equal(p2$name, "World")
  expect_equal(p3$name, "World")
  expect_equal(p1$count, 4)
  expect_equal(p2$count, 4)
  expect_equal(p3$count, 4)
})

scp <- script("Example with multiple, finite arguments.") %>%
  argument("first", nargs = 3) %>%
  argument("last")

test_that("Script with finite arguments is parsed correctly by getopt", {
  p <- parse_args(scp, c("one", "two", "three", "four"))
  expect_equal(p$first, c("one", "two", "three"))
  expect_equal(p$last, "four")
  expect_error(parse_args(scp, c("one", "two", "three")))
  expect_error(parse_args(scp, c("one", "two", "three", "four", "five")))
})

scp <- script("Example with multiple arguments, one of which is infinite.") %>%
  argument("first") %>%
  argument("middle", nargs = Inf) %>%
  argument("last")

test_that("Script with an infinite argument", {
  p <- parse_args(scp, c("one", "two", "three", "four"))
  expect_equal(p$first, "one")
  expect_equal(p$middle, c("two", "three"))
  expect_equal(p$last, "four")
  expect_error(parse_args(scp, c("one", "two")))
})

scp <- script("Example with mixed arguments and options.") %>%
  option("--char", "-c", type = "character", help = "A character option") %>%
  option("--flag", "-f", flag = TRUE, help = "A logical option") %>%
  argument("first") %>%
  argument("last", nargs = Inf)

test_that("Script with mixed arguments and options", {
  p <- parse_args(scp, c("--char", "hello", "one", "two", "three"))
  expect_equal(p$char, "hello")
  expect_equal(p$first, "one")
  expect_equal(p$last, c("two", "three"))

  p <- parse_args(scp, c("one", "two", "three", "--flag"))
  expect_equal(p$flag, TRUE)
  expect_equal(p$first, "one")
  expect_equal(p$last, c("two", "three"))

  p <- parse_args(scp, c("one", "--flag", "--char", "hello", "two"))
  expect_equal(p$first, "one")
  expect_equal(p$flag, TRUE)
  expect_equal(p$last, "two")
})
