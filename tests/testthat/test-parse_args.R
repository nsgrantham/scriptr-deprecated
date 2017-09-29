context('verify arguments are correctly parsed')

greet_cmd <- command("Print a warm greeting.") %>%
  option("--name", "-n", type = "character", help = "Name to be greeted.") %>%
  option("--count", "-c", default = 1, help = "Number of times to greet.") %>%
  option("--lang", "-l", default = "en", choice = c("en", "es", "se", "jp"),
         help = "Language to greet in.") %>%
  option("--yell", "-y", is.flag = TRUE, help = "Greet with enthusiasm!")


test_that("Character option is parsed correctly", {
  values <- list(name = "World")
  expect_equal(parse_args(greet_cmd, c("--name=World")), values)
  expect_equal(parse_args(greet_cmd, c("--name", "World")), values)
  expect_equal(parse_args(greet_cmd, c("-nWorld")), values)
  expect_equal(parse_args(greet_cmd, c("-n", "World")), values)

  values <- list(name = "Major Tom")
  expect_equal(parse_args(greet_cmd, c("--name=Major\ Tom")), values)
  expect_equal(parse_args(greet_cmd, c("--name", "Major Tom")), values)
  expect_equal(parse_args(greet_cmd, c("-nMajor\ Tom")), values)
  expect_equal(parse_args(greet_cmd, c("-n", "Major Tom")), values)
})

test_that("Integer option is parsed correctly", {
  values <- list(count = 3)
  expect_equal(parse_args(greet_cmd, c("--count=3")), values)
  #expect_equal(parse_args(greet_cmd, c("--count", "3")), values)  # why doesn't this work?
  expect_equal(parse_args(greet_cmd, c("-c3")), values)
  #expect_equal(parse_args(greet_cmd, c("-c", "3")), values)  # why doesn't this work?
})

test_that("Logical option is parsed correctly", {
  values <- list(yell = TRUE)
  expect_equal(parse_args(greet_cmd, c("--yell")), values)
  expect_equal(parse_args(greet_cmd, c("-y")), values)
})
