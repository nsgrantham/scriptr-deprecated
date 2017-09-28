context('verify arguments are correctly parsed')

greet_cmd <- command("Print a warm greeting.") %>%
  option("--name", "-n", type = "character", help = "Name to be greeted.") %>%
  option("--count", "-c", default = 1, help = "Number of times to greet.") %>%
  option("--lang", "-l", default = "en", choice = c("en", "es", "se", "jp"),
         help = "Language to greet in.") %>%
  option("--yell", "-y", is.flag = TRUE, help = "Greet with enthusiasm!")


test_that('Example greeter parses correctly', {
  o1 <- parse_args(greet_cmd, c("--name=World"))
  expect_equal(o1, list(name='World'))

  o2 <- parse_args(greet_cmd, c("-n", "World"))
  expect_equal(o2, list(name='World'))
})
