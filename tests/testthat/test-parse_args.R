context('verify arguments are correctly parsed')

greet_cmd <- command("Print a warm greeting.") %>%
  option("--name", "-n", type = "character", help = "Name to be greeted.") %>%
  option("--count", "-c", default = 1, help = "Number of times to greet.") %>%
  option("--lang", "-l", default = "en", choice = c("en", "es", "se", "jp"),
         help = "Language to greet in.") %>%
  option("--yell", "-y", is.flag = TRUE, help = "Greet with enthusiasm!")

test_that('getopt parses valid arguments correctly for greeter example', {
  # no args given
  o0 <- parse_args(greet_cmd, c())
  expect_equal(length(o0), 0)

  # a single long argument
  o1 <- parse_args(greet_cmd, c("--name=World"))
  expect_equal(o1$name, 'World')

  # a single short argument
  o2 <- parse_args(greet_cmd, c("-n", "World"))
  expect_equal(o2$name, 'World')
  expect_equal(attr(o2, 'positional'), character(0))

  # positional arguments
  o3 <- parse_args(greet_cmd, c("-n", "World", "One", "Two"))
  expect_equal(o3$name, 'World')
  expect_equal(attr(o3, 'positional'), c('One', 'Two'))

  # long flag
  o4l <- parse_args(greet_cmd, c("--yell"))
  expect_true('yell' %in% names(o4l))

  # short flag
  o4s <- parse_args(greet_cmd, c("-y"))
  expect_true('yell' %in% names(o4s))

  # multiple long arguments
  o5l <- parse_args(greet_cmd, c("--name=World", "--count=4"))
  expect_equal(o5l$name, 'World')
  expect_equal(o5l$count, '4')

  # multiple short arguments
  o5s <- parse_args(greet_cmd, c("-n", "World", "-c", "4"))
  expect_equal(o5s$name, 'World')
  expect_equal(o5s$count, '4')

  # mixed length arguments
  o5m <- parse_args(greet_cmd, c("--name=World", "-c", "4"))
  expect_equal(o5m$name, 'World')
  expect_equal(o5m$count, '4')
})
