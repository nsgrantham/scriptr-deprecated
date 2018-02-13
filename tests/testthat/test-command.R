
cmd <- command("My description.")
cmd <- option(cmd, "--verbose", "-v", flag = TRUE)

expect_true(class(cmd) == "command")
expect_true(class(cmd$params[[1]]) == "option")
