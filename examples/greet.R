library(scriptr)
library(magrittr)

greet <- command("Print a warm greeting.") %>%
  option("--name", "-n", type = "character", help = "Name to be greeted.") %>%
  option("--count", "-c", default = 1, help = "Number of times to greet.") %>%
  option("--lang", "-l", default = "en", choice = c("en", "es", "se", "ja"), help = "Language to greet in.") %>%
  option("--yell", "-y", is.flag = TRUE, help = "Greet with enthusiasm!") %>%
  script(function(name, count, lang, yell) {
    hello <- list(
      en = "Hello",
      es = "Hola",
      se = "Hallå",
      ja = "こんにちは"
    )
    begin <- ifelse(yell && lang == "es", "¡", "")
    end <- ifelse(yell, "!", "")
    greeting <- paste(hello[[lang]], name)
    greeting <- paste0(begin, greeting, end)
    print(rep(greeting, count))
  })

greet()
