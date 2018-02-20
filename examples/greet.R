library(scriptr)

greet <- script("Print a warm greeting.") %>%
  argument("name", help = "Name to be greeted.") %>%
  option("--count", "-c", default = 1, help = "Number of times to greet.") %>%
  option("--lang", "-l", default = "en",
         type = choice(c("ch", "de", "en", "es", "fr", "ja", "ru", "se")),
         help = "Language to greet in.") %>%
  option("--yell", "-y", flag = TRUE, help = "Greet with enthusiasm!") %>%
  command(function(name, count, lang, yell) {
    hello <- list(
      ch = "你好",
      de = "Hallo",
      en = "Hello",
      es = "Hola",
      fr = "Bonjour",
      ja = "こんにちは",
      ru = "Привет",
      se = "Hallå"
    )
    begin <- if (yell && lang == "es") "¡" else ""
    end <- if (yell) "!" else ""
    greeting <- paste(hello[[lang]], name)
    greeting <- paste0(begin, greeting, end)
    print(rep(greeting, count))
  })

greet()
