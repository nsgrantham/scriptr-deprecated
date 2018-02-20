#' Help page
#' @param scp Script object
build_help_page <- function(scp) {
  # Build more advanced help page later
  help_page <- paste0(scp$description, "\n\n")
  help_page <- paste0(help_page, "Options:\n")
  scp <- option(scp, "--help", flag = TRUE,
                help = "Show this message and exit.")
  opts <- get_options(scp)
  texts <- list()
  for (opt_name in names(opts)) {
    texts[[opt_name]] <- build_option_text(opts[[opt_name]])
  }
  max_nchar <- max(sapply(texts, function(x) nchar(x$left)))
  for (text in texts) {
    blanks <- paste0(rep(" ", max_nchar - nchar(text$left)), collapse = "")
    help_page <- paste(help_page, text$left, blanks, text$right)
  }
  help_page
}

#' Build option text
#' @param opt An option
build_option_text <- function(opt) {
  text <- list()
  text$left <- "  "
  if (!is.null(opt$short_opt)) {
    text$left <- paste0(text$left, opt$short_opt, ", ")
  }
  text$left <- paste0(text$left, opt$long_opt)
  text$right <- paste(opt$help, "\n")
  text
}
