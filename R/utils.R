
#' Check if string is valid long option name
#' @param x String
is.long_opt <- function(x) {
  startsWith(x, "--")
}

#' Check if string is valid short option name
#' @param x String
is.short_opt <- function(x) {
  !is.long_opt(x) && startsWith(x, "-")
}

#' Check if string is valid variable name
#' @param x String
is.valid_name <- function(x) {
  make.names(x) == x
}

#' Check if numeric value is an integer
#' (Does not use the name is.integer because it is already defined in base)
#' @param x Numeric
is.wholenumber <- function(x) {
  x %% 1 == 0
}

#' Helper function to merge lists (left to right)
#'
#' @param ... Lists to merge
merge_lists <- function(...) {
  lists <- list(...)
  merged_list <- list()
  for (list_ in lists) {
    stopifnot(is.list(list_))
    for (name in names(list_)) {
      merged_list[[name]] <- list_[[name]]
    }
  }
  merged_list
}

#' Simple function to remove option prefix
#' @param x String beginning with '-' or '--'
#' @importFrom stringr str_sub str_length
remove_prefix <- function(x) {
  if (is.long_opt(x))  return(str_sub(x, 3, str_length(x)))
  if (is.short_opt(x)) return(str_sub(x, 2, str_length(x)))
  x
}

#' Convert x to type
#' @param x String
#' @param type String of R type
as.type <- function(x, type) {
  switch(type,
         character = as.character(x),
         logical = as.logical(x),
         integer = as.integer(x),
         double = as.double(x),
         numeric = as.numeric(x),
         complex = as.complex(x),
         x)
}
