#' Parse command-line arguments using standard libc functions
#'
#' @param params List of script parameters
#' @param args Command line arguments
#' @export
parse_args <- function(params, args) {
  getopt_params <- lapply(params, prepare_getopt_param)
  # HACK: libc in getopt assumes first element of args vector is the command
  # call and skips it, so give it empty string to skip
  getopt_args <- c("", args)
  getopt_values <- callgetopt(args = getopt_args, opts = getopt_params)
  if (!is.null(attr(getopt_values, "error"))) {
    stop("Error parsing command arguments")
  }
  defaults <- lapply(params, function(param) param$default)
  values <- process_getopt_values(params, getopt_values)
  values <- merge_lists(defaults, values)
  values
}

#' Translate a single parameter element to the format required for getopt
#'
#' @param param List containing parameter details
#' @export
prepare_getopt_param <- function(param) {
  if (class(param) == "option") {
    if (param$type == "logical") {
      opttype <- "flag"
    } else if (!is.null(param$default)) {
      opttype <- "optional"
    } else {
      opttype <- "required"
    }
    name <- ifelse(!is.null(param$long_opt), remove_prefix(param$long_opt), "")
    short <- ifelse(!is.null(param$short_opt), remove_prefix(param$short_opt), "")
    getopt_param <- list(
      name = name,
      opttype = opttype,
      short = short
    )
  }
  getopt_param
}

#' Convert strings returned by getopt into appropriate R types
#'
#' @param params List of script parameters
#' @param values List of script values in strings
process_getopt_values <- function(params, values) {
  for (value_name in names(values)) {
    value <- values[[value_name]]
    param <- params[[value_name]]
    if (value == "" && param$type == "logical") {
      values[[value_name]] <- !param$default
    } else {
      values[[value_name]] <- as.type(value, param$type)
    }
  }
  values
}
