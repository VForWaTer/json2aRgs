#' @title get_parameters
#'
#' @description
#'     This package is intended to be used inside a docker container to parse
#'     parameters from "/in/parameters.json" with the help of
#'     the parameter configuration in "/src/tool.yml".
#'
#' @return A list of the parsed parameters specified in "/in/parameters.json"
#' @examples
#' \dontrun{
#'      params <- get_parameters()
#'      }
#' @export
#' @importFrom jsonlite read_json
#' @importFrom yaml read_yaml
#' @importFrom tools file_ext
#' @importFrom utils read.csv read.table

get_parameters <- function() {
  # get the parameter file env variable
  PARAM_FILE <- Sys.getenv(x = "PARAM_FILE")
  if (PARAM_FILE == "") {
    PARAM_FILE <- "/in/parameters.json"
  }

  # get the config file env variable
  CONF_FILE <- Sys.getenv(x = "CONF_FILE")
  if (CONF_FILE == "") {
    CONF_FILE <- "/src/tool.yml"
  }

  # get the tool name
  TOOL <- tolower(Sys.getenv(x = "TOOL_RUN"))

  # parse the json
  params <- read_json(path = PARAM_FILE, simplifyVector = TRUE)[[TOOL]]
  params_names <- names(params)

  # parse the config yaml, directly access parameters section
  config <- read_yaml(CONF_FILE)
  params_config <- config$tools[[TOOL]]$parameters

  # get all names from params_config that have a default value and are not optional to parse default values
  filtered_config_names <- names(Filter(function(x) !is.null(x$default) && !is.null(x$optional) && x$optional == FALSE, params_config))
  print(filtered_config_names)
  # combine the two lists of parameter names
  params2parse <- unique(c(params_names, filtered_config_names))

  # initiate list to save parsed parameters
  parsed_params <- list()

  # parse parameters
  for (name in params2parse) {
    # type of the parameter
    t <- params_config[[name]][["type"]]

    # get the value
    val <- params[[name]]

    # handle value specific types
    if (t == "enum") {
      if (!(val %in% params_config[[name]]$values)) {
        stop(paste("The value '", val, "' is not contained in [", paste(params_config[[name]]$values, collapse = " "), "]", sep = ""))
      }
    } else if (t %in% c("datetime", "date", "time")) {
      val <- as.POSIXct(val)
    } else if (t == "file") {
      # get the ext and use the corresponding reader
      ext <- tolower(file_ext(val))
      if (ext == "dat") {
        # matrix files: no header, no index
        val <- as.matrix(read.table(val))
        dimnames(val) <- NULL
      } else if (ext == "csv") {
        val <- read.csv(val)
      }
    }

    # parse default values for parameters that are still NULL; a default value exists, as the parameter comes from the filtered_config_names
    if (is.null(val)) {
      val <- params_config[[name]]$default
    }

    # append value to parsed_params
    parsed_params[[name]] <- val
  }
  return(parsed_params)
}
