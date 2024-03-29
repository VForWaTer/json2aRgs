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
  filtered_config_names <- sapply(names(params_config), function(name) {
    x <- params_config[[name]]
    if (!is.null(x$default) && (!exists("optional", x) || isFALSE(x$optional))) {
      return(x$default)
    } else {
      return(NULL)
    }
  })

  # Filter out the NULL values
  filtered_config_names <- filtered_config_names[!sapply(filtered_config_names, is.null)]

  # combine the two lists of parameter names
  params2parse <- unique(c(params_names, names(filtered_config_names)))

  # initiate list to save parsed parameters
  parsed_params <- list()

  # parse parameters
  for (name in params2parse) {
    # type of the parameter
    t <- params_config[[name]][["type"]]

    # get the value from parameters.json
    if (name %in% names(params)) {
       val <- params[[name]]

    # if parameter is not included in parameters.json, go for default value in config
    } else {
       val <- params_config[[name]]$default
    }

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
    } else if (tolower(t) %in% c("integer", "float")) {
      print(t)
      # check for min in params_config
      if ("min" %in% names(params_config[[name]])) {
        min <- params_config[[name]]$min
      } else {
        min <- NULL
      }
      # check for max in params_config
      if ("max" %in% names(params_config[[name]])) {
        max <- params_config[[name]]$max
      } else {
        max <- NULL
      }

      # check whether val is in min and max range
      if (!is.null(min) && !is.null(max)) {
        # check if min is smaller than or equal to max
        if (max <= min) {
          stop(sprintf("There is an error in your parameter configuration / tool.yml, as the given minimum value (%s) for parameter '%s' is higher than or equal to the maximum number (%s).", min, name, max))
        # check if val is between min and max
        } else if (!(min <= val && val <= max)) {
          stop(sprintf("%s is %s, but it must be between %s and %s.", name, val, min, max))
        }
        # check if val is greater than or equal to min
        } else if (!is.null(min) && !(min <= val)) {
          stop(sprintf("%s is %s, but must be higher than or equal to %s.", name, val, min))
        # check if val is smaller than or equal to max
        } else if (!is.null(max) && !(val <= max)) {
          stop(sprintf("%s is %s, but must be smaller than or equal to %s.", name, val, max))
        # return val if not violating min-max constraints
        } else {
          parsed_params[[name]] <- val
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
