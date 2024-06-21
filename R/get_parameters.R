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
  # get the input file env variable
  PARAM_FILE <- Sys.getenv(x = "PARAM_FILE")
  if (PARAM_FILE == "") {
      PARAM_FILE <- "/in/input.json"
  }

  # get the config file env variable
  CONF_FILE <- Sys.getenv(x = "CONF_FILE")
  if (CONF_FILE == "") {
    CONF_FILE <- "/src/tool.yml"
  }

  # get the tool name
  TOOL <- tolower(Sys.getenv(x = "TOOL_RUN"))

  # parse the json, access parameters section
  params <- jsonlite::read_json(path = PARAM_FILE, simplifyVector = TRUE)[[TOOL]][["parameters"]]

  # parse the config yaml, access parameters section
  params_config <- yaml::read_yaml(CONF_FILE)$tools[[TOOL]]$parameters

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
  params2parse <- unique(c(names(params), names(filtered_config_names)))

  # initiate list to save parsed parameters
  parsed_params <- list()

  # parse parameters
  for (name in params2parse) {
    # type of the parameter
    type <- params_config[[name]][["type"]]

    # get the value from parameters.json
    if (name %in% names(params)) {
      val <- params[[name]]

    # if parameter is not included in parameters.json, go for default value in config
    } else {
      val <- params_config[[name]]$default
    }

    # handle value specific types

    # enumeration
    if (tolower(type) == "enum") {
      if (!(val %in% params_config[[name]]$values)) {
        stop(paste("The value '", val, "' is not contained in [", paste(params_config[[name]]$values, collapse = " "), "]", sep = ""))
      }
    }

    # datetime
    else if (tolower(type) %in% c("datetime", "date", "time")) {
      val <- as.POSIXct(val)
    }

    # boolean
    else if (tolower(type) %in% c("boolean", "bool")) {
      if (!isTRUE(val) && !isFALSE(val)) {
        stop(paste("The value '", val, "' is not a boolean value."))
      }
    }

    # integer and float
    else if (tolower(type) %in% c("integer", "float")) {

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

      # check if val is in min and max range
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
        if (tolower(type) == "integer") {
          val <- as.integer(val)
        } else if (tolower(type) == "float") {
          val <- as.numeric(val)
        }
      }
    }

    # append value to parsed_params
    parsed_params[[name]] <- val
  }

  return(parsed_params)
}
