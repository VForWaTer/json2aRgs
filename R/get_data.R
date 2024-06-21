#' @title get_data
#'
#' @description
#'     This package is intended to be used inside a docker container to parse
#'     data from "/in/input.json" with the help of
#'     the input configuration in "/src/tool.yml".
#'     The data can be parsed by file extension.
#'     If return_data_paths is set to FALSE, the function will return the paths
#'     to the data files. If set to TRUE, the function will try to parse the data.
#' @param return_data_paths Logical. If FALSE (default), the function will return the paths
#'     to the data files. If TRUE, the function will try to parse the data.
#'
#' @return A list of the parsed data specified in "/in/input.json"
#' @examples
#' \dontrun{
#'      data <- get_data()
#'      }
#' @export
#' @importFrom jsonlite read_json
#' @importFrom yaml read_yaml
#' @importFrom tools file_ext
#' @importFrom utils read.csv read.table

get_data <- function(return_data_paths = FALSE) {
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

  # parse the json, access data section
  data <- jsonlite::read_json(path = PARAM_FILE, simplifyVector = TRUE)[[TOOL]][["data"]]

  # parse the config yaml, access data section
  data_config <- yaml::read_yaml(CONF_FILE)$tools[[TOOL]]$data

  # initialize list for data to return
  data_return <- list()

  # parse data
  for (name in names(data)) {
    # get the file path
    path <- data[[name]]

    # get the file extension
    ext <- tolower(tools::file_ext(path))
    # add a dot at the beginning
    ext <- paste(".", ext, sep = "")

    # check if the dataset is given with the correct extension
    if ("extension" %in% names(data_config[[name]])) {
      if (!(ext %in% data_config[[name]]$extension)) {
        stop(paste("The file extension '", ext, "' is not contained in [", paste(data_config[[name]]$extension, collapse = " "), "]", sep = ""))
      }
    }

    # check if the file exists
    if (!file.exists(path)) {
      stop(paste("The file '", path, "' does not exist."))
    }

    # do not parse files by extension if parse_files is FALSE
    if (return_data_paths) {
      data_return[[name]] <- path
      next
    }

    # .dat
    if (ext == ".dat") {
      parsed <- as.matrix(read.table(path)) # matrix files: no header, no index
      dimnames(parsed) <- NULL
    }

    # .csv
    else if (ext == ".csv") {
      parsed <- read.csv(path)
    }

    # not supported to parse
    else {
      parsed <- path
    }

    # append value to parsed_params
    data_return[[name]] <- parsed
  }
  return(data_return)
}
