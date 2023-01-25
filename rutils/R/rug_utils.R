#' Validate that a JSON file meets a required schema
#'
#' @param jsonparams A JSON file to be validated against a schema
#' @param jsonschema A predefined JSON schema
#' @param cds_package The package that contains the JSON schema
#'
#' @export
validate_parameters <- function(jsonparams, jsonschema, cds_package = "rugplot"){
  schemafile <- base::system.file("extdata", jsonschema, package = cds_package)
  cat(paste("Schema filename",schemafile))
  jsonvalidate::json_validate(jsonparams,schemafile,verbose=TRUE,error=TRUE)
  validate_json_file(jsonparams)
}

#' Reads columns from a file in table format
#'
#' Additional info
#' @param filename a string data file name including the relative path
#' @param select_columns a vector including the column names to be read from the data file
#'
#' @return a data.table object
#' @export
#'
# #' @examples
read_data <- function(filename,select_columns){
  # Check for empty list
  if (length(select_columns)<1) {
    select_columns = NULL
    cat("\nReading all columns\n")
  } else {
    cat("Selected columns",select_columns,"\n")
  }
  tryCatch(cols <- data.table::fread(filename,select = select_columns),
           error = function(c) {
             c$message <- paste0(c$message, " (in ", filename, ")")
             stop(c)
           }
           # ,warning = function(c) {
           #   c
           #   }
  )
  # print(cols)
}

#' This function validates a json structure
#'
#' @param jsonparams a json structure stored in a file name to be validated
#'
#' @return an R list of parameters extracted from the json structure
#' @export
#'
# #' @examples
validate_json_file <- function(jsonparams) {
  if (file.exists(fileparams)){
    tryCatch(lp <-  jsonlite::fromJSON(jsonparams),
             error = function(c) {
               c$message <- paste0(c$message, " (in ", jsonparams, ")")
               stop(c)
             }
    )
  } else {
    message <- paste0("Parameter file '", jsonparams, "' not found.")
    stop(message)
  }
}

#' Display JSON schema
#'
#' Displays detailed information about the parameters required in a JSON object.
#'
#' @param jsonschema a JSON file stored in ext/data
#' @param cds_package a containerized data science package that has the `jsonchema` parameter
#' @return NULL
#' @export
#'
# #' @examples
display_schema <- function(jsonschema,cds_package="rugplot") {
  jsfile <- system.file("extdata",jsonschema, package = cds_package)
  strschema <- paste(readLines(jsfile),collapse="\n")
  class(strschema)
  cat(jsonlite::prettify(strschema))
}

