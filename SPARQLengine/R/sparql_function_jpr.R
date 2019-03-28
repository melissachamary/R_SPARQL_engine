library("jsonlite")#check imports
library("httr")
library("data.table")




#' @title sparqlGet
#' sparql query using get http method
#'
#'@param url endpoint url
#'@param sparql name of data frame on which foreign key constraint is checked
#'@param type STATIC to csv value

#'@return data.table results
#'@import httr
#'@import data.table
#'
#' @export
#'
sparqlGet <- function(url, sparql, type = "csv") {
  url <- paste0(url, "/query")
 r <- GET(url, query = list(query = sparql), accept("text/csv"))

    csv <- httr::content(r, as = "text")
  dt  <- fread(csv, header = T, sep = ",")
  dtclean <- dt[ , lapply(.SD, function(x) gsub("http:\\/\\/[a-zA-Z0-9.\\/-]+[\\/#]{1}", "", x))]
  return(dtclean)
}

###### UNUSED function


sparqlPost <- function(url, sparql) {
  url <- paste0(url, "/update")
  r <- POST(url, body = sparql)
  # TODO check 204 status
  return(r)
}
sparqlUpload <- function(url, file) {
  url <- paste0(url, "/data")
  r <- POST(url, body = upload_file(file, "text/turtle"))
  return(r)
}
