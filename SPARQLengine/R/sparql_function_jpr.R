library("jsonlite")#check imports
library("httr")
library("data.table")
library("RCurl")




#' @title sparqlQuery
#' sparql query using POST curl method
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
sparqlQuery <- function(url, sparql) {
  type<-"text/csv"
  result<-NULL
  if(class(url)!="connection_object"){
    warning(paste("[sparql] try to create connection_object from using",url))
    connect<-create_connection(sparqlEndpoint_url =url)
  }else{
    connect<-url
  }
  if(!is.null(getAuthentication(connect))){
    httpResponse<-POST(getEndpoint(connect),body=list(query=sparql),encode="form", getAuthentication(connect),
                       accept(type),timeout(100000))
  }else{
    httpResponse<-POST(getEndpoint(connect),body=list(query=sparql),encode="form",
         accept(type),timeout(100000))
  }
  if(!http_error(httpResponse)){
    result<-data.table(content(httpResponse))

  }else{
    warnings(paste("[sparql] process failed. Caused By", http_status(httpResponse)$message))
  }
  return(result)

}




######################################################
###########  TO REMOVE CAUSE UNUSED ##################
######################################################
sparql_OLD <- function(url, sparql, type = "csv") {
  sp<-curlEscape(sparql)
  sys_command<- "curl -X POST -d 'query=<<SPARQL_QUERY>>' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/csv' '<<URL>>' > <<TEMP_FILE>> "
  if(!dir.exists(paste(getwd(),sep="/","temp/"))){
    dir.create(paste(getwd(),sep="/","temp/"))
  }
  file<-tempfile(tmpdir = getwd(),pattern="temp/")
  file.create(file)

  sys_command<-gsub(pattern="<<SPARQL_QUERY>>", replacement=sp,
                    x=sys_command)
  sys_command<-gsub(pattern="<<URL>>", replacement=url,x=sys_command)
  sys_command<-gsub(pattern="<<TEMP_FILE>>", replacement=file,x=sys_command)

  system2(sys_command)
  system(sys_command , wait = TRUE , intern = FALSE)

  dt  <- fread(file, header = T, sep = ",")
  dtclean <- dt[ , lapply(.SD, function(x) gsub("http:\\/\\/[a-zA-Z0-9.\\/-]+[\\/#]{1}", "", x))]
  file.remove(file)
  return(dtclean)
}



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

