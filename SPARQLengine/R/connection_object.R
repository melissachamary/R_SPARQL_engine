#' @title connection_object
#' class enable to connect
#' @export
#'
setClass("connection_object", representation=
        representation(sparqlEndpoint_url= "character", user = "character", pwd = "character", authentication_protocol="character"),
        prototype = prototype(
          sparqlEndpoint_url= character(0),
           user="",
           pwd="",
          authentication_protocol = character(0)
           )
)
#' @title check connection
#' test httr_status of simple connection object call by GET. Stop if status
#' @export
#'
setGeneric(name="check_connection",
           def=function(object){
             standardGeneric("check_connection")
           })
setMethod(f="check_connection",signature="connection_object",function(object){
auth<-getAuthentication(object)
if(is.null(auth)){
  connectionTest<-GET(url = getEndpoint(object))
}else{
  connectionTest<-GET(url=getEndpoint(object), auth )
}
return(list(check=!http_error(connectionTest), http_status=http_status(connectionTest)$message))
})

#### GETTER
#' @export
setGeneric(name="getEndpoint",
           def=function(object){
             standardGeneric("getEndpoint")
           })

#' @export
setMethod(f="getEndpoint",signature="connection_object",function(object){
  object@sparqlEndpoint_url
})

#' @export
setGeneric(name="getAuthentication",
           def=function(object){
             standardGeneric("getAuthentication")
           })
#' @export
setMethod(f="getAuthentication",signature="connection_object",function(object){
  if(all(nchar(c(object@user,object@pwd))>0)){
    print(c(object@user,object@pwd))
    return(authenticate(user = object@user, password =object@pwd, type=object@authentication_protocol))
  }else{
    return(NULL)
  }
})



#' @title create_connection
#' function to create connection_object. It checks that spar
#' @export
#'
#'

create_connection<-function(sparqlEndpoint_url, user = NA, pwd = NA, authentication_protocol=NA){
  error<-"[create_connection] error in sparqlEndpoint_url :\n"
  if(!exists("sparqlEndpoint_url", mode = "character")){
    error<-paste(error, sep = "\t argument exists at character format ?", exists("sparql_engine", mode = "character"))
    stop(error)
  }
  if(length(sparqlEndpoint_url) != 1 ){
    error<- paste(error, sep="\n \t argument isn't a single value (multiple/none)", length(sparql_engine) == 1)
    stop(error)
  }
  if(all(!is.na(c(user, pwd)))){
    if(!authentication_protocol%in%c("basic", "digest", "digest_ie", "gssnegotiate", "ntlm", "any")){
      warning("[create_connection] authentitacion protocol not filled or unknown. \n By default authentication will performed using the protocol basic (curl)")
    authType<-"basic"
    }else{
      authType<-authentication_protocol
    }
    endpoint<-new(Class = "connection_object", sparqlEndpoint_url= sparqlEndpoint_url , user = user, pwd = pwd, authentication_protocol= authType)
  }else {
    print(c(user,pwd))
    if(any(is.na(c(user, pwd))) && !all(is.na(c(user, pwd)))){
      warning("[create_connection] one of user or pwd parameter is missing. \n Endpoint will be created whithout considering authentication parameters")
    }
    endpoint<-new(Class = "connection_object", sparqlEndpoint_url= sparqlEndpoint_url)
  }

  connected<-check_connection(endpoint)
  if(!connected$check){
    stop(paste("[create_connection] url access with argument in function failed. Caused by ", (connected$http_status)))
  }
  return(endpoint)
}
