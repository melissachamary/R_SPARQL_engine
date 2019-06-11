#' @title SPARQL_query_builder
#' class enable to make some simple and parametric SPARQL queries
#' @export
#'
setClass("SPARQL_query_builder", representation=
           representation(prefix="data.frame",graph="list",method="character",constraint="list"),
         prototype = prototype(
           prefix=data.frame(prefix=character(),uri=character()),
           graph=c(),
           method="SELECT",
           constraint=list(element=c(),parameters=c(),
                           constraintDescription = data.frame(typeConstraint=character(),groupConstraint = character(), string=character()))
         )
)
#### Prefix slot setter, getter #####
#' @title getPrefix
#' @return prefix data.frame for the query
#' @export
setGeneric(name="getPrefix",
           def=function(object){
             standardGeneric("getPrefix")
           })

setMethod(f="getPrefix",signature="SPARQL_query_builder",function(object){
  return(object@prefix)
})

#' @title addPrefix WARNING : unimplemented
#' @return import prefix into the class
#' @export
setGeneric(name="addPrefix",
           def=function(object,prefix_table){
             standardGeneric("addPrefix")
           })
setMethod("addPrefix","SPARQL_query_builder",function(object,prefix_table){
  checkColName<-length(which(!colname(getPrefix(object)) %in% colnames(prefix_table)))==0
  if(checkColName){
    print("coherent colname, data could be imported")
  }else{
    print("not coherent colname, data shouldn't imported")
  }
})

#' @title getGraph
#' @return graph list
#' @export
setGeneric(name="getGraph",
           def=function(object){
             standardGeneric("getGraph")
           })
setMethod(f="getGraph",signature="SPARQL_query_builder",function(object){
  return(object@graph)
})
#' @title getMethod
#' @return return method (select, insert etc.)
#' @export
setGeneric(name="getMethod",
           def=function(object){
             standardGeneric("getMethod")
           })
setMethod(f="getMethod",signature="SPARQL_query_builder",function(object){
  return(object@method)
})

#' @title getConstraint
#' @return list of constraint  that correspond to constaint in SPARQL query (triple pattern)
#' @export
setGeneric(name="getConstraint",
           def=function(object){
             standardGeneric("getConstraint")
           })
setMethod(f="getConstraint",signature="SPARQL_query_builder",function(object){
  return(object@constraint)
})
#' @title getParameterConstraint
#' @return list of parameter that should be filled before the query execution (parameter should looks like "<<[:alpha:]+>>")
#' @export
setGeneric(name="getParameterConstraint",
           def=function(object){
             standardGeneric("getParameterConstraint")
           })
setMethod(f="getParameterConstraint",signature="SPARQL_query_builder",function(object){
  return(object@constraint$parameters)
})

#' @title addConstraint
#' @description add constraint by 1. checking element type and refine parameter element list (if one of the element is parameter). Notice that parameters are identified by the following pattern "<<[:alpha:]+>>"
## @input subj, pred, obj : element corresponding to query element (?xxx) explicit/prefixed uri or parameter
#' @export
setGeneric(name="addConstraint",
           def=function(object,subj, pred, obj){
             standardGeneric("addConstraint")
           })
setMethod(f="addConstraint",signature="SPARQL_query_builder",function(object, subj, pred, obj){
  cst<-getConstraint(object)
  cst$element<-unique(c(cst$element,c(subj,pred,obj)[isQueryVariable(c(subj,pred,obj))]))
  cst$parameters<-unique(c(cst$parameters,unique(c(subj,pred,obj)[isQueryParameter(c(subj,pred,obj))])))
  cst$constraintDescription<-rbind(cst$constraintDescription,
                                   data.frame(typeConstraint="simple",  groupConstraint=NA,string = paste(c(subj,pred,obj), collapse=" ")))
  # Add check constraint
  object@constraint<-cst
  object
})

#' @title addConstraintFrame
#' @description add set of constraint in the object
## @input dataFrame : data.frame with the same column name ad the object constraint data frame
## @input keep : elements that should be return by the query, if keep is empty all query eement will be return (?XXX)
#' @export
setGeneric(name="addConstraintFrame",
           def=function(object, data, keep){
             standardGeneric("addConstraintFrame")
           })
setMethod(f="addConstraintFrame",signature="SPARQL_query_builder",function(object, data, keep){
  print(data)
  if(is.data.frame(data) && nrow(data)>0 && length(which(c("subj","pred","obj")%in%colnames(data)))==3){
    cst<-getConstraint(object)
    cst$constraintDescription<-rbind(cst$constraintDescription,
                                     data.frame(typeConstraint="simple",  groupConstraint=NA,
                                                string = unlist(apply(X=data[,c("subj","pred","obj")],1,
                                                                      FUN = function(X){
                                                                        paste(X, collapse=" ")
                                                                      }))
                                     ))
    queryKeep<-keep[isQueryVariable(keep)]
    if(is.null(keep)||is.na(keep)){
      element<-character()
      element<-c(element,as.character(data$subj[which(isQueryVariable(data$subj))]),
                 as.character(data$obj[which(isQueryVariable(data$obj))]),
                 as.character(data$pred[which(isQueryVariable(data$pred))])
      )
      print(data$subj[which(isQueryVariable(data$subj))])
      cst$element<-unique(c(cst$element,element))
    }else if(length(which(queryKeep %in% unlist(as.vector(data))))==length(queryKeep)){
      cst$element<-unique(c(cst$element,unique(queryKeep)))
    }else{
      print("query keep issue")
      print(keep)
      print(queryKeep[which(!queryKeep %in% unlist(as.vector(data)))])
    }
    parameter<-character()
    parameter<-c(parameter,as.character(data$subj[which(isQueryParameter(data$subj))]),
                 as.character(data$obj[which(isQueryParameter(data$obj))]),
                 as.character(data$pred[which(isQueryParameter(data$pred))])
    )
    cst$parameters<-unique(c(cst$parameters,
                             parameter))

    object@constraint<-cst

  }else{
    print(paste("[addConstraintFrame] warning no constraint added"))
  }
  # Add check constraint
  return(object)
})


isQueryParameter<-function(x){
  return(str_detect("^<<[a-zA-Z-_]*>>$",string = x))}
isQueryVariable<-function(x){
  return(str_detect("^[?][a-zA-Z-_]+$",string = x))
}
isFixedValue<-function(x){
  uriValue<-str_detect("^<[a-zA-Z0-9-_://#.]+>$",string = x)
  prefixShortenValue<-str_detect("^[a-zA-Z]+:[a-zA-Z]+$",string = x)
  dataValue #to add in further version
  return((uriValue || prefixShortenValue))
}


#### BUILD Query #####
#' @title build
#' @description buold the corresponding query in the object (sparql_query_object)
## @input param_data : data.frame that elicit element parameter in the query, it should be contain 2 columns : parameter, value and contain exactly 1 row by query parameter
## @input keep : elements that should be return by the query, if keep is empty all query eement will be return (?XXX)
#' @export
setGeneric(name="build",
           def=function(object,param_data){
             standardGeneric("build")
           })
setMethod(f="build",signature="SPARQL_query_builder",function(object,param_data){
  print(object)

  pref_string<-paste(unlist(apply(X = getPrefix(object), 1,FUN = function(X){
    return(computePrefix(uri=X[2],prefix=X[1]))
  })),collapse = "")
  from_string<- paste(unlist(lapply(X = getGraph(object), FUN = function(X){
    return(computeGraph(X))
  })),collapse=" ")

  selection_string<-computeSelect(getConstraint(object)[["element"]])

  if(length(getParameterConstraint(object))==0){
    bodyQuery_string<-computeBodyConstraint(getConstraint(object))
  }else {
    bodyQuery_string<-computeBodyConstraint(getConstraint(object),param_data )

  }
  return(paste(c(pref_string, selection_string, from_string, bodyQuery_string),collapse=" \n "))

})



computePrefix<-function(prefix, uri){
  return(paste("PREFIX",
               paste(prefix,sep=":<",
                     paste(uri,sep="","> \n "))))
}

computeGraph<-function(graph){
  if(!is.null(graph) && !is.na(graph)){
    return(paste("FROM", sep=" <",
                 paste(graph,sep="",
                       "> \n ")))
  }else{
    return("")
  }

}

computeSelect<-function(elements){
  if(is.null(elements)){
    data <-"*"
  }else{
    data <- paste(unique(elements), collapse = " ")
  }
  print(data)

  return(paste("SELECT", data))
}


computeBodyConstraint<-function(constraint_data, param_data=NULL){

  #### envoie une liste maintenant
  c_data<-constraint_data[["constraintDescription"]]
  p_data<-constraint_data[["parameters"]]
  data<-character(0)
  #-- pas de contrainte
  if(nrow(c_data) == 0){
    data <-"?s ?p ?o"
  }else{
    data <- paste(unique(c_data$string), collapse=" . \n ")
    if( length(p_data) >0 ){
      if(! missing(param_data) && setequal(names(param_data),c("parameter","value"))
         && length(setdiff(p_data, param_data$parameter))==0){
        if(length(unique(param_data$parameter)) < length(param_data$parameter)){
          warning("[build Body string] some parameter data have multiple value in parameter_data")
        }
        for(p in 1:nrow(param_data)){
          data<-gsub(x=data, pattern = param_data$parameter[p],replacement = param_data$value[p])
        }
        print(data)
      }else{
        data<-character(0)
        print(param_data$parameter)
        print(p_data)
        stop("[build Body string] missing parameters or unapporpriate parameter_data structure")
      }
    }
    #-- contrainte existante
  }
  return(paste(c(" WHERE{",data,"}"),collaspe =" "))
}

computeBodyConstraint_old<-function(constraint_data){
  if(nrow(constraint_data) == 0){
    data <-"?s ?p ?o"
  }else{
    data <- paste(unique(constraint_data$string), collapse=" . \n ")
  }
  return(paste(c(" WHERE{",data,"}"),collaspe =" "))
}





