
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
#### Prefix slot setter, getter
setGeneric(name="getPrefix",
           def=function(object){
             standardGeneric("getPrefix")
           })
setMethod(f="getPrefix",signature="SPARQL_query_builder",function(object){
  return(object@prefix)
})

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

setGeneric(name="getGraph",
           def=function(object){
             standardGeneric("getGraph")
           })
setMethod(f="getGraph",signature="SPARQL_query_builder",function(object){
  return(object@graph)
})

setGeneric(name="getMethod",
           def=function(object){
             standardGeneric("getMethod")
           })
setMethod(f="getMethod",signature="SPARQL_query_builder",function(object){
  return(object@method)
})

setGeneric(name="getConstraint",
           def=function(object){
             standardGeneric("getConstraint")
           })
setMethod(f="getConstraint",signature="SPARQL_query_builder",function(object){
  return(object@constraint)
})



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
setGeneric(name="build",
           def=function(object){
             standardGeneric("build")
           })
setMethod(f="build",signature="SPARQL_query_builder",function(object){
  pref_string<-paste(unlist(apply(X = getPrefix(object), 1,FUN = function(X){
      return(computePrefix(uri=X[2],prefix=X[1]))
  })),collapse = "")
  from_string<- paste(unlist(lapply(X = getGraph(object), FUN = function(X){
    return(computeGraph(X))
  })),collapse=" ")

  selection_string<-computeSelect(object@constraint[["element"]])

  bodyQuery_string<-computeBodyConstraint(object@constraint[["constraintDescription"]])
  print(paste(c(pref_string, selection_string, from_string, bodyQuery_string),collapse=" \n"))
  return(paste(c(pref_string, selection_string, from_string, bodyQuery_string),collapse=" \n"))

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
  return(paste("SELECT", data))
}


computeBodyConstraint<-function(constraint_data){
  if(nrow(constraint_data) == 0){
    data <-"?s ?p ?o"
  }else{
    data <- paste(unique(constraint_data$string), collapse=" . \n ")
  }
  return(paste(c(" WHERE{",data,"}"),collaspe =" "))
}






