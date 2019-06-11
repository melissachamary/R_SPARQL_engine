##### DATA RDF GENERATION ####
#-------WRITE TTL parameters ############
prefixUnMutable<-"@prefix xml: <http://www.w3.org/XML/1998/namespace>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix owl: <http://www.w3.org/2002/07/owl#>"
#prefixDataSpecific : should be parametrized
prefixDataSpecific<-"@prefix cepi: <http://www.chu-lyon.fr/epitrack/epitrack-core#>
@prefix hepi: <http://www.chu-lyon.fr/epitrack/epitrack-hospital#>
@prefix depi: <http://www.chu-lyon.fr/epitrack/data/laboratory/model#>
@prefix encoding: <http://www.chu-lyon.fr/epitrack/ontologies/localSemanticStandard#>

@prefix skos: <http://www.w3.org/2004/02/skos/core#>
@prefix btl2: <http://purl.org/biotop/btl2.owl#>
"

insertInitPattern <-""
insertEndPattern <-""

#---- CORE SCRIPT ###############
paramStatements <- as.data.table(read_excel(param_statements_file, sheet = "RDF", col_types = "text"))
paramStatements[, 'statementIndex' := paste("st",sep="_",seq(1:nrow(paramStatements)))]
tablesName<-unique(paramStatements$table)
for(t in tablesName){
  paramData<-paramStatements[table==t]
  # Creation tous les statements => si data = NA ==> NA sur le statement
  for(p in 1:nrow(paramData)){
    w[[t]][,(paramData[p,statementIndex]):= substitute(.SD[[1]],paramData[p,]), .SDcols=c(paramData[p,column])]
  }
  # Conservation des ligne respectant les statements mandatory
  mat<-as.matrix(w[[t]][,.SD, .SDcols = c(paramData[,statementIndex])])
  mat<-t(mat)
  mandatory <-paramData[,mandatory]
  matNA<-is.na(mat)
  keepRow<-apply(X=as.matrix(mat[which(as.logical(mandatory)),]),2, FUN = function(X){return(!is.na(X))})
  if(length(which(as.logical(mandatory)))>1){
    keepRow <- apply(X=keepRow,2, FUN= function(X){return(sum(X))})==length(which(as.logical(mandatory)))
  }
  mat<-mat[,which(keepRow)]
  data_fullstatements<-apply(X=mat[,],2,FUN=function(X){
    return(paste(paste(X[which(!is.na(X))], collapse=" ; \n \t ") , c(". \n")))})

  fileRDF<-paste(fileRDF_path_pattern,sep="_",paste("DATA",sep="_",paste(t,sep=".","ttl")))
  write(file=fileRDF,append = FALSE, x = prefixDataSpecific)
  write(file=fileRDF,append = TRUE, x = prefixUnMutable)

  for(i in data_fullstatements){
    write(file=fileRDF,append = TRUE, x = i)
  }

}

