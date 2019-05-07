
library(stringr)
library(readxl)
library(openxlsx)
source("R/sparql_function_jpr.R")
source("R/query_builder_object.R")
source("R/query_generation_function.R")
source("R/connection_object.R")


######### TEST ########
#### Connection_Object####
#### Valid URL whithout requiring authentication
sparql_url<-"http://localhost:8890/sparql/"
### Valide URL but authentication is mandatory (401 hhtp error if not)
sparql_auth<-"http://localhost:8890/sparql-auth/"
### REQ 1 : connection_object should check ability of connection and raise error if not that explicit the http status
  ### SPEC 1.1
test_1.1_PASS<-create_connection(sparqlEndpoint_url =sparql_url)
test_1.1_FAIL<-create_connection(sparqlEndpoint_url=sparql_auth) # 401 in CHECK CONNECTION
### REQ 2 : connection_object should check inconsistant parameter set (user whithout pwd) and warn user about what it does
  ### SPEC 2.1 if one of user or pwd is missing, connection object should be created without authentication parameter
test_2.1_WARN<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba")# OK
test_2.1_WARN<-create_connection(sparqlEndpoint_url=sparql_url,pwd = "dba") ## 401 in CHECK CONNECTION
  ### SPEC 2.2 authentication protocol should be used in connection (obvious but has to be checked)
test_2.2_PASS<-create_connection(sparqlEndpoint_url=sparql_auth,user = "dba",pwd = "dba", authentication_protocol = "digest")
test_2.2_PASS<-create_connection(sparqlEndpoint_url=sparql_auth,user = "dba",pwd = "dba", authentication_protocol = "basic")
  ### SPEC 2.3 if authentication_protocol is missing or unknown, warn that by default authentication protocol will be the curl basic one
test_2.3_WARN1<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba",pwd = "dba") # FAIL because of digest authentication protocol
test_2.3_WARN2<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba",pwd = "dba", authentication_protocol = "d") # OK
  ### SPEC 2.4 Fill authentication information (user, pwd and authentication_protocol) not required by endpoint shouldn't raise any error (ignored by server)
test_2.2_PASS1<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba",pwd = "dba", authentication_protocol = "digest")
test_2.2_PASS2<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba",pwd = "dba", authentication_protocol = "basic")

#### SPARQL_QUERY_OBJECT ####
#############################
#### getTemplateQueryParameter function
### REQ  : should either create R list object and xlsx file if parameter file is correct
getTemplateQueryParameter()
getTemplateQueryParameter(paste(getwd(),sep='',"/test/query_object_data/"))
getTemplateQueryParameter(paste(getwd(),sep='',"/test/query_object_data/test_template.xlsx"))
getTemplateQueryParameter(paste(getwd(),sep='',"/tes"))

### Check
#
### createQueryObjectListFromXX function
createQueryObjectListFromList()
"/test/query_object_data/TEST1-1_COLUMN_MISSING.xlsx"
"/test/query_object_data/TEST1-1_SHEET_COL_MISSING.xlsx"
"/test/query_object_data/TEST1-1_SHEET_MISSING.xlsx"
# NOTICE : most of check constraint is done by createQueryObjectListFromList, we focus test on this capacity of this function
#### REQ 1: Structure check
######## SPEC 1.1 all sheet/data.frame should be available each containing at least column describe by getTemplate
test_1.1_FAIL1<-createQueryObjectListFromFile(paste(getwd(),sep="","/test/query_object_data/TEST1-1_SHEET_MISSING.xlsx"))
test_1.1_FAIL2<-createQueryObjectListFromFile(paste(getwd(),sep="","/test/query_object_data/TEST1-1_SHEET_COL_MISSING.xlsx"))
test_1.1_FAIL3<-createQueryObjectListFromFile(paste(getwd(),sep="","/test/query_object_data/TEST1-1_COLUMN_MISSING.xlsx")) ### MARCHE PAS==> PK

test_pass<-createQueryObjectListFromFile(paste(getwd(),sep="","/test/query_object_data/getTemplateQueryParameter.xlsx"))
test_data_file<-"/Users/mimi/Documents/HCL_work/workspace/R_SPARQL_engine/SPARQLengine/test/query_object_data/getTemplateQueryParameter.xlsx"
listQuery_Object<-createQueryObjectListFromFile(test_data_file)
for(i in names(listQuery_Object)){
  if(is.null(listQuery_Object[[i]])){
    print(paste("null element",i))
  }else{
    print(i)
    print(build(listQuery_Object[[i]]))
    a<-buildQuery(listQuery_Object[[i]])
    print(a)
  }
}

#query_result<-POST(url,body=list(query=hgQuery),encode="form",
#authenticate(user="dba", password="dba", type = "digest"),accept("text/csv"))

################## OLD TESTS #################

#### TRIPLESTORE-DATA
prefix_list<-list(generalPrefix = data.frame(prefix= c("rdfs", "rdf",   "owl", "xml","xsd"),
                                             uri= c("http://www.w3.org/2000/01/rdf-schema#", "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                                    "http://www.w3.org/2002/07/owl#","http://www.w3.org/XML/1998/namespace",
                                                    "http://www.w3.org/2001/XMLSchema#")),
                  epitrackPrefix = data.frame(prefix= c("btl2", "cepi", "hepi", "depi" ),
                                              uri= c("http://purl.org/biotop/btl2.owl#",  "http://www.chu-lyon.fr/epitrack/epitrack-core#",
                                                     NA, NA))
)

graph_list<-list(cepi='http://www.epitrack/core-ontology/',
                 hepi='',
                 depi='')
sparql_engine<-"http://localhost:8890/sparql/"



query_builder<-new(Class = "SPARQL_query_builder", prefix=rbind(prefix_list[["generalPrefix"]],
                                                                prefix_list[["epitrackPrefix"]][1:2,]),
                   graph=list(graph_list[["cepi"]]),method='SELECT')
query_builder<-addConstraint(query_builder,subj="?s",obj="cepi:CEPI0000000021", pred="rdfs:subClassOf")
query_builder<-addConstraint(query_builder,subj="?s",obj="?label", pred="rdfs:label")
query_builder<-addConstraint(query_builder,subj="?s",obj="?ATC_CODE", pred="cepi:ATC_code")

query_builder_param<-new(Class = "SPARQL_query_builder", prefix=rbind(prefix_list[["generalPrefix"]],
                                                                      prefix_list[["epitrackPrefix"]][1:2,]),
                         graph=list(graph_list[["cepi"]]),method='SELECT')
query_builder_param<-addConstraint(query_builder_param,subj="?s",obj="<<PARAM_ATC>>", pred="rdfs:subClassOf")
query_builder_param<-addConstraint(query_builder_param,subj="?s",obj="?label", pred="rdfs:label")
query_builder_param<-addConstraint(query_builder_param,subj="?s",obj="?ATC_CODE", pred="cepi:ATC_code")


query_builder_all<-new(Class = "SPARQL_query_builder", prefix=rbind(prefix_list[["generalPrefix"]],
                                                                    prefix_list[["epitrackPrefix"]][1:2,]),
                       graph=list(graph_list[["cepi"]]),method='SELECT')
constraintFrame<-data.frame(subj=c("?s","?s","?s"),pred=c("rdfs:subClassOf","rdfs:label","cepi:ATC_code"),
                            obj=c("cepi:CEPI0000000021","?label", "?ATC_CODE"))
query_builder_all<-addConstraintFrame(query_builder_all,data = constraintFrame,keep=NA)


query_builder_all_param<-new(Class = "SPARQL_query_builder", prefix=rbind(prefix_list[["generalPrefix"]],
                                                                          prefix_list[["epitrackPrefix"]][1:2,]),
                             graph=list(graph_list[["cepi"]]),method='SELECT')
constraintFrame_param<-data.frame(subj=c("?s","?s","?s"),pred=c("rdfs:subClassOf","rdfs:label","cepi:ATC_code"),
                                  obj=c("<<PARAM_ATC>>","?label", "?ATC_CODE"))
query_builder_all_param<-addConstraintFrame(query_builder_all_param,data = constraintFrame_param,keep=NA)

connect
ur<-getEndpoint(connect)
body=list(query=sparql)
POST(getEndpoint(connect),body=list(query=sparql),encode="form",accept("text/csv"),timeout(100000))

parameter_data<-data.frame(parameter = character(0), value=character(0))
parameter_data<-rbind(parameter_data,
                      data.frame(parameter="<<PARAM_ATC>>", value = "cepi:CEPI0000000021"))

a<-build(query_builder_all_param,param_data = parameter_data)
queryString<-build(query_builder_all)

sparqlGet(sparql_engine, queryString, type = "csv")

sparqlGet(sparql_engine, sparql = queryString)
