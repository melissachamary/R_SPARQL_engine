library(httr)
#############################################

##############################################
http_error("http://localhost:8890/sparql-auth")
hgQuery<-"PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
 PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl:<http://www.w3.org/2002/07/owl#>
PREFIX xml:<http://www.w3.org/XML/1998/namespace>
PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>
PREFIX btl2:<http://purl.org/biotop/btl2.owl#>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
PREFIX cepi:<http://www.chu-lyon.fr/epitrack/epitrack-core#>
PREFIX hepi:<http://www.chu-lyon.fr/epitrack/epitrack-hospital#>
PREFIX encoding:<http://www.chu-lyon.fr/epitrack/ontologies/localSemanticStandard#>
PREFIX depi:<http://www.chu-lyon.fr/epitrack/data/laboratory/model#>

SELECT ?uri_ast ?ast_label ?SIR_lab ?uri_ident ?ident_label ?ast_label_shorten ?ident_label_shorten ?uri_sample ?label_type_sample ?date
FROM <http://www.epitrack/core-ontology/>
FROM <http://www.epitrack/hospital-ontology/>
FROM <http://www.epitrack/encoding-standards/>
FROM <http://www.epitrack/data/microbiology-data/>

WHERE{
?uri_ast rdf:type depi:DEPI0000000001 .
?uri_ast depi:hasTest ?test_uri .
?test_uri rdfs:label ?ast_label .
?test_uri rdfs:subClassOf+ <http://www.chu-lyon.fr/epitrack/ontologies/localSemanticStandard#GLIMS-ATB> .
?uri_ast depi:hasInterpretation ?SIR_uri .
?SIR_uri rdfs:label ?SIR_lab .
?uri_ast depi:refineObservation ?uri_ident .
?uri_ident depi:hasResult ?uri_encodingIdent .
?uri_encodingIdent rdfs:label ?ident_label .
?test_uri cepi:source_id ?ast_label_shorten .
?uri_encodingIdent cepi:source_id ?ident_label_shorten .
?uri_ident depi:observedOn ?uri_sample .
?uri_sample rdf:type ?uri_sample_type .
?uri_sample_type rdfs:subClassOf <http://www.chu-lyon.fr/epitrack/ontologies/localSemanticStandard#GLIMS-SAMPLE> .
?uri_sample_type rdfs:label ?label_type_sample .
?uri_sample depi:date ?date .
?uri_sample depi:inWard ?ward .
?ward btl2:partOf hepi:HG2
}
"
url<-"http://localhost:8890/sparql-auth/"
query_result<-POST(url,body=list(query=hgQuery),encode="form",
           authenticate(user="dba", password="dba", type = "digest"),accept("text/csv"))
query_data<-content(query_result)

query_result<-POST(url,body=list(query=hgQuery),encode="form",
                   authenticate(user="dba", password="dba", type = "digest"),accept("text/csv"))
query_data<-content(query_result)

query_result<-POST(url,body=list(query=hgQuery),encode="form",
                   config=list(authenticate(user="dba", password="dba", type = "basic")),accept("text/csv"),timeout(100000))
query_data<-content(query_result)

query_result<-POST("http://localhost:8890/sparql-graph-crud-auth/",body=list(query=hgQuery),encode="form",
                   config=list(authenticate(user="dba", password="dba", type = "basic")),accept("text/csv"),timeout(100000))
query_data<-content(query_result)

test_insert<-POST(url,body=list(query="CREATE Graph <DEBILE_PROFOND_1>"),encode="form",
                 config=list(authenticate(user="dba", password="dba", type = "basic")),accept("text/csv"),timeout(100000))

test_insert<-POST("http://localhost:8890/sparql-graph-crud-auth/",body=list(query="CREATE Graph <DEBILE_PROFOND_1>"),encode="form",
                  config=list(authenticate(user="dba", password="dba", type = "digest")),accept("text/csv"),timeout(100000))
test_insert<-POST("http://localhost:8890/sparql-graph-crud-auth/",body=list(query="CREATE Graph <DEBILE_PROFOND_1>"),encode="form",
                  authenticate(user="dba", password="dba", type = "digest"),accept("text/csv"),timeout(100000))

test_insert<-POST("http://localhost:8890/sparql-auth/",body=list(query=hgQuery),encode="form",
                  authenticate(user="dba", password="dba", type = "digest"),accept("text/csv"),timeout(100000))
test_insert<-POST("http://localhost:8890/sparql-auth/",body=list(query="DROP Graph <DEBILE_PROFOND_1>"),encode="form",
                  authenticate(user="dba", password="dba", type = "digest"),accept("text/csv"),timeout(100000))
content(test_insert)


sparql_url<-"http://localhost:8890/sparql/"
sparql_auth<-"http://localhost:8890/sparql-auth/"
connection_without_auth<-create_connection(sparqlEndpoint_url =sparql_url) # OK
connection_without_auth<-create_connection(sparqlEndpoint_url=sparql_auth) # 401 in CHECK CONNECTION
connection_with_partialauth_data<-create_connection(sparqlEndpoint_url=sparql_auth,user = "dba") ## 401 in CHECK CONNECTION
connection_with_partialauth_data<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba")# OK
connection_with_partialauth_data<-create_connection(sparqlEndpoint_url=sparql_auth,pwd = "dba") ## 401 in CHECK CONNECTION
connection_with_partialauth_data<-create_connection(sparqlEndpoint_url=sparql_url,pwd = "dba")# OK
connection_with_auth_data<-create_connection(sparqlEndpoint_url=sparql_auth,user = "dba",pwd = "dba", authentication_protocol = "digest") # OK
connection_with_auth_data<-create_connection(sparqlEndpoint_url=sparql_auth,user = "dba",pwd = "dba") # OK

connection_with_auth_data<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba",pwd = "dba", authentication_protocol = "digest")
connection_with_auth_data<-create_connection(sparqlEndpoint_url=sparql_url,user = "dba",pwd = "dba")

