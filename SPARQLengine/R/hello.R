query<-"PREFIX xml:<http://www.w3.org/XML/1998/namespace>
PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl:<http://www.w3.org/2002/07/owl#>

PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
PREFIX btl2:<http://purl.org/biotop/btl2.owl#>


PREFIX cepi:<http://www.chu-lyon.fr/epitrack/epitrack-core#>
SELECT ?uri ?label ?ATC_CODE
FROM <http://www.epitrack/core-ontology/>
WHERE {
?uri rdfs:subClassOf cepi:CEPI0000000021.
?uri rdfs:label ?label.
?uri cepi:ATC_code ?ATC_CODE .
}"

source("R/sparql_function_jpr.R")
source("R/query_builder_object.R")
#### TRIPLESTORE-DATA ####
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



query_builder<-new(Class = "SPARQL_query_builder", prefix=rbind(prefix_list[["generalPrefix"]],prefix_list[["epitrackPrefix"]][1:2,]), graph=list(graph_list[["cepi"]]),method='SELECT')
getGraph(query_builder)
query_builder<-addConstraint(query_builder,subj="?s",obj="cepi:CEPI0000000021", pred="rdfs:subClassOf")
query_builder<-addConstraint(query_builder,subj="?s",obj="?ATC_CODE", pred="rdfs:label")

query_builder<-addConstraint(query_builder,subj="?s",obj="?label", pred="cepi:ATC_code")

queryString<-build(query_builder)

sparqlGet(sparql_engine, queryString, type = "csv")

sparqlGet(sparql_engine, sparql = queryString)
