
library(SPARQL)

m1 = load.rdf("Data/OntoPTv0.6.rdfs")

query <-
  "PREFIX  dgp1187: <http://data-gov.tw.rpi.edu/vocab/p/1187/>
SELECT ?ye ?fi ?ac
WHERE {
?s dgp1187:year ?ye .
?s dgp1187:fires ?fi .
?s dgp1187:acres ?ac .
}"

a <- m1$listObjectProperties()

sparql.rdf(m3, "SELECT ?s ?p { ?s ?p ?o }")