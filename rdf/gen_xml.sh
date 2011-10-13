#! /bin/bash

# Generate the RDF/XML from the Turtle code

rapper -I "http://purl.org/ontology/mo/" -i turtle -o rdfxml-abbrev musicontology.n3 > musicontology.rdfs


