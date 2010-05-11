#!/bin/bash

if !(test -e musicontology.rdfs) ; then ln -s ../rdf/musicontology.rdfs . ; fi
if !(test -e onto_splitter.pl) ; then ln -s ../ontosplit/onto_splitter.pl . ; fi
if !(test -e onto_spec.pl) ; then ln -s ../ontospec/onto_spec.pl . ; fi
if !(test -e split.sh) ; then ln -s ../ontosplit/split.sh . ; fi
if !(test -e spec.sh) ; then ln -s ../ontospec/spec.sh . ; fi

if (test -e musicontology-core.rdfs) ; then rm musicontology-core.rdfs ; fi
if (test -e musicontology-external.rdfs) ; then rm musicontology-external.rdfs ; fi

./split.sh
./spec.sh

