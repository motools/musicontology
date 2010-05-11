:- module(onto_splitter,[split/0]).

/**
 * Just a util to split the ontology between terms defined by the document
 * and terms defined outside it
 *
 * Yves Raimond, C4DM, QMUL, 2007
 */

:- use_module(library('semweb/rdf_db')).
:- use_module(library('rdf')).


/**
 * Adjust these to your needs
 */
document('musicontology.rdfs').
primary_ns('http://purl.org/ontology/mo/').
output_core('musicontology-core.rdfs').
output_external('musicontology-external.rdfs').
output_level1('musicontology-level1.rdfs').
output_level2('musicontology-level2.rdfs').
output_level3('musicontology-level3.rdfs').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').

/**
 * Top-level goal
 */
split :- 
	load,
	core,
	%external,
	level1,
	level2,
	level3.

/**
 * Load the document
 */
load :- 
	document(Doc),
	rdf_db:rdf_load(Doc).

/**
 * Extract and export the core (explores up to three bnodes)
 */
core :- 
	primary_ns(NS),
	findall(rdf(S,P,O),
		(rdf_db:rdf(A,rdfs:isDefinedBy,NS),description(A,rdf(S,P,O))),
		TriplesCore),
	output_core(Core),
	open(Core,write,CoreStream),
	rdf_write_xml(CoreStream,TriplesCore),
	close(CoreStream).

/**
 * Extract and export the externals
 */
external :-
	primary_ns(NS),
	findall(rdf(S,P,O),
		(rdf_db:rdf(A,B,C),\+(C=mo:_),description(C,rdf(S,P,O))),
		TriplesExternal),
	output_external(External),
	open(External,write,ExternalStream),
	rdf_write_xml(ExternalStream,TriplesExternal),
	close(ExternalStream).

/**
 * Extract and export level 1 terms
 */
level1 :- 
	findall(rdf(S,P,O),
		(rdf_db:rdf(A,mo:level,literal('1')),description(A,rdf(S,P,O))),
		Triples1),
	output_level1(Level1),
	open(Level1,write,Level1Stream),
	rdf_write_xml(Level1Stream,Triples1),
	close(Level1Stream).

/**
 * Extract and export level 2 terms
 */
level2 :-
        findall(rdf(S,P,O),
                (rdf_db:rdf(A,mo:level,literal('2')),description(A,rdf(S,P,O))),
                Triples2),
        output_level2(Level2),
        open(Level2,write,Level2Stream),
        rdf_write_xml(Level2Stream,Triples2),
        close(Level2Stream).
/**
 * Extract and export level 3 terms
 */
level3 :-
        findall(rdf(S,P,O),
                (rdf_db:rdf(A,mo:level,literal('3')),description(A,rdf(S,P,O))),
                Triples3),
        output_level3(Level3),
        open(Level3,write,Level3Stream),
        rdf_write_xml(Level3Stream,Triples3),
        close(Level3Stream).



/**
 * All the triples describing a given ontology term
 * This predicates include blank-nodes closure
 */
description(A,rdf(A,P,O)) :-
	rdf_db:rdf(A,P,O).
description(A,rdf(S,P,O)) :-
	rdf_db:rdf(A,_,BN),
	rdf_db:rdf_is_bnode(BN),
	description(BN,rdf(S,P,O)).


