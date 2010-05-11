:- module(onto_spec,[
		op(200,fx,gen)
	,	gen/1
	,	onto_spec/1
	,	onto_spec_to_file/1
	,	header/1
	,	glance_html_desc/1
	,	classes_html_desc/1
	,	props_html_desc/1
	,	inds_html_desc/1
	,	deprecs_html_desc/1
	]).


/**
 * This SWI-Prolog module allows to generate a
 * HTML specification from the local RDF knowledge
 * base.
 *
 * The output is similar to the specgen script used in
 * the SIOC project.
 *
 * It handles rdfs:Class, rdf:Property, individuals, domain, range, domain-of, range-of,
 * and also unionOf for domains and ranges
 *
 * Copyright Yves Raimond (c) 2007, Centre for Digital Music, Queen Mary, University of London
 */

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- consult(library('semweb/rdf_http_plugin')).
:- consult(library('semweb/rdf_turtle')).

:- op(200,fx,gen).

:- rdf_db:rdf_register_ns(bibo,'http://purl.org/ontology/bibo/').
:- rdf_db:rdf_register_ns(dc,'http://purl.org/dc/elements/1.1/').
:- rdf_db:rdf_register_ns(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_db:rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_db:rdf_register_ns(frbr,'http://purl.org/vocab/frbr/core#').
:- rdf_db:rdf_register_ns(dcterms,'http://purl.org/dc/terms/').
:- rdf_db:rdf_register_ns(event,'http://purl.org/NET/c4dm/event.owl#').
:- rdf_db:rdf_register_ns(timeline,'http://purl.org/NET/c4dm/timeline.owl#').
:- rdf_db:rdf_register_ns(time,'http://www.w3.org/2006/time#').
:- rdf_db:rdf_register_ns(vs,'http://www.w3.org/2003/06/sw-vocab-status/ns#').
:- rdf_db:rdf_register_ns(geo,'http://www.w3.org/2003/01/geo/wgs84_pos#').
:- rdf_db:rdf_register_ns(af,'http://purl.org/ontology/af/').
:- rdf_db:rdf_register_ns(chord,'http://purl.org/ontology/chord/').

/**
 * Top-level predicate:
 * generate a HTML specification of the in-store schema
 *
 * onto_spec(-Spec)
 */
onto_spec(Spec) :-
	header(Header),
	tail(Tail),
	glance_html_desc(Glance),
	classes_html_desc(Classes),
	props_html_desc(Properties),
	inds_html_desc(Individuals),
	deprecs_html_desc(Deprecated),
	format(atom(Spec),'~w~w\n<h3>Classes and Properties (full detail)</h3>\n~w~w~w~w~w',
		[Header,Glance,Classes,Properties,Individuals,Deprecated,Tail]).

classes_html_desc(Classes) :-
	((setof(NC-Desc,NS^(class_html_desc(NS:NC,Desc)),DescsMess),!);DescsMess=[]),
	keysort(DescsMess,Descs),
	reverse(Descs,DescsR),
	list_to_atom(DescsR,Classes).

props_html_desc(Properties) :-
	((setof(NP-Descp,NS^prop_html_desc(NS:NP,Descp),DescspMess),!);DescspMess=[]),
	keysort(DescspMess,Descsp),
	reverse(Descsp,DescspR),
	list_to_atom(DescspR,Properties).

inds_html_desc(Individuals) :-
	((setof(NI-Desci,NS^(ind_html_desc(NS:NI,Desci)),DescsiMess),!);DescsiMess=[]),
	keysort(DescsiMess,Descsi),
	reverse(Descsi,DescsiR),
	list_to_atom(DescsiR,Individuals).

deprecs_html_desc(Deprecated) :-
	((setof(Dep-Descd,NS^(deprecated_html_desc(NS:Dep,Descd)),DescsdMess),!);DescsdMess=[]),
	keysort(DescsdMess,Descsd),
	reverse(Descsd,DescsdR),
	list_to_atom(DescsdR,Deprecated).


header(H) :- author_name(Name), author_foaf(FOAF), page_title(Title),
			 sformat(H,
				'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">\n<head>\n<link rel="meta" type="application/rdf+xml" title="FOAF" href="~w" />\n <meta http-equiv="content-type" content="text/html; charset=iso-8859-1" />\n<meta name="author" content="~w" />\n<link href="style.css" rel="stylesheet" type="text/css" /><meta name="robots" content="all" />\n<title>~w</title>\n</head>\n<body>',
					[FOAF,Name,Title]).

tail('</body></html>').

/**
 * Outputs the specification to a file
 */
gen File :- onto_spec_to_file(File).
onto_spec_to_file(File) :-
	open(File,write,Stream,[]),
	onto_spec(Spec),
	write(Stream,Spec),
	close(Stream).

/**
 * Wrappers for some simple rdf queries - plug your ontology language here:-)
 */
class_comment(NS:C,Comment) :-
	class(Class),
	rdf(Class,rdfs:comment,literal(Comment1)),newline_to_br(Comment1,Comment),
	rdf_global_id(NS:C,Class).
class_status(NS:C,Status) :-
	class(Class),
	rdf(Class,vs:term_status,literal(Status)),
	rdf_global_id(NS:C,Class).
property_comment(NS:P,Comment) :-
	property(Property),
	rdf(Property,rdfs:comment,literal(Comment1)),newline_to_br(Comment1,Comment),
	rdf_global_id(NS:P,Property).
property_status(NS:P,Status) :-
	property(Property),
	rdf(Property,vs:term_status,literal(Status)),
	rdf_global_id(NS:P,Property).
property_domain(NS:P,Domain) :-
	rdf(Property,rdfs:domain,Domain),
	\+bnode(Domain),
	rdf_global_id(NS:P,Property).
property_domain(NS:P,Domain) :-
	rdf(Property,rdfs:domain,UnionOf),
	rdf(UnionOf,rdf:type,owl:'Class'),
	rdf(UnionOf,owl:unionOf,DomainList),
	rdf_list_member(Domain,DomainList),
	rdf_global_id(NS:P,Property).
property_range(NS:P,Range) :-
        rdf(Property,rdfs:range,Range),
	\+bnode(Range),
        rdf_global_id(NS:P,Property).
property_range(NS:P,Range) :-
        rdf(Property,rdfs:range,UnionOf),
        rdf(UnionOf,rdf:type,owl:'Class'),
        rdf(UnionOf,owl:unionOf,RangeList),
	rdf_list_member(Range,RangeList),
        rdf_global_id(NS:P,Property).
property_inverse(NS:P,Inverse) :-
	rdf(Property,owl:inverseOf,Inverse),
	rdf_global_id(NS:P,Property).
property_subprop_of(NS:P,Super) :-
	rdf(Property,rdfs:subPropertyOf,Super),
	rdf_global_id(NS:P,Property).

/* dc:title */
individual_title(NS:C,Title) :-
		individual(I),
		rdf(I,dc:title,literal(Title)),
		rdf_global_id(NS:C,I) .
/* rdfs:label */		
individual_title(NS:C,Title) :-
		\+((individual(I),
		rdf(I,dc:title,literal(Title)),
		rdf_global_id(NS:C,I))),
		
		individual(I),
		rdf(I,rdfs:label,literal(Title)),
		rdf_global_id(NS:C,I) .
/* default : blank */
individual_title(NS:C,Title) :-
		\+((individual(I),
		rdf(I,dc:title,literal(Title)),
		rdf_global_id(NS:C,I))),
		\+((individual(I),
		rdf(I,rdfs:label,literal(Title)),
		rdf_global_id(NS:C,I))),
		Title=''.

/* dc:description */
individual_description(NS:C,Desc) :-
		individual(I),
       	rdf(I,dc:description,literal(Desc)),
        rdf_global_id(NS:C,I) .
/* rdfs:comment */
individual_description(NS:C,Desc) :-
		\+((individual(I),
       	rdf(I,dc:description,literal(Desc)),
        rdf_global_id(NS:C,I))),

		individual(I),
        rdf(I,rdfs:comment,literal(Desc)),
        rdf_global_id(NS:C,I) .
/* default : blank */
individual_description(NS:C,Desc) :-
		\+((individual(I),
       	rdf(I,dc:description,literal(Desc)),
        rdf_global_id(NS:C,I))),
		\+((individual(I),
        rdf(I,rdfs:comment,literal(Desc)),
        rdf_global_id(NS:C,I))),
		Desc = ''.

individual_class(NS:I,NS2:C) :-
	individual(In),
	class(Cl),
	rdf(In,rdf:type,Cl),
	rdf_global_id(NS:I,In),rdf_global_id(NS2:C,Cl).
subclassof(C,C2) :-
	rdf(C,rdfs:subClassOf,C2).
class(Class) :-
	rdf(Class,rdf:type,owl:'Class').
term_level(NS:T,Level) :-
	(rdf(Term,mo:level,literal(Level1)),rdf_global_id(NS:T,Term),format(atom(Level),'level ~w',[Level1]),!);Level=''.
%class(Class) :-
%	rdf(Class,rdf:type,owl:'Class').
individual(I) :-
	rdf(I,rdf:type,C),class(C).
property(Property) :-
	rdf(Property,rdf:type,rdf:'Property').
property(Property) :-
	rdf(Property,rdf:type,owl:'ObjectProperty').
property(Property) :-
	rdf(Property,rdf:type,owl:'DatatypeProperty').
rdf_list_member(Member,List) :-
	rdf(List,rdf:first,Member).
rdf_list_member(Member,List) :-
	rdf(List,rdf:rest,List2),
	rdf_list_member(Member,List2).
deprecated(Old,New) :-
	rdf(Old,vs:term_status,literal('deprecated')),
	(rdf(Old,owl:sameAs,New) ; rdf(Old,owl:equivalentProperty,New)).


/**
 * Spec At A Glance - FOAF/SIOC style
 */
glance_html_desc(Desc) :-
	((setof(C-CD,
	Class^S1^NS^(
		class(Class),rdf_global_id(NS:C,Class),\+deprecated(Class,D), NS = 'mo', 
		sformat(S1,
			'<a href="#term_~w">~w</a> | ',
			[C,C]
			),
		string_to_atom(S1,CD)
	),
	ClassesL),!);ClassesL=[]),
	keysort(ClassesL,ClassesLSorted),reverse(ClassesLSorted,ClassesLSR),
	list_to_atom(ClassesLSR,Classes),
	((setof(P-PD,
        Property^S2^NS^(
                property(Property),rdf_global_id(NS:P,Property),\+deprecated(Property,D), NS = 'mo',
                sformat(S2,
                        '<a href="#term_~w">~w</a> | \n',
                        [P,P]
                        ),
                string_to_atom(S2,PD)
        ),
        PropertiesL),!);PropertiesL=[]),
	keysort(PropertiesL,PropertiesLS),reverse(PropertiesLS,PropertiesLSR),
        list_to_atom(PropertiesLSR,Properties),
	((setof(I-ID,
	Individual^S3^NS^(
		individual(Individual),rdf_global_id(NS:I,Individual),\+deprecated(Individual,D),
		sformat(S3,
			'<a href="#term_~w">~w</a> | \n',
			[I,I]
			),
		string_to_atom(S3,ID)
	),
	IndividualsL),!);IndividualsL=[]),
	keysort(IndividualsL,IndividualsLS),reverse(IndividualsLS,IndividualsLSR),
	list_to_atom(IndividualsLSR,Individuals),
	sformat(String,
		'<div class="glance" id="glance">\n<p>Classes: | ~w </p>\n<p>Properties: | ~w</p>\n<p>Individuals: | ~w </p></div>',
		[Classes,Properties,Individuals]
		),
	string_to_atom(String,Desc).


/**
 * HTML description of classes - FOAF/SIOC style
 */
class_html_desc(NS:C,HtmlDesc) :- %domain-of AND range-of available
	class_comment(NS:C,Comment),
	class_status(NS:C,Status),term_level(NS:C,Level),
	sformat(StringHtml,
		'<div class="specterm" id="term_~w"><h3>Class: ~w   -   ~w   -   ~w</h3>\n<em>~w</em>\n - ~w \n<br/>\n<p style="float: right; font-size: small;">[<a href="#glance">back to top</a>]</p>\n<br/>',
		[C,NS:C,Status,Level,C,Comment]
	),
	string_to_atom(StringHtml,HtmlDesc1),
	range_of_desc(NS:C,RangeOf),
	domain_of_desc(NS:C,DomainOf),
	subclass_of_desc(NS:C,SubClassOf),
	sformat(StringAtt,
		'\n<table style="th { float: top; }">~w\n~w\n~w</table>\n',
		[RangeOf,DomainOf,SubClassOf]
	),
	string_to_atom(StringAtt,Att),
	atom_concat(Att,'\n</div>',Att2),
	atom_concat(HtmlDesc1,Att2,HtmlDesc).

range_of_desc(NS:C,'') :-
        range_of_html_desc(NS:C,''),!.
range_of_desc(NS:C,Desc) :-
	range_of_html_desc(NS:C,RangeOf),
	sformat(ROF,
                '<tr><th>in-range-of:</th>~w</tr>\n',
                [RangeOf]
        ),
        string_to_atom(ROF,Desc).

domain_of_desc(NS:C,'') :-
        domain_of_html_desc(NS:C,''),!.
domain_of_desc(NS:C,Desc) :-
	domain_of_html_desc(NS:C,DomainOf),
	sformat(DOF,
                '<tr><th>in-domain-of:</th>~w</tr>\n',
                [DomainOf]
        ),
	string_to_atom(DOF,Desc).

subclass_of_desc(NS:C,'') :-
        subclass_of_html_desc(NS:C,''),!.
subclass_of_desc(NS:C,Desc) :-
        subclass_of_html_desc(NS:C,SubClassOf),
        sformat(SOF,
                '<tr><th>sub-class-of:</th>~w</tr>\n',
                [SubClassOf]
        ),
        string_to_atom(SOF,Desc).

/**
 * HTML description of properties - FOAF/SIOC style
 */
prop_html_desc(NS:P,HtmlDesc) :-
	property_comment(NS:P,Comment),
	property_status(NS:P,Status),term_level(NS:P,Level),
	sformat(StringHtmlComment,
		'<div class="specterm" id="term_~w">\n<h3>Property: ~w   -   ~w   -   ~w</h3>\n<em>~w</em> - ~w \n<br/>',
		[P,NS:P,Status,Level,P,Comment]
	),
	string_to_atom(StringHtmlComment,HtmlComment),
	domain_html_desc(NS:P,DomainHtmlDesc),
	range_html_desc(NS:P,RangeHtmlDesc),
	inverse_html_desc(NS:P,InverseHtmlDesc),
	subprop_html_desc(NS:P,SubPropHtmlDesc),
	sformat(StringHtmlDomainRange,
		'<table style="th { float: top; }">\n<tr><th>Domain:</th>\n~w\n</tr>\n<tr><th>Range:</th>\n~w</tr>\n~w\n~w\n</table>\n<br/></div>',
		[DomainHtmlDesc,RangeHtmlDesc,InverseHtmlDesc,SubPropHtmlDesc]
		),
	string_to_atom(StringHtmlDomainRange,HtmlDomainRange),
	atom_concat(HtmlComment,HtmlDomainRange,HtmlDesc).

/**
 * HTML description of individuals
 */
ind_html_desc(NS:I,HtmlDesc) :-
	individual_title(NS:I,Title),
	individual_description(NS:I,Description),term_level(NS:I,Level),
	individual_class(NS:I,NS2:C),
	sformat(S,
		'<div class="specterm" id="term_~w">\n<h3>Individual: ~w   -   ~w</h3>\n<em>~w</em> - ~w \n<br/><table style="th { float: top; }"><tr><th>Class:</th>\n<td><a href="#term_~w">~w</a></td>\n</tr></table></div>',
		[I,I,Level,Title,Description,C,NS2:C]
	),
	string_to_atom(S,HtmlDesc).

/**
 * HTML description of domains
 */
domain_html_desc(NS:P,DomainHtmlDesc) :-
	property(Prop),rdf_global_id(NS:P,Prop),
	findall(Html,(
		property_domain(NS:P,Domain),
		((class(Domain),rdf_global_id(_:D,Domain),
		sformat(S,
			'<td><a href="#term_~w">~w</a></td>\n',
			[D,D]),
		string_to_atom(S,Html)
			);
		(\+class(Domain),rdf_global_id(NS2:D2,Domain),sformat(S,
                        '<td><a href="~w">~w</a></td>\n',
                        [Domain,NS2:D2])
		)),
                string_to_atom(S,Html)
			),Htmls),
	list_to_atom(Htmls,DomainHtmlDesc).

/**
 * HTML description of range
 */
range_html_desc(NS:P,RangeHtmlDesc) :-
        property(Prop),rdf_global_id(NS:P,Prop),
        findall(Html,(
                property_range(NS:P,Range),
                ((class(Range),rdf_global_id(_:D,Range),
                sformat(S,
                        '<td><a href="#term_~w">~w</a></td>\n',
                        [D,D]),
                string_to_atom(S,Html)
                        );
                (\+class(Range),rdf_global_id(NS2:D2,Range),sformat(S,
                        '<td><a href="~w">~w</a></td>\n',
                        [Range,NS2:D2])
                )),
                string_to_atom(S,Html)
                        ),Htmls),
        list_to_atom(Htmls,RangeHtmlDesc).

/**
 * HTML description of inverse-of
 */
inverse_html_desc(NS:P,InverseHtmlDesc) :-
	property(Prop),rdf_global_id(NS:P,Prop),
	findall(Html,(
		property_inverse(NS:P,Inverse),
		((class(Inverse),rdf_global_id(_:D,Inverse),
                sformat(S,
                        '<td><a href="#term_~w">~w</a></td>\n',
                        [D,D]),
                string_to_atom(S,Html)
                        );
                (\+class(Inverse),rdf_global_id(NS2:D2,Inverse),sformat(S,
                        '<td><a href="~w">~w</a></td>\n',
                        [Inverse,NS2:D2])
                )),
                string_to_atom(S,Html)
                        ),Htmls),
        list_to_atom(Htmls,InverseHtmlDesc2),
	(InverseHtmlDesc2=''->InverseHtmlDesc='';(sformat(Temp,'<tr><th>Inverse-of: </th>~w</tr>',[InverseHtmlDesc2]),string_to_atom(Temp,InverseHtmlDesc))).

/**
 * HTML description of sub-property-of
 */
subprop_html_desc(NS:P,SubPropHtmlDesc) :-
	property(Prop),rdf_global_id(NS:P,Prop),
	findall(Html,(
		property_subprop_of(NS:P,Super),
		((property(Super),rdf_global_id(_:D,Super),
		sformat(S,
			'<td><a href="#term_~w">~w</a></td>\n',
			[D,D]),
		string_to_atom(S,Html)
			);
		(\+property(Super),rdf_global_id(NS2:D2,Super),sformat(S,
			'<td><a href="~w">~w</a></td>\n',
			[Super,NS2:D2])
		)),
		string_to_atom(S,Html)
			),Htmls),
		list_to_atom(Htmls,SubPropHtmlDesc2),
		(SubPropHtmlDesc2=''->SubPropHtmlDesc='';(sformat(Temp,'<tr><th>Sub-property-of: </th>~w</tr>',[SubPropHtmlDesc2]),string_to_atom(Temp,SubPropHtmlDesc))).


/**
 * HTML description of "domain of"
 */
domain_of_html_desc(NS:C,DomainOf) :-
	class(Class),rdf_global_id(NS:C,Class),
	findall(Html,(
		property_domain(NS2:P,Class),rdf_global_id(NS2:P,_Property),
                sformat(S,
                        '<td><a href="#term_~w">~w</a></td>\n',
                        [P,P]),
                string_to_atom(S,Html)
		),Htmls),
	list_to_atom(Htmls,DomainOf).

/**
 * HTML description of "range of"
 */
range_of_html_desc(NS:C,RangeOf) :-
        class(Class),rdf_global_id(NS:C,Class),
        findall(Html,(
                property_range(NS2:P,Class),rdf_global_id(NS2:P,_Property),
                sformat(S,
                        '<td><a href="#term_~w">~w</a></td>\n',
                        [P,P]),
                string_to_atom(S,Html)
                ),Htmls),
        list_to_atom(Htmls,RangeOf).

/**
 * HTML description of "subclass of"
 */
subclass_of_html_desc(NS:C,SubClassDesc) :-
        class(Class),rdf_global_id(NS:C,Class),
        findall(Html,(
                subclassof(Class,Class2),
                ((class(Class2),rdf_global_id(_:D,Class2),
                sformat(S,
                        '<td><a href="#term_~w">~w</a></td>\n',
                        [D,D]),
                string_to_atom(S,Html)
                        );
                (\+class(Class2),rdf_global_id(NS2:D2,Class2),sformat(S,
                        '<td><a href="~w">~w</a></td>\n',
                        [Class2,NS2:D2])
                )),
                string_to_atom(S,Html)
                        ),Htmls),
        list_to_atom(Htmls,SubClassDesc).


/**
 * HTML description of deprecated concepts/properties
 */
deprecated_html_desc(NS:T,DepDesc) :-
	deprecated(Term,NewTerm),
	rdf_global_id(NS:T,Term),
	rdf_global_id(NS:NT,NewTerm),
	format(atom(DepDesc),
		'<div class="specterm" id="term_~w">\n<h3>Deprecated: ~w</h3>\n<em>Equivalent to</em> - <a href="#term_~w">~w</a> \n<br/></div>'
		,[T,NS:T,NT,NS:NT]).

list_to_atom([],'').
list_to_atom([_-A],A):-!.
list_to_atom([A],A):-!.
list_to_atom([_-H|T],At) :-
	!,
	list_to_atom(T,Temp),
	atom_concat(Temp,H,At).
list_to_atom([H|T],At) :-
	list_to_atom(T,Temp),
	atom_concat(Temp,H,At).
	

bnode(B) :- atom_concat('__',_,B).

newline_to_br(Literal,LiteralBR) :-
	atom_chars(Literal,L1),
	newline_to_br_l(L1,L2),
	atom_chars(LiteralBR,L2).
newline_to_br_l([],[]).
newline_to_br_l(['\n'|T1],['<','b','r','/','>','\n'|T2]) :-
	!,newline_to_br_l(T1,T2).
newline_to_br_l([H|T1],[H|T2]) :-
	newline_to_br_l(T1,T2).

:- 
	nl,
	writeln(' Usage:'),
	writeln(' - Load a RDF file using rdf_load/1'),
	writeln(' - Specify author_name/1, author_foaf/1 and page_title/1'),
	writeln(' - Generate the spec using ''gen ''spec.html'''''),
	nl,nl.

