#!/usr/local/bin/pl -s

:- use_module(onto_spec).
:- use_module(library('semweb/rdf_db')).

all('musicontology.rdfs').
core_rdfs('musicontology-core.rdfs').
%external_rdfs('musicontology-external.rdfs').
level1_rdfs('musicontology-level1.rdfs').
level2_rdfs('musicontology-level2.rdfs').
level3_rdfs('musicontology-level3.rdfs').


output('musicontology.html').

spec(File,Glance,Classes,Props,Inds,Deprecs) :-
	rdf_db:rdf_load(File),
	glance_html_desc(Glance),
        classes_html_desc(Classes),
        props_html_desc(Props),
        inds_html_desc(Inds),
        deprecs_html_desc(Deprecs),
        rdf_db:rdf_retractall(_,_,_).

core(Glance,Classes,Props,Inds,Deprecs) :-
        core_rdfs(Core),
	spec(Core,Glance,Classes,Props,Inds,Deprecs).

%external(Glance,Classes,Props,Inds,Deprecs) :-
%        external_rdfs(External),
%	spec(External,Glance,Classes,Props,Inds,Deprecs).

%level1(Glance,Classes,Props,Inds,Deprecs) :-
%        level1_rdfs(Level1),
%	spec(Level1,Glance,Classes,Props,Inds,Deprecs).

%level2(Glance,Classes,Props,Inds,Deprecs) :-
%	level2_rdfs(Level2),
%	spec(Level2,Glance,Classes,Props,Inds,Deprecs).

%level3(Glance,Classes,Props,Inds,Deprecs) :-
%	level3_rdfs(Level3),
%	spec(Level3,Glance,Classes,Props,Inds,Deprecs).

all(Glance,Classes,Props,Inds,Deprecs) :-
	all(All),
	spec(All,Glance,Classes,Props,Inds,Deprecs).


:- 	
	open('1-header.htm',read,Header),
	open('2-overview.htm',read,Overview),
	open('3-introduction.htm',read,Introduction),
	open('4-glance-core.htm',read,GlanceCore),
	%open('5-glance-external.htm',read,GlanceExternal),
	%open('6-glance-level1.htm',read,GlanceLevel1),
	%open('7-glance-level2.htm',read,GlanceLevel2),
	%open('8-glance-level3.htm',read,GlanceLevel3),
	open('9-rdf-proplist.htm',read,PropList),
	open('10-large-intro.htm',read,LargeIntro),
	open('11-standards.htm',read,Standards),
	open('12-full-detail.htm',read,FullDetail),
	open('13-references.htm',read,References),
	open('14-changelog+footer.htm',read,ChangeLog),
	core(CoreGlanceD,_,_,_,_),
	%external(ExternalGlanceD,_,_,_,_),
	%level1(Level1GlanceD,_,_,_,_),
	%level2(Level2GlanceD,_,_,_,_),
	%level3(Level3GlanceD,_,_,_,_),
	all(_,Classes,Props,Inds,Deprecs),
	output(Output),
	open(Output,write,Otp),
	copy_stream_data(Header,Otp),
	copy_stream_data(Overview,Otp),
	copy_stream_data(Introduction,Otp),
	copy_stream_data(GlanceCore,Otp),
	write(Otp,CoreGlanceD),
	%copy_stream_data(GlanceExternal,Otp),
	%write(Otp,ExternalGlanceD),
	%copy_stream_data(GlanceLevel1,Otp),
	%write(Otp,Level1GlanceD),
	%copy_stream_data(GlanceLevel2,Otp),
	%write(Otp,Level2GlanceD),
	%copy_stream_data(GlanceLevel3,Otp),
	%write(Otp,Level3GlanceD),
	copy_stream_data(PropList,Otp),
	copy_stream_data(LargeIntro,Otp),
	copy_stream_data(Standards,Otp),
	copy_stream_data(FullDetail,Otp),
	write(Otp,Classes),
	write(Otp,Props),
	write(Otp,Inds),
	write(Otp,Deprecs),
	copy_stream_data(References,Otp),
	copy_stream_data(ChangeLog,Otp),
	close(Otp),
	halt.

