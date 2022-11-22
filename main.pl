:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(files)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(ordsets)).
:- use_module(library(time)).

:- use_module('../teruel/teruel').
:- use_module('../marquete/marquete').

run(ConfigFile) :-
    portray_clause(doclog(1, 0, 0)),    
    atom_chars(ConfigFileA, ConfigFile),
    consult(ConfigFileA),
    generate_nav(Nav),
    generate_footer(Footer),
    Sections = ["nav"-Nav, "footer"-Footer],
    generate_page_docs(Sections),
    generate_readme(Sections),
    halt.

generate_nav(NavHtml) :-
    source_folder(SF),
    path_segments(SF, SFSG),
    subnav(SFSG, ".", Nav),
    member("nav"-NavHtml, Nav).

subnav(Base, Dir, ["name"-Dir, "nav"-Nav]) :-
    append(Base, [Dir], DirSg),
    path_segments(RealDir, DirSg),
    directory_files(RealDir, Files),
    dirs_only(Files, DirSg, DirsOnly),
    list_to_ord_set(Files, FilesSet),
    list_to_ord_set(DirsOnly, DirsOnlySet),
    ord_subtract(FilesSet, DirsOnlySet, FilesOnly),
    maplist(subnav(DirSg), DirsOnly, NavDirs),
    maplist(file_link(RealDir), FilesOnly, FilesLink),
    render("nav.html", ["files"-FilesLink, "dirs"-NavDirs], Nav).

file_link(Dir, File, ["name"-File, "link"-Link]) :-
    source_folder(SF),
    append(SF, Extra, Dir),
    append(Extra, ['/'|File], Link).

generate_footer(Footer) :-
    current_time(T),
    phrase(format_time("%b %d %Y", T), Time),
    render("footer.html", ["time"-Time], Footer).

generate_readme(Sections) :-
    readme_file(ReadmeFile),
    project_name(ProjectName),
    output_folder(OutputFolder),
    path_segments(OutputFolder, OutputFolderSg),
    append(OutputFolderSg, ["index.html"], OutputFileSg),
    path_segments(OutputFile, OutputFileSg),
    phrase_from_file(seq(ReadmeMd), ReadmeFile),
    markdown(ReadmeMd, ReadmeHtml),
    Vars0 = ["project_name"-ProjectName, "readme"-ReadmeHtml],
    append(Vars0, Sections, Vars),
    render("index.html", Vars, IndexHtml),
    phrase_to_file(seq(IndexHtml), OutputFile).

generate_page_docs(Sections) :-
    source_folder(DocsFolder),
    output_folder(OutputFolder),    
    make_directory(OutputFolder),
    directory_files(DocsFolder, Files),
    path_segments(DocsFolder, Base),
    path_segments(OutputFolder, Output),
    maplist(process_file(Base, Output, Sections), Files),
    copy_css(Output).

process_file(Base, Output0, Sections, File0) :-
    append(Base, [File0], FileSg),
    append(File0, ".html", Output1),
    append(Output0, [Output1], OutputSg),
    path_segments(Output, OutputSg),
    path_segments(File, FileSg),
    file_exists(File),
    portray_clause(process_file(File)),
    open(File, read, FileStream),
    read_term(FileStream, Term, []),
    (
	Term = (:- module(ModuleName, PublicPredicates)) ->
	document_file(File, Output, ModuleName, PublicPredicates, Sections)
    ;   true
    ),
    close(FileStream).
    

process_file(Base0, Output0, Sections, Dir0) :-
    append(Base0, [Dir0], DirSg),
    append(Output0, [Dir0], Output),
    path_segments(Dir, DirSg),
    directory_exists(Dir),
    path_segments(OutputDir, Output),
    make_directory(OutputDir),
    directory_files(Dir, Files),
    maplist(process_file(DirSg, Output, Sections), Files).

document_file(InputFile, OutputFile, ModuleName, PublicPredicates, Sections) :-
    maplist(document_predicate(InputFile), PublicPredicates, Predicates),
    phrase_from_file(module_description(ModuleDescriptionMd), InputFile),
    markdown(ModuleDescriptionMd, ModuleDescriptionHtml),
    atom_chars(ModuleName, ModuleNameStr),
    project_name(ProjectName),
    Vars0 = [
	"project_name"-ProjectName,
	"module_name"-ModuleNameStr,
	"module_description"-ModuleDescriptionHtml,
	"predicates"-Predicates
    ],
    append(Vars0, Sections, Vars),
    render("page.html", Vars, HtmlOut),
    phrase_to_file(seq(HtmlOut), OutputFile).

module_description(X) -->
    ... ,
    "/**",
    seq(X),
    "*/",
    ... .

module_description("No description") -->
    ... .


% First try to extract description based on name and arity, if not, fallback to extremely simple description
document_predicate(InputFile, Predicate, ["name"-Name, "description"-Description]) :-
    portray_clause(documenting(Predicate)),
    (
	(
	    phrase_from_file(predicate_documentation(Predicate, Name, DescriptionMd), InputFile),
	    markdown(DescriptionMd, Description)
	)
    ;	document_predicate(Predicate, ["name"-Name, "description"-Description])
    ).

document_predicate(PredicateName/PredicateArity, ["name"-Name, "description"-Description]) :-
    phrase(format_("~a/~d", [PredicateName, PredicateArity]), Name),
    Description = "".

document_predicate(PredicateName//PredicateArity, ["name"-Name, "description"-Description]) :-
    phrase(format_("~a/~d", [PredicateName, PredicateArity]), Name),    
    Description = "".

document_predicate(op(_,_,Operator), ["name"-Name, "description"-Description]) :-
    atom_chars(Operator, Name),
    Description = "".

predicate_documentation(Predicate, Name, Description) -->
    ... ,
    "%!  ",
    predicate_name(Predicate, Name),
    "\n%", whites, "\n",
    predicate_description(Description),
    ... .

predicate_name(PredicateName/0, Name) -->
    {
	atom_chars(PredicateName, NameCs)
    },
    NameCs,
    seq(RestCs),
    {
	append(NameCs, RestCs, Name)
    }.

predicate_name(PredicateName/Arity, Name) -->
    {
	atom_chars(PredicateName, NameCs)
    },
    NameCs,
    "(",
    seq(Args),
    ")",
    !,
    {
	Commas is Arity - 1,
	phrase(commas(Commas), Args)
    },
    seq(RestCs),
    {
	phrase((NameCs,"(",seq(Args),")",seq(RestCs)), Name)
    }.

predicate_description(Description) -->
    "%   ", seq(Line), "\n",
    predicate_description(Description0),
    {
	append(Line, ['\n'|Description0], Description)
    }.
predicate_description(Description) -->
    "%", whites, "\n",
    predicate_description(Description0),
    {
	Description = ['\n'|Description0]
    }.
predicate_description("") --> [].

whites --> [].
whites --> " ", whites.
whites --> "\t", whites.

commas(0) -->
    seq(X),
    {
	\+ member(',', X)
    }.
commas(N) -->
    seq(X),
    {
	\+ member(',', X)
    },
    ",",
    commas(N0),
    {
	N is N0 + 1
    }.

dirs_only([F|Fs], Output, [F|FOs]) :-
    append(Output, [F], OutputFile),
    path_segments(File, OutputFile),
    directory_exists(File),
    dirs_only(Fs, Output, FOs).

dirs_only([F|Fs], Output, FOs) :-
    append(Output, [F], OutputFile),
    path_segments(File, OutputFile),    
    \+ directory_exists(File),
    dirs_only(Fs, Output, FOs).

dirs_only([], _, []).

string_without([X|Xs], Block) -->
    [X],
    {
	X \= Block
    },
    string_without(Xs, Block).

string_without([], Block) -->
    [X],
    {
	X = Block
    }.

string_without([], _) -->
    [].

copy_css(Output) :-
    append(Output, ["doclog.css"], OutputFileSg),
    path_segments(OutputFile, OutputFileSg),
    setup_call_cleanup(open("doclog.css", read, Stream),(
	get_n_chars(Stream, _, Css),
        phrase_to_file(seq(Css), OutputFile)),
        close(Stream)).
