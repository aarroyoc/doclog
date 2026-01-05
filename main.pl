:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(files)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(ordsets)).
:- use_module(library(time)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(debug)).

:- use_module(teruel/teruel).
:- use_module(djota/djota).

:- dynamic(output_folder/1).
:- dynamic(source_folder/1).
:- dynamic(base_url/1).
:- dynamic(sitemap_url/1).

run(SourceFolder, OutputFolder) :-
    catch((
        portray_color(blue, doclog(2, 3, 1)),
	assertz(output_folder(OutputFolder)),
	assertz(source_folder(SourceFolder)),
	retractall(sitemap_url(_)),

	% Load config file
	path_segments(SourceFolder, S1),
	append(S1, ["doclog.config.pl"], C1),
	path_segments(ConfigFile, C1),
	atom_chars(ConfigFileA, ConfigFile),
	consult(ConfigFileA),



	% Ensure output directory exists 
	make_directory_path(OutputFolder),
	% Generate chrome
	generate_nav_lib(NavLib),
	generate_nav_learn(NavLearn),
	generate_footer(Footer),
	format("Generated navigation and footer.~n", []),
	% Build footer.html path using segments
	path_segments(OutputFolder, O1),
	append(O1, ["footer.html"], FooterOutSg),
	path_segments(FooterOut, FooterOutSg),
	phrase_to_file(seq(Footer), FooterOut),
	format("Generated footer file.~n", []),

	Sections = ["nav_lib"-NavLib, "nav_learn"-NavLearn,"footer"-Footer],

	% Build everything else
	generate_page_learn(Sections),
	do_copy_files,
	format("Generating Page Docs...~n", []),
	generate_page_docs(Sections),
	format("Generating Readme...~n", []),
	generate_readme(Sections),
	format("Generated Readme...~n", []),

	portray_color(green, done),
	halt
	),
	Error,
    (write(Error), nl, halt(1))).

docs_base_url(BaseURL) :-
    ( base_url(X) ->
      BaseURL = X
    ; BaseURL = "/"
    ).

do_copy_files :-
    source_folder(S1),
    output_folder(O1),
    path_segments(S1, S2),
    path_segments(O1, O2),
    findall(A-B, copy_file(A,B), Files),
    maplist(do_copy_files_(S2, O2), Files).

do_copy_files_(S2, O2, A1-B1) :-
    path_segments(A1, A2),
    path_segments(B1, B2),
    append(S2, A2, A3),
    append(O2, B2, B3),
    path_segments(A, A3),
    path_segments(B, B3),
    ( file_newer(A, B) ->
      portray_color(blue, skip_copy_file(A, B))
    ; portray_color(green, copy_file(A, B)),
      file_copy(A, B)
    ).

% We do some path detection which will fail unless all paths are equally
% separated. This relates a base path, a tail path and the segments of the
% union.
%
% canonicalize("C:\\mypath", "my/otherpath", ["C:", "mypath", "my", "otherpath"]).
canonicalize(Base, Path, ResSG) :-
    path_segments(Base, BaseSG),
    path_segments(Sep, ["", ""]),
    Sep = [Separator],
    replace_char('\\\\', Separator, Path, Path1),
    replace_char('/', Separator, Path1, Path2),
    path_segments(Path2, PathSG),
    append(BaseSG, PathSG, ResSG).

generate_nav_lib(NavHtml) :-
    source_folder(S1),
    source_lib_folder(S20),
    canonicalize(S1, S20, SFSG),
    subnav(SFSG, ".", Nav),
    member("nav"-NavHtml, Nav).

subnav(Base, Dir, ["name"-Dir, "nav"-Nav, "type"-"dir"]) :-
    append(Base, [Dir], DirSg),
    path_segments(RealDir, DirSg),
    directory_exists(RealDir),
    directory_files(RealDir, Files),
    files_not_omitted_files(RealDir, Files, FilesReal),
    sort(FilesReal, FilesSorted),
    maplist(subnav(DirSg), FilesSorted, Items),
    render("nav_tree.html", ["items"-Items], Nav).

subnav(Base, File, ["name"-Name, "link"-['/'|Link], "type"-"file"]) :-
    append(Base, [File], FileSg),
    append(Name, ".pl", File),
    path_segments(FilePath, FileSg),
    file_exists(FilePath),
    append(_, ["."|LinkSg], FileSg),
    \+ member(".", LinkSg),
    path_segments(Link0, LinkSg),
    append(Link1, ".pl", Link0),
    append(Link1, ".html", Link).

files_not_omitted_files(_, [], []).
files_not_omitted_files(Base, [X|Xs], Ys) :-
    source_folder(S1),
    source_lib_folder(S2),
    canonicalize(S1, S2, SFSG),
    path_segments(SF, SFSG),
    path_segments(Separator, ["", ""]),
    findall(FullOmitFile,(
		omit(Omit),
		member(OmitFile, Omit),
		append([SF, Separator, ".", Separator, OmitFile], FullOmitFile)
	    ), OmitFiles),
    append([Base, Separator, X], File),
    (
	member(File, OmitFiles) ->
	Ys = Ys0
    ;   Ys = [X|Ys0]
    ),
    files_not_omitted_files(Base, Xs, Ys0).

generate_nav_learn(NavLearn) :-
    learn_pages_categories(Categories),
    maplist(generate_nav_learn_cat, Categories, Items),
    render("nav_tree.html", ["items"-Items], NavLearn).

generate_nav_learn_cat(Category, SubNav) :-
    learn_pages(Pages),
    findall(Item, (
		member(Page, Pages),
		Page = page(Name, Category, Source),
		append(BaseFile, ".dj", Source),
		append(BaseFile, ".html", File),
		append("/learn/", File, Link),
		Item = ["name"-Name, "link"-Link, "type"-"file"]
	    ), Items),
    render("nav_tree.html", ["items"-Items], Text),
    SubNav = ["name"-Category, "nav"-Text, "type"-"dir"].

generate_footer(Footer) :-
    current_time(T),
    phrase(format_time("%b %d %Y", T), Time),
    render("footer.html", ["time"-Time], Footer).

generate_page_learn(Sections) :-
    learn_pages(Pages),
    output_folder(OutputFolder),
    path_segments(OutputFolder, O1),
    append(O1, ["learn"], LearnFolderSg),
    path_segments(LearnFolder, LearnFolderSg),
    make_directory_path(LearnFolder),
    maplist(generate_page_learn_(Sections, LearnFolderSg), Pages).

generate_page_learn_(Sections, LearnFolderSg, page(Name, Category, Source)) :-
    source_folder(SF),
    learn_pages_source_folder(SourceFolder),
    project_name(ProjectName),
    docs_base_url(BaseURL),
    path_segments(SF, S0),
    path_segments(SourceFolder, S1),
    append(S0, S1, S2),
    append(S2, [Source], S3),
    path_segments(SourceFile, S3),
    append(F1, ".dj", Source),
    append(F1, ".html", F2),
    append(LearnFolderSg, [F2], O1),
    path_segments(OutputFile, O1),
    append("/learn/", F2, LearnURL),
    ( sitemap_url(LearnURL) -> true ; assertz(sitemap_url(LearnURL)) ),
    ( file_newer(SourceFile, OutputFile) ->
      portray_color(blue, skip_rendering_learn_page(Name, Category))
    ; portray_color(green, rendering_learn_page(Name, Category)),
      phrase_from_file(seq(Text), SourceFile),
      djot(Text, Html),
      Vars0 = ["project_name"-ProjectName, "base_url"-BaseURL, "name"-Name, "category"-Category, "content"-Html],
      append(Vars0, Sections, Vars),
      render("learn.html", Vars, LearnHtml),
      phrase_to_file(seq(LearnHtml), OutputFile)
    ).


generate_readme(Sections) :-
    source_folder(S1),
    path_segments(S1, S2),
    readme_file(R1),
    append(S2, [R1], R2),
    path_segments(ReadmeFile, R2),
    project_name(ProjectName),
    docs_base_url(BaseURL),
    output_folder(OutputFolder),
    path_segments(OutputFolder, OutputFolderSg),
    append(OutputFolderSg, ["index.html"], OutputFileSg),
    path_segments(OutputFile, OutputFileSg),
    ( sitemap_url("/index.html") -> true ; assertz(sitemap_url("/index.html")) ),
    ( file_newer(ReadmeFile, OutputFile) ->
      portray_color(blue, skip_readme)
    ; portray_color(green, readme),
      phrase_from_file(seq(ReadmeMd), ReadmeFile),
      djot(ReadmeMd, ReadmeHtml),
      Vars0 = ["project_name"-ProjectName, "base_url"-BaseURL, "readme"-ReadmeHtml],
      append(Vars0, Sections, Vars),
      render("index.html", Vars, IndexHtml),
      phrase_to_file(seq(IndexHtml), OutputFile)
    ).

generate_page_docs(Sections) :-
    source_folder(S1),
    source_lib_folder(S2),
    canonicalize(S1, S2, Base),
    path_segments(DocsFolder, Base),
    output_folder(OutputFolder),
    make_directory_path(OutputFolder),
    directory_files(DocsFolder, Files0),
    sort(Files0, Files),
    path_segments(OutputFolder, Output),
    append(Output, ["search-index.json"], SearchIndexSg),
    path_segments(SearchIndex, SearchIndexSg),
    setup_call_cleanup(open(SearchIndex, write, SearchWriteStream),(
	format(SearchWriteStream, "[", []),
        maplist(process_file(Base, Output, Sections, SearchWriteStream), Files),
	format(SearchWriteStream, "{}]", [])
		       ), close(SearchWriteStream)),

    % Render navmenu.html → nav_menu.html
    Sections = ["nav_lib"-NavLib, "nav_learn"-NavLearn | _],
    VarsNav = Sections,
    render("navmenu.html", VarsNav, NavHtml),
    append(Output, ["nav_menu.html"], NavOutSg),
    path_segments(NavOut, NavOutSg),
    phrase_to_file(seq(NavHtml), NavOut),
    append(Output, ["doclog.css"], F1),
    append(Output, ["doclog.js"], F2),
    append(Output, ["default.min.css"], F5),
    append(Output, ["highlight.prolog.min.js"], F6),
	append(Output,["robots.txt"], F25),
	path_segments(F26,F25),
    path_segments(F3, F1),
    path_segments(F4, F2),
    path_segments(F7, F5),
    path_segments(F8, F6),
    file_copy("doclog.css", F3),
    file_copy("doclog.js", F4),
    file_copy("highlight.js/default.min.css", F7),
    file_copy("highlight.js/highlight.prolog.min.js", F8),
    file_copy("robots.txt", F26),
	write_sitemap(OutputFolder).	


write_sitemap(OutputFolderPath) :-
    path_segments(OutputFolderPath, OutputFolderSg),
    append(OutputFolderSg, ["sitemap.xml"], SitemapPathSg),
    path_segments(SitemapPath, SitemapPathSg),
	findall(S,sitemap_url(S),URLs),
    phrase_to_file(sitemap_xml(URLs), SitemapPath).
	
sitemap_xml(URLs) -->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
    sitemap_urls(URLs),
    "</urlset>\n".

sitemap_urls([URL|URLs]) -->
    "  <url><loc>", URL, "</loc></url>\n",
    sitemap_urls(URLs).

sitemap_urls([]) --> [].


process_file(Base, Output0, Sections, SearchWriteStream, File0) :-
    append(Base, [File0], FileSg),
    append(File1, ".pl", File0),
    append(File1, ".html", Output1),
    append(Output0, [Output1], OutputSg),
    path_segments(Output, OutputSg),
    path_segments(File, FileSg),
    file_exists(File),
    add_sitemap_url_from_output(Output),

    ( file_newer(File, Output) ->
      portray_color(blue, skip_file(File))
    ; portray_color(green, process_file(File)),
      open(File, read, FileStream),
      read_term(FileStream, Term, []),
      (
        Term = (:- module(ModuleName, PublicPredicates)) ->
        (
            predicates_clean(PublicPredicates, PublicPredicates1, Ops),
            document_file(File, Output, ModuleName, PublicPredicates1, Ops, Sections),
            append_predicates_search_index(Output, PublicPredicates1, Ops, SearchWriteStream)
        )
      ; true
      ),

      close(FileStream)
    ).

process_file(Base0, Output0, Sections, SearchWriteStream, Dir0) :-
    append(Base0, [Dir0], DirSg),
    append(Output0, [Dir0], Output),
    path_segments(Dir, DirSg),
    directory_exists(Dir),
    path_segments(OutputDir, Output),
    make_directory_path(OutputDir),
    directory_files(Dir, Files),
    maplist(process_file(DirSg, Output, Sections, SearchWriteStream), Files).

predicates_clean([], [], []).
predicates_clean([X|Xs], [X|Ys], Ops) :-
    X \= op(_,_,_),
    predicates_clean(Xs, Ys, Ops).
predicates_clean([X|Xs], Ys, [X|Ops]) :-
    X = op(_,_,_),
    predicates_clean(Xs, Ys, Ops).

append_predicates_search_index(Output, PublicPredicates, Ops, SearchWriteStream) :-
    output_folder(OF),
    append(OF, Relative, Output),
    maplist(append_search_index(Relative, SearchWriteStream, Ops), PublicPredicates).

append_search_index(Output, SearchWriteStream, Ops, Predicate) :-
    predicate_string(Predicate, Ops, PredicateString),
    phrase(escape_js(PredicateStringSafe), PredicateString),
    format(SearchWriteStream, "{\"link\": \"~s#~s\", \"predicate\": \"~s\"},", [Output, PredicateStringSafe, PredicateStringSafe]).

append_search_index(Output, SearchWriteStream, op(_,_,Operator)) :-
    atom_chars(Operator, NameUnsafe),
    phrase(escape_js(Name), NameUnsafe),
    format(SearchWriteStream, "{\"link\": \"~s\", \"predicate\": \"~s\"},", [Output, Name]).

add_sitemap_url_from_output(Output) :-
    output_folder(OF),
    append(OF, Relative, Output),
    replace_char('\\', '/', Relative, Relative1),
    ( Relative1 = ['/'|_] ->
        URL = Relative1
    ;   URL = ['/'|Relative1]
    ),
    ( sitemap_url(URL) -> true ; assertz(sitemap_url(URL)) ).


escape_js([]) --> [].
escape_js([X|Xs]) -->
    [X],
    {
	X \= (\)
    },
    escape_js(Xs).
escape_js(Xs) -->
    "\\",
    escape_js(Xs0),
    { append("\\\\", Xs0, Xs) }.

replace_char(_, _, [], []).
replace_char(X, Y, [X|Xs], [Y|Ys]) :-
    replace_char(X, Y, Xs, Ys).
replace_char(X, Y, [Z|Xs], [Z|Ys]) :-
    X \= Z,
    replace_char(X, Y, Xs, Ys).

% let's try to document every text comment we see
% Later, we'll add public predicates that have no documentation
document_file(InputFile, OutputFile, ModuleName, PublicPredicates, Ops, Sections) :-
    phrase_from_file(seq(FileText), InputFile),
    phrase(documented_predicates(Predicates0, Ops), FileText),
    public_undocumented_predicates(Predicates0, Ops, PublicPredicates, PublicUndocumented),
    (   PublicUndocumented \= [],
        % Only portray if there are undocumented items
        portray_color(yellow, undocumented_public_predicates(PublicUndocumented))
    ;
        green(Green),
        reset(Reset),
        format("~sFile fully documented.~s~n", [Green, Reset])
    ),
    maplist(document_predicate(Ops), PublicUndocumented, Predicates1),
    append(Predicates0, Predicates1, Predicates),
    phrase(module_description(ModuleDescriptionMd), FileText),
    djot(ModuleDescriptionMd, ModuleDescriptionHtml),
    atom_chars(ModuleName, ModuleNameStr),
    project_name(ProjectName),
    docs_base_url(BaseURL),
    source_folder(S1),
    source_lib_folder(S2),
    canonicalize(S1, S2, S5),
    path_segments(SF, S5),
    websource(WebSourceBase),
    append(SF, ExtraFile, InputFile),
    path_segments(Separator, ["", ""]),
    append([Separator, LibraryUse, ".pl"], ExtraFile),
    % The use_module directive always has to use forward slashes!
    replace_char('\\', '/', LibraryUse, LibraryUse1),
    append(WebSourceBase, ExtraFile, WebSource),
    Vars0 = [
	"project_name"-ProjectName,
	"base_url"-BaseURL,
	"module_name"-ModuleNameStr,
	"module_description"-ModuleDescriptionHtml,
	"predicates"-Predicates,
	"websource"-WebSource,
	"library"-LibraryUse1
    ],
    append(Vars0, Sections, Vars),
    render("page.html", Vars, HtmlOut),
    phrase_to_file(seq(HtmlOut), OutputFile).

documented_predicates([], _) --> "".
documented_predicates([PredicateVars|Ps], Ops) -->
    predicate_documentation(Predicate, Name, DescriptionDjot),!,
    {
	predicate_string(Predicate, Ops, PredicateString),
	portray_clause(documenting(PredicateString)),
	djot(DescriptionDjot, Description),
	PredicateVars = ["predicate"-PredicateString, "name"-Name, "description"-Description]
    },
    documented_predicates(Ps, Ops).

documented_predicates(Ps, Ops) -->
    ... , "\n",
    documented_predicates(Ps, Ops).

module_description(X) -->
    ... ,
    "/**",
    seq(X),
    "*/",
    ... .

module_description("No description") -->
    ... .

predicate_string(Predicate, Ops, PredicateString) :-
    Predicate = PN/PA,
    member(op(_, _, PN), Ops),
    phrase(format_("(~a)/~d", [PN, PA]), PredicateString).

predicate_string(Predicate, Ops, PredicateString) :-
    Predicate = PN/_PA,
    \+ member(op(_, _, PN), Ops),
    phrase(format_("~q", [Predicate]), PredicateString).

predicate_string(Predicate, _Ops, PredicateString) :-
    Predicate = _PN//_PA,
    phrase(format_("~q", [Predicate]), PredicateString).

document_predicate(Ops, Predicate, ["predicate"-Name, "name"-Name, "description"-Description]) :-
    predicate_string(Predicate, Ops, Name),
    Description = "".

public_undocumented_predicates(_, _, [], []).
public_undocumented_predicates(Documented, Ops, [Predicate|Public], Undocumented) :-
    predicate_string(Predicate, Ops, PredicateString),
    member(["predicate"-PredicateString|_], Documented),
    public_undocumented_predicates(Documented, Ops, Public, Undocumented).
public_undocumented_predicates(Documented, Ops, [Predicate|Public], [Predicate|Undocumented]) :-
    predicate_string(Predicate, Ops, PredicateString),
    \+ member(["predicate"-PredicateString|_], Documented),
    public_undocumented_predicates(Documented, Ops, Public, Undocumented).

predicate_documentation(Predicate, Name, Description) -->
    "%% ", seq(Name), "\n%", { \+ member('\n', Name) },
    whites, "\n",
    predicate_description(Description),
    { phrase(predicate_name(Predicate), Name) }.

predicate_name_seq([X|Xs]) -->
    [X],
    { maplist(dif(X), " ()") },
    predicate_name_seq(Xs).
predicate_name_seq([]) --> "".

predicate_name(PredicateName//Arity) -->
    predicate_name_seq(PredicateNameCs),
    "(",
    seq(Args),
    ")//",
    {
	Commas #= Arity - 1,
	phrase(commas(Commas), Args),
	atom_chars(PredicateName, PredicateNameCs)
    }.

predicate_name(PredicateName//0) -->
    predicate_name_seq(PredicateNameCs),
    "//",
    {
	atom_chars(PredicateName, PredicateNameCs)
    }.

predicate_name(PredicateName/Arity) -->
    predicate_name_seq(PredicateNameCs),
    "(",
    seq(Args),
    ")",
    ... ,
    {
	Commas #= Arity - 1,
	phrase(commas(Commas), Args),
	atom_chars(PredicateName, PredicateNameCs)
    }.

predicate_name(PredicateName/0) -->
    predicate_name_seq(PredicateNameCs),
    ".",
    {
	atom_chars(PredicateName, PredicateNameCs)
    }.

predicate_name(PredicateName/0) -->
    predicate_name_seq(PredicateNameCs),
    ... ,
    {
	atom_chars(PredicateName, PredicateNameCs)
    }.

predicate_description(Description) -->
    "% ", seq(Line), "\n",
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

red(X) :-
    chars_utf8bytes(A, [27]),
    append(A, "[0;31m", X).
green(X) :-
    chars_utf8bytes(A, [27]),
    append(A, "[0;32m", X).
yellow(X) :-
    chars_utf8bytes(A, [27]),
    append(A, "[0;33m", X).
blue(X) :-
    chars_utf8bytes(A, [27]),
    append(A, "[0;34m", X).
reset(X) :-
    chars_utf8bytes(A, [27]),
    append(A, "[0m", X).

portray_color(Color, X) :-
    call(Color, A), reset(B),
    phrase(portray_clause_(X), S),
    append(S1, "\n", S),
    format("~s~s~s~n", [A,S1,B]).

file_newer(A, B) :-
    file_exists(B),
    file_modification_time(A, TA),
    file_modification_time(B, TB),
    TA @< TB.
