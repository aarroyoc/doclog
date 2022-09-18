:- use_module(library(dcgs)).
:- use_module(library(files)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(ordsets)).

:- use_module('../teruel/teruel').

run(DocsFolder, OutputFolder) :-
    portray_clause(doclog(1, 0, 0)),
    make_directory(OutputFolder),
    directory_files(DocsFolder, Files),
    path_segments(DocsFolder, Base),
    path_segments(OutputFolder, Output),
    maplist(process_file(Base, Output), Files),
    gen_index(Output, Files).

process_file(Base, Output0, File0) :-
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
	Term =.. [:-,module(ModuleName, PublicPredicates)] ->
	document_file(File, Output, ModuleName, PublicPredicates)
    ;   true
    ),
    close(FileStream).
    

process_file(Base0, Output0, Dir0) :-
    append(Base0, [Dir0], DirSg),
    append(Output0, [Dir0], Output),
    path_segments(Dir, DirSg),
    directory_exists(Dir),
    path_segments(OutputDir, Output),
    make_directory(OutputDir),
    directory_files(Dir, Files),
    maplist(process_file(DirSg, Output), Files),
    gen_index(Output, Files).

document_file(InputFile, OutputFile, ModuleName, PublicPredicates) :-
    portray_clause(documenting(PublicPredicates)),
    phrase_from_file(module_description(ModuleDescription), InputFile),
    atom_chars(ModuleName, ModuleNameStr),
    render("page.html", ["module_name"-ModuleNameStr, "module_description"-ModuleDescription], HtmlOut),
    open(OutputFile, write, OutputStream),
    format(OutputStream, "~s", [HtmlOut]),
    close(OutputStream).

module_description(X) -->
    ... ,
    "/**",
    seq(X),
    "*/",
    ... .

module_description([]) -->
    ... .

gen_index(Output, Files) :-
    append(Output, ["index.html"], OutputIndexSg),
    path_segments(OutputIndexFile, OutputIndexSg),
    dirs_only(Files, Output, DirsOnly),
    list_to_ord_set(Files, FilesSet),
    list_to_ord_set(DirsOnly, DirsOnlySet),
    ord_subtract(FilesSet, DirsOnlySet, FilesOnly),
    open(OutputIndexFile, write, IndexStream),
    render("index.html", ["files"-FilesOnly, "dirs"-DirsOnly], HtmlIndex),
    format(IndexStream, "~s", [HtmlIndex]),
    close(IndexStream).

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


