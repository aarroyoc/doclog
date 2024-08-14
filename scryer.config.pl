project_name("Scryer Prolog").
readme_file("/home/aarroyoc/dev/scryer-prolog/INDEX.dj").
source_folder("/home/aarroyoc/dev/scryer-prolog/src/lib").
output_folder("/home/aarroyoc/dev/doclog/output").
websource("https://github.com/mthom/scryer-prolog/tree/master/src/lib").
omit(["ops_and_meta_predicates.pl", "tabling"]).
learn_pages_source_folder("/home/aarroyoc/dev/scryer-prolog/learn").
learn_pages_categories(["First steps", "Advanced topics"]).
learn_pages([
		   page("Test page", "First steps", "test-page.dj"),
		   page("Second test page", "Advanced topics", "second-test-page.dj"),
		   page("Third test page", "First steps", "test-page-3.dj"),
]).

