# WIP - DocLog

WORK IN PROGRESS

# TODO

* Parse Markdown
* Module comments
* Predicate comments
* Show source code of module
* Make it beautiful
* Pipeline Scryer Docs generation

Create documentation from your Prolog code

Example, Scryer Prolog documentation: https://scryer-prolog-docs.netlify.app/

# How to document your code?

There are two kind of comments in DocLog: module and predicate.

Each file/module has the option to write a module comment. It will be displayed at the beginning of the page. You can use Markdown inside the comment. The syntax is:
```
/**
COMMENT
MORE COMMENT
*/
```

Predicate comments start with %! and they're followed by N % comments. You should indicate in the first line, the name of the predicate, var names and modes. Then you can use Markdown to explain the rest of the predicate.

```
%! append(L0, L1, L)
%  L is L0 and L1 appended together
```
 