# WIP - DocLog

WORK IN PROGRESS

Create documentation from your Prolog code

Example, Scryer Prolog documentation: https://scryer-prolog-docs.netlify.app/

## How to document your code?

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

## Using Doclog

First, clone the repo:

```
$ git clone https://github.com/aarroyoc/doclog
$ cd doclog
```

Then, install the dependencies:

```
$ make setup
```

After that, you must create a configuration file. This file will contain several options required to document your project, including your source directory, a README file and an output dir. Take a look at `scryer.config.pl` for an example file.

With your config file, you can execute Doclog:

```
bash doclog.sh CONFIG_FILE
```

And wait for the docs to be generated!