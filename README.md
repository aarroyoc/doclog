# DocLog

Create documentation from your Prolog code

Example, Scryer Prolog documentation: https://www.scryer.pl

## How to document your code?

There are two kind of comments in DocLog: module and predicate.

Each file/module has the option to write a module comment. It will be displayed at the beginning of the page. You can use [Djot](https://djot.net/) inside the comment. The syntax is:
```
/**
COMMENT
MORE COMMENT
*/
```

Predicate comments start with %% and they're followed by N % comments. Of those lines, the first line comment should be empty. You should indicate in the first line, the name of the predicate, var names and modes. Then you can use [Djot](https://djot.net/) to explain the rest of the predicate.

```
%% append(L0, L1, L)
%
% L is L0 and L1 appended together
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

After that, you must create a configuration file called `doclog.config.pl`. This file will contain several options required to document your project, and must be in your source directory. Take a look at `scryer.config.pl` for an example file.

With your config file, you can execute Doclog:

```
./doclog.sh SOURCE_FOLDER OUTPUT_FOLDER
```

And wait for the docs to be generated!

While developing, it might be usefull to rebuild everytime something in the SOURCE\_FOLDER changed. You can do so, by starting this command:

```
./watch.sh SOURCE_FOLDER OUTPUT_FOLDER
```
