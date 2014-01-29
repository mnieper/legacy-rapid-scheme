Rapid Scheme
============

A Scheme implementation in the browser

Overview
--------

Rapid Scheme will be an R7RS compliant Scheme implementation including a compiler that compiles a Scheme program to a web worker that can be run in a browser environment. The compiler itself is written in Scheme and will be able to compile itself. A set of Javascript tools to run the generated web worker will be provided.

Quick Start
-----------

At the moment, the implementation is work-in-progress.

Implementation
--------------

Rapid Scheme will be compatible to the small language as described in R7RS as much as possible. In addition, it will have the following features, which are left unspecified in R7RS:

- No matter how often a library or its definitions are referenced in a program, which includes referencing during runtime in the `environment` eval library procedure, the library is only loaded once.
- At the beginning of a top-level program, an import declaration takes precedence over an `import` form that may have been imported from a previous import declaration. An unambiguous way to separate the import declarations from the command and definitions of a top-level program is to wrap the command and definitions in a `begin` form.
