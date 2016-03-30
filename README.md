# Caradoc - a PDF parser and validator

Caradoc is a parser and validator of PDF files written in OCaml.

## Dependencies

Along with an OCaml compiler, this program depends on the following libraries :

- `menhir`, the parser generator that we use to convert PDF grammar into OCaml code ;
- `ounit`, to manage unit tests ;
- `cryptokit`, to handle the decoding of [Deflate](http://www.ietf.org/rfc/rfc1951.txt) streams in PDF files.

The prefered way to install dependencies is via [opam](https://opam.ocaml.org/), the OCaml package manager.
The following commands give an example of installation.
```
apt-get install ocaml opam
apt-get install zlib1g-dev
opam init
opam install ocamlfind
opam install cryptokit ounit menhir
```

It is also possible to use the corresponding Debian packages on Jessie.


## Installation

After installing the dependencies, just type `make` to compile the code.
You may want to run `make test` to check that the program runs properly on your architecture and versions of OCaml and OPAM.

## Examples

To obtain simple statistics on a PDF file, just type :
```
caradoc stats path/to/your/input.pdf
```

To validate a PDF file, check the exit code of :
```
caradoc stats --strict path/to/your/input.pdf
```

To normalize a PDF file into the strict syntax :
```
caradoc cleanup path/to/your/input.pdf --out path/to/your/output.pdf
```

To print the xref table(s) :
```
caradoc xref path/to/your/input.pdf
```

To print the trailer(s) :
```
caradoc trailer path/to/your/input.pdf
```

To extract a specific object, given its object number (and generation number, defaulted to zero) :
```
caradoc object --num 2 path/to/your/input.pdf
caradoc object --num 2 --gen 5 path/to/your/input.pdf
```

To extract complex data in a single command (xref table, dump of all objects, list of types, graph of references) :
```
caradoc extract --xref <xref output file> --dump <objects output file> --types <types output file> --dot <graph output file> path/to/your/input.pdf
```

To print the list of PDF types handled by this version of Caradoc :
```
caradoc types
```

## Ad-hoc parser options

You can specify an option file as parameter of most commands in the relaxed parser mode :

```
caradoc stats --options path/to/option/file input.pdf
```

You need to put one option per line in the option file.

The following options are defined, to cope with common errors produced by various PDF software.

- `allow-invalid-version` : allow the header of the PDF file to be ill-formed. However, the file must begin with `%PDF`.
- `allow-dict-duplicates` : allow a PDF dictionary to contain several times the same key. In that case, the last occurrence of the key is kept as the actual value.
- `zero-offset-as-free` : treat an xref table entry with an offset of zero as a *free* object. Some software produce such ill-formed PDFs.
- `undefined-ref-as-null` : treat a reference to an undeclared object as the *null* object. By default, such references trigger an error.
- `allow-arbitrary-info` : allow the *Info* dictionary to contain arbitrary (metadata) keys. Otherwise, unknown keys trigger an error in the type checker.

## Structure of the code

The source code is organized as follows :

- `src/` : source code of `caradoc` ;
- `src/data/` : modules to represent PDF data (e.g. object, xref table, PDF document) ;
- `src/graph/` : graph checker ;
- `src/parser/` : core parsing code (for the strict and the relaxed parser) ;
- `src/parser/relaxed/` : specific parsing code of the relaxed parser (to fetch objects from xref tables) ;
- `src/stats/` : modules to store statistics about a PDF file ;
- `src/stream/` : decoding of PDF streams ;
- `src/type/` : code of the type checker ;
- `src/type/pdf/` : definitions of PDF types ;
- `src/util/` : various helper modules ;
- `test/` : unit tests ;
- `test_files/` : examples of valid and corrupted PDF files.

