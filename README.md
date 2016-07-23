# Syntactic extensibility outside expressions

Racket's macro system only expands forms in *expression* position by
default.

However, certain DSLs within Racket, notably
[`match`](http://docs.racket-lang.org/reference/match.html), provide
hooks for defining macros that are expanded in DSL-specific contexts.
In the case of `match`, one may define
[match expanders](http://docs.racket-lang.org/reference/match.html#%28part._.Extending_match%29)
which allow abstraction over match patterns.

This package provides a form `define-auxiliary-macro-context`, with an
implementation based on that of match expanders, which allows
convenient definition of new kinds of macro expansion contexts. In
principle, match expanders could be defined in terms of
`define-auxiliary-macro-context`.

Potential uses include:

 - defining one's own pattern language, complete with match-expander
   style macros for extending the pattern syntax

 - defining a new kind of `lambda`, whose binding specification form
   is extensible

Toy/testing examples of the use of `define-auxiliary-macro-context`
can be found in
[`auxiliary-macro-context/test/`](auxiliary-macro-context/test/).

A real-world example can be found in the implementation of the
pattern-language over assertions in
[Syndicate](https://github.com/tonyg/syndicate/).

# Installation

Install via the package catalog,

    raco pkg install auxiliary-macro-context

Alternatively, check out the repository and, in the directory
containing `Makefile`,

    make link

or

	raco pkg install --link -n auxiliary-macro-context `pwd`

# Licence

Copyright (C) 2016 Tony Garnock-Jones <mailto:tonyg@leastfixedpoint.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program (see the files "lgpl.txt" and
"gpl.txt"). If not, see <http://www.gnu.org/licenses/>.
