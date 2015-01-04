# gecf

k-edge-conectedness of graph using push-relabel flow search

## Usage

Reads sequence of edges from stdin. For details on format see --help.
(Essentially either: \[1 2\] \[2 3\] \[3 1\] ... or 1 2 2 3 3 1)
At this point only returns computed k.

## Get

git clone ...
build using Leiningen. ($ lein deps && lein run)

## License

Copyright © 2014 Martin Chmelík

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
