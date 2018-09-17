# pg-types

The naming of this repo is not great, but I'll fix that later.
Basically, these are some simple utilities to take a postgres query
template and spit out some structured information relaying the input and
output types of the query.  The possible use cases for this are to
generate code types in different languages that specify the interface
for a given query.  Potentially, the easiest thing is to just generate
thrift types, but as a fun exercise, I might just skip the middleman and
generate some Python named tuples / functions on my own.

The executable here is just a simple way for me to test.  This is
intended primarily as a library (though I may create different
executables for code generation at some point!)
