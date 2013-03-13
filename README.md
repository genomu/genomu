Genomu
======

A concurrency-oriented K/V database

Installation
============

In order to build and fire up Genomu, you'll need a couple of things:

* Erlang R16B: http://erlang.org/
* Elixir (latest master branch): https://github.com/elixir-lang/elixir
* avahi daemon (only if you're on Linux, don't worry about it on OS X)

To build, simply run `make`. Hopefully, this will end successfully and you
can fire up a test cluster using `make start`. That will start three
nodes (their configs can be found in the ./test directory). Make sure to
check out its web console at http://localhost:9119


License
=======

   Copyright 2012, 2013 Spawngrid, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.