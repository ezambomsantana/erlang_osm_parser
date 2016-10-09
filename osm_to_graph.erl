-module(osm_to_graph).

-import(osm_parser, [show/1]).

% usage:
%
% l(osm_to_graph).
% osm_to_graphr:init("map.osm").

-export([
         init/1
        ]).

init(Infilename) ->

    ListOsm = osm_parser:show(Infilename),
    Graph = digraph:new(),
    io:format("name: ~w~n", [ digraph:no_vertices(Graph) ]),	
    create_graph(ListOsm, Graph),
    io:format("name: ~w~n", [ digraph:no_vertices(Graph) ]),	
    Graph.

create_graph([Head | _List], Graph) ->

    ListWay = element(2,Head),

    create_vertex(Graph , ListWay).


create_vertex(_Graph , []) ->
	ok;

create_vertex(Graph , [Element | MoreElements]) ->
	io:format("passou\n"),
	Name = element(1, Element),
    io:format("name: ~w~n", [ Name ]),	
	digraph:add_vertex(Graph, Name),
	create_vertex(Graph , MoreElements).

% it is missing one level
print_node([]) ->
	ok;

print_node([Element | MoreElements]) ->
	io:format("name: ~s~n", [element(1, Element)]),
	print_attribute(element(2,Element)),
	print_node(MoreElements).

print_attribute([]) ->
	ok;

print_attribute([Element | MoreElements]) ->
	io:format("atribute: ~s~n", [element(1, Element)]),
	io:format("atribute-value: ~s~n", [element(2, Element)]),
	print_attribute(MoreElements).
