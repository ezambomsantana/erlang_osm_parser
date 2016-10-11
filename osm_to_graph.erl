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
    create_graph(ListOsm, Graph),
    io:format("num Vertix: ~w~n", [ digraph:no_vertices(Graph) ]),
    io:format("num Edge: ~w~n", [ digraph:no_edges(Graph)  ]),	
  %  print_node(ListOsm),	
    print_graph(Graph),
    Graph.

% create the vertices and the edges from the list of osm ways
create_graph(ListWay, Graph) ->

    create_vertex(Graph , ListWay),

    create_edges(Graph , ListWay , ListWay).


% iterate over the list of ways to create the vertices
create_vertex(_Graph , []) ->
	ok;

create_vertex(Graph , [Element | MoreElements]) ->
	Name = element(1, Element),
	digraph:add_vertex(Graph, Name),
	create_vertex(Graph , MoreElements).

% iterate over the list of ways to create the edges where the vertex is the origin
create_edges(_Graph , [], _List) ->
	ok;

create_edges(Graph , [Element | MoreElements], List) ->
	NameVertex = element(1, Element),
	NodeList = element(2, Element),
	create_edges_vertex(Graph , NameVertex , NodeList , List),
	create_edges(Graph , MoreElements, List).


% find all nodes to all vertices
create_edges_vertex(_Graph , _NameVertex ,  [] , _List) ->
	ok;

create_edges_vertex(Graph , NameVertex , [Element | MoreElements] , List) ->

	Name = element(1, Element),
	NodeList = element(2, Element),

        case Name of

		nd ->
		
			create_edges_for_vertex(Graph , NameVertex , NodeList, List),
			create_edges_vertex(Graph, NameVertex , MoreElements, List);

		_ -> 

			create_edges_vertex(Graph, NameVertex , MoreElements, List)

	end.


% get the id of the nodes
create_edges_for_vertex(_Graph , _NameVertex , [] , _List) ->
	ok;

create_edges_for_vertex(Graph , NameVertex , [ Node | MoreNodes ] , List) ->

	verify_child_destination( Graph , NameVertex , Node , List ),

	create_edges_for_vertex( Graph , NameVertex , MoreNodes , List).



% iterate over all the possible destinations of a vertex
verify_child_destination(_Graph , _NameVertex , _Node , []) ->
	ok;

verify_child_destination(Graph , NameVertex , Node , [Element | MoreElements]) ->

	NameDestination = element(1, Element),
	NodeListDestination = element(2, Element),

	case NameVertex of

		NameDestination -> 
			
			verify_child_destination( Graph , NameVertex , Node , MoreElements);

		_ ->

			verify_child_attribute_destination(Graph, NameVertex , NameDestination , Node , NodeListDestination),
			verify_child_destination( Graph , NameVertex , Node , MoreElements)
	end.

	
% get all the nodes from the destination
verify_child_attribute_destination(_Graph , _NameVertex , _NameDestination , _Node , []) ->
	ok;

verify_child_attribute_destination(Graph , NameVertex , NameDestination , Node , [Element | MoreElements]) ->

	NodeListDestination = element(2, Element),

	verify_child_origin( Graph , Node , NameVertex , NameDestination , NodeListDestination),

	verify_child_attribute_destination( Graph , NameVertex , NameDestination , Node , MoreElements).


% comparethe id of the origin node and the id of the destination node
% if it is equal, then exists an edge between the two vertices
verify_child_origin( _Graph , _Node , _NameVertex , _NameDestination , []) ->
	ok;

verify_child_origin( Graph , Node , NameVertex , NameDestination , [Element | MoreElements]) ->
    IdNodeOrigin = element(2, Node),
    IdNodeDestin = element(2, Element),

    case IdNodeOrigin of

	IdNodeDestin ->

		V1 = digraph:vertex(Graph, NameVertex),
		V2 = digraph:vertex(Graph, NameDestination),

		digraph:add_edge(Graph, element(1 , V1), element(1 , V2)),
    		
		true;

	_ -> 
		
		false
    end,
    verify_child_origin( Graph , Node , NameVertex , NameDestination , MoreElements ).

% print the list of ways and children
print_node([]) ->
	ok;

print_node([Element | MoreElements]) ->
	io:format("name: ~s~n", [element(1, Element)]),
	print_children(element(2,Element)),
	print_node(MoreElements).

print_children([]) ->
	ok;

print_children([Element | MoreElements]) ->
	io:format("name: ~s~n", [element(1, Element)]),
	print_attribute(element(2,Element)),
	print_children(MoreElements).

print_attribute([]) ->
	ok;

print_attribute([Element | MoreElements]) ->
	io:format("atribute: ~s~n", [element(1, Element)]),
	io:format("atribute-value: ~s~n", [element(2, Element)]),
	print_attribute(MoreElements).


print_graph( Graph ) ->
	
	Vertices = digraph:vertices( Graph ),
	Edges = digraph:edges( Graph ),

	print_vertices( Vertices ),
	print_edges( Graph , Edges ).



print_vertices([]) ->
	ok;

print_vertices([Element | MoreElements]) ->
	io:format("name: ~s~n", [ Element ]),
	print_vertices(MoreElements).



print_edges(_Graph , []) ->
	ok;

print_edges(Graph, [Element | MoreElements]) ->
	Edge = digraph:edge(Graph, Element),
	io:format("vInicio: ~s~n", [ element( 2 , Edge ) ]),
	io:format("vFim: ~s~n", [ element( 3 , Edge) ]),
	print_edges( Graph , MoreElements).
