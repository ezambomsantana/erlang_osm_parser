-module(osm_to_graph_test).

-import(osm_to_graph, [int/1]).

% usage:
%
% l(osm_to_graph).
% osm_to_graphr:init("map.osm").

-export([
         test/1
        ]).

test(Infilename) ->

    ListVertex = [{v1, {10 , 0}}, {v2, {20 , 0}}, {v3, {30 , 0}}, {v4, {30 , 0}}, {v5, {30 , 0}}],
    DictVertices = dict:from_list(ListVertex),

    io:format("vakue dict: ~w~n", [ dict:find(v1, DictVertices) ]),
    

    Graph = osm_to_graph:init(Infilename),
    Path = digraph:get_path( Graph , '40271235' , '440744039'),
    io:format("Path: ~w~n", [ Path ]).
 
