-module(osm_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         show/1
        ]).

show(Infilename) ->
    {Doc, _Misc} = xmerl_scan:file(Infilename),
    show_node(Doc),
    ok.

%
% Show a node/element and then the children of that node.
show_node(Node) ->
    case Node of
        #xmlElement{name=Name, attributes=Attributes, content=Content} ->
            
	    case Name of
		
		way -> 

			io:format("name: ~s~n", [ Name ]),
            		List = children(Content , []),
			print_node(List);
		_ ->
            		show_children(Content)
	    end;

            _ -> ok
    end.

show_children([]) ->
    ok;
show_children([Node | MoreNodes]) ->
    show_node(Node),
    show_children(MoreNodes).

children([], List) ->
    List;

children([Node | MoreNodes], List) ->
    Element = extract_children(Node),
    case Element of

	ok ->
    		
		children(MoreNodes , List);

	_ ->
		NewList = List ++ Element,
		children(MoreNodes , NewList)

    end.


extract_children(Node) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		tag -> 

			List = get_attributes(Attributes , []),
			[ { Name , List } ];

		_ -> ok

	    end;
            _ -> ok
    end.
			
get_attributes([] , List) ->
    List;

get_attributes([Attribute | MoreAttributes], List) ->
    #xmlAttribute{name=Name, value=Value} = Attribute,
    NewList = List ++ [ { Name, Value } ],
    get_attributes(MoreAttributes , NewList).

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
