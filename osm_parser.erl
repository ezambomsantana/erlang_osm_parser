-module(osm_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         show/1
        ]).

% Init the XML processing
show(Infilename) ->
    {Doc, _Misc} = xmerl_scan:file(Infilename),
    init(Doc).

% read the OSM tag and extract all children
init(Node) ->
    case Node of
        #xmlElement{name=Name, attributes=Attributes, content=Content} ->
            
	    case Name of
		
		osm -> 

			List = osm(Content , []),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

osm([], List) ->
    List;

osm([Node | MoreNodes], List) ->
    Element = extract_node(Node),
    case Element of

	ok ->
    		
		osm(MoreNodes , List);

	_ ->
		NewList = List ++ Element,
		osm(MoreNodes , NewList)

    end.

%
% Show a node/element and then the children of that node.
extract_node(Node) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes, content=Content} ->
            
	    case Name of
		
		way -> 
			
			Id = get_way_id(Attributes),

			IsHighway = get_is_highway(Content), 

			case IsHighway of
	
				true -> 
			
            				List = children(Content , []),
					[ { Id , List } ];

				false ->

					ok
			end;

		_ ->
			ok
	    end;

            _ -> ok
    end.

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

		nd -> 

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

get_way_id([]) ->
    not_found;

get_way_id([Attribute | MoreAttributes]) ->
    #xmlAttribute{name=Name, value=Value} = Attribute,
    case Name of 

	id -> 
		
		list_to_atom(Value);

	_ ->

		get_way_id(MoreAttributes)
    end.


get_is_highway([]) ->
    false;

get_is_highway([Node | MoreNodes]) ->
    Element = extract_children(Node),
    case Element of

	ok ->
    		
		get_is_highway(MoreNodes);

	_ ->
        	#xmlElement{name=Name, attributes=Attributes, content=Content} = Node,

		IsHighway = get_is_highway_attributes(Attributes),

		case IsHighway of
			true -> 
			
            			IsHighway;

			false ->

				get_is_highway(MoreNodes)
		end

    end.


get_is_highway_attributes([]) ->
    false;

get_is_highway_attributes([Attribute | MoreAttributes]) ->
    #xmlAttribute{value=Value} = Attribute,
    case Value of 

	"highway" -> 
				
		true;

	_ ->

		get_is_highway_attributes(MoreAttributes)
    end.


