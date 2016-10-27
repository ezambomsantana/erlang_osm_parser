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
    
    {DocNode, _Misc1} = xmerl_scan:file(Infilename),
    ListLatLon = init_node(DocNode),

    {DocWays, _Misc2} = xmerl_scan:file(Infilename),
    init_way( DocWays , ListLatLon ).

%% get the nodes

% read the OSM tag and extract all children
init_node(Node) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		osm -> 

			List = osm_node(Content , []),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.


osm_node([], List) ->
    List;

osm_node([Node | MoreNodes], List) ->
    Element = extract_node_node(Node),
    case Element of

	ok ->
    		
		osm_node(MoreNodes , List);

	_ ->
		NewList = List ++ Element,
		osm_node(MoreNodes , NewList)

    end.

%
% Show a node/element and then the children of that node.
extract_node_node( Node ) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes } ->
            
	    case Name of
		
		node -> 
			
			Id = get_node_attribute( Attributes , id ),

			Lat = get_node_attribute( Attributes , lat ),

			Lon = get_node_attribute( Attributes , lon ),

			Value = { Id , Lat , Lon },

			[ { Id , Value } ];

		_ ->
			ok
	    end;

            _ -> ok
    end.

get_node_attribute([] , _AttributeType) ->
    not_found;

get_node_attribute([Attribute | MoreAttributes] , AttributeType) ->
    #xmlAttribute{name=Name, value=Value} = Attribute,
    case Name of 

	AttributeType -> 
		
		list_to_atom(Value);

	_ ->

		get_node_attribute(MoreAttributes , AttributeType)
    end.





%% get the ways

% read the OSM tag and extract all children
init_way(Node , ListLatLon) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		osm -> 

			List = osm(Content , [] , ListLatLon),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

osm([], List , _ListLatLon) ->
    List;

osm([Node | MoreNodes], List , ListLatLon) ->
    Element = extract_node(Node , ListLatLon),
    case Element of

	ok ->
    		
		osm(MoreNodes , List , ListLatLon);

	_ ->
		NewList = List ++ Element,
		osm(MoreNodes , NewList , ListLatLon)

    end.

%
% Show a node/element and then the children of that node.
extract_node( Node , ListLatLon ) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes, content=Content} ->
            
	    case Name of
		
		way -> 
			
			Id = get_way_id(Attributes),

			IsHighway = get_is_highway(Content), 

			case IsHighway of
	
				true -> 
			
            				List = children(Content , [] , ListLatLon),
					[ { Id , List } ];

				false ->

					ok
			end;

		_ ->
			ok
	    end;

            _ -> ok
    end.

children([], List , _ListLatLon) ->
    List;

children([Node | MoreNodes], List , ListLatLon) ->
    Element = extract_children(Node, ListLatLon),
    case Element of

	ok ->
    		
		children(MoreNodes , List , ListLatLon);

	_ ->
		NewList = List ++ Element,
		children(MoreNodes , NewList , ListLatLon)

    end.


extract_children(Node , ListLatLon) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		tag -> 

			List = get_attributes_tag(Attributes , []),
			[ { Name , List } ];

		nd -> 

			List = get_attributes_nd(Attributes , [] , ListLatLon ),
			[ { Name , List } ];

		_ -> ok

	    end;
            _ -> ok
    end.

get_attributes_tag([] , List) ->
    List;

get_attributes_tag([Attribute | MoreAttributes], List) ->
    #xmlAttribute{name=Name, value=Value} = Attribute,
    NewList = List ++ [ { Name, Value } ],
    get_attributes_tag(MoreAttributes , NewList).

%get the attributes of the way nodes ( id, lat, and lon)
get_attributes_nd([] , List , _ListLatLon) ->
    List;

get_attributes_nd([Attribute | MoreAttributes], List , ListLatLon) ->
    #xmlAttribute{name=Name, value=Value} = Attribute,
    { Lat , Lon } = case Name of

	ref -> 

	    La = get_coordinates( list_to_atom(Value) , lat , ListLatLon ), 
	    Lo = get_coordinates( list_to_atom(Value) , lat , ListLatLon ), 
	    { La , Lo }

    end,

    NewList = List ++ [ { Name, Value , { Lat , Lon } } ],
    get_attributes_nd(MoreAttributes , NewList , ListLatLon).

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

get_coordinates( _Name , _Type , [] ) ->
    ok;

get_coordinates( Name , Type , [ Head | MoreLatLon ] ) ->
    
    Node = element ( 1 , Head ),
    case Name of 

	Node -> 
		case Type of
			lat ->
				element( 2 , Head );
			lon ->
				element( 3 , Head )
		end;
	_ ->
		get_coordinates( Name , Type , MoreLatLon )
    end.

get_is_highway([]) ->
    false;

get_is_highway([Node | MoreNodes]) ->
    Element = extract_children_highway(Node),
    case Element of

	ok ->
    		
		get_is_highway(MoreNodes);

	_ ->
        	#xmlElement{attributes=Attributes} = Node,

		IsHighway = get_is_highway_attributes(Attributes),

		case IsHighway of
			true -> 
			
            			IsHighway;

			false ->

				get_is_highway(MoreNodes)
		end

    end.

extract_children_highway(Node) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		tag -> 

			List = get_attributes_tag(Attributes , []),
			[ { Name , List } ];

		_ -> ok

	    end;
            _ -> ok
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


