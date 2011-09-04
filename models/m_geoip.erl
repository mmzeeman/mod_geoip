%%
%% Copyright notice
%%

-module(m_geoip).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-export([
    lookup/1
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Address, #m{value=undefined}, _Context) ->
    lookup(Address).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
   undefined.

%% @doc Fetch information belonging to the ip address
%% @spec lookup(Address) -> proplist()
lookup(Address) ->
    case egeoip:lookup_pl(Address) of 
	{error, Mess} ->
	    ?DEBUG({error, Mess, input, Address}),
	    undefined;
	PropList ->
	    PropList
    end.
