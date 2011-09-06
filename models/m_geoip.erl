%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010-2011 Channel.Me

%% Copyright 2010-2011 Channel.Me
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.%%

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
