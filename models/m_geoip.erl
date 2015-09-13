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
    lookup/1,
    lookup/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(DbName, #m{value=undefined}=M, _Context) when is_atom(DbName) ->
    M#m{value=DbName};
m_find_value(Address, #m{value=undefined}, _Context) ->
    lookup(Address);
m_find_value(Address, #m{value=DbName}, _Context) ->
    lookup(DbName, Address).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
   undefined.

%% @doc Fetch information belonging to the ip address
lookup(Address) ->
    lookup(city, Address).

%% @doc Fetch information belonging to the ip address from a specific database.
lookup(Db, Address) when is_binary(Address) ->
    lookup(Db, z_convert:to_list(Address));
lookup(Db, Address) when is_list(Address) ->
    case inet:parse_address(Address) of
        {ok, IpAddress} ->
            lookup(Db, IpAddress);
        {error, einval} ->
            undefined
    end;
lookup(Db, IpAddress) when is_tuple(IpAddress) ->
    case geodata2:lookup(Db, IpAddress) of
        {ok, Info} ->
            make_trans_tuples(Info);
        not_found ->
            undefined;
        {error, Reason} ->
            ?DEBUG(Reason),
            undefined
    end.

%%
%% Helpers
%%

make_trans_tuples(List) ->
    make_trans_tuples(List, []).

make_trans_tuples([], Acc) ->
    Acc;
make_trans_tuples([{<<"names">>, Names} | Rest], Acc) ->
    make_trans_tuples(Rest, [{<<"name">>, make_trans_tuple(Names, [])} | Acc]);

make_trans_tuples([{K,L} | Rest], Acc) when is_list(L) ->
    T = make_trans_tuples(L),
    make_trans_tuples(Rest, [{K, T}| Acc]);
make_trans_tuples([H | Rest], Acc) ->
    make_trans_tuples(Rest, [H| Acc]).


% Transform a list of names from maxminds db to a zotonic trans tuple
make_trans_tuple([], Acc) ->
    {trans, Acc};
make_trans_tuple([{Lang, Value} | Rest], Acc) ->
    case z_trans:to_language_atom(Lang) of
        {ok, LanguageAtom} ->
            make_trans_tuple(Rest, [{LanguageAtom, Value} | Acc]);
        {error, not_a_language} ->
            % skip language
            make_trans_tuple(Rest, Acc)
    end.






