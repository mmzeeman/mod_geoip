%%
%%
%%

-module(mod_geoip).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").
-behaviour(gen_server).

-mod_title("GeoIP").
-mod_description("Module which can be used to lookup geographical information given an ip address.").
-mod_prio(500).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").

-record(state, {}).

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    process_flag(trap_exit, true),
    zotonic:ensure_started(egeoip),
    {ok, #state{}}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    application:stop(egeoip).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
