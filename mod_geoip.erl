%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010-2015 Channel.Me

%% Copyright 2010-2015 Channel.Me
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

-module(mod_geoip).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-mod_title("GeoIP").
-mod_description("Module which can be used to lookup geographical information given an ip address.").
-mod_prio(500).
-mod_provides([geoip]).
-mod_depends([admin]).

-export([
    observe_admin_menu/3,

    observe_module_activate/2,
    observe_module_deactivate/2
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

observe_admin_menu(admin_menu, Acc, Context) ->
    [#menu_item{id=admin_geoip,
                parent=admin_modules,
                label=?__("GeoIP", Context),
                url={admin_geoip},
                visiblecheck={acl, use, mod_geoip}}
     | Acc].

observe_module_activate(#module_activate{module=?MODULE}, _Context) ->
    application:start(geodata2),
    %% TODO: add code to download the databases.
    {ok, _} = geodata2:open_base(city, "priv/GeoLite2-City.mmdb"),
    {ok, _} = geodata2:open_base(country, "priv/GeoLite2-Country.mmdb"),
    ok;

observe_module_activate(_, _Context) ->
    ok.

observe_module_deactivate(#module_deactivate{module=?MODULE}, _Context) ->
    application:stop(geodata2);

observe_module_deactivate(_, _Context) ->
    ok.


