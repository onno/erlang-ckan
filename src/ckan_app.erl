%%%--------------------------------------------------------------------------
%%% @author Onno van Zinderen Bakker
%%% @copyright 2011 Onno van Zinderen Bakker
%%% @doc Callbacks for the ckan application.
%%% @end
%%%--------------------------------------------------------------------------

-module(ckan_app).

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ckan.
start(_Type, _StartArgs) ->
    ok.
    
%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ckan.
stop(_State) ->
    ok.