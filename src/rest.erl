%%%--------------------------------------------------------------------------
%%% @author Onno van Zinderen Bakker
%%% @copyright 2011 Onno van Zinderen Bakker
%%% @doc Utility functions for accessing REST interfaces.
%%% @end
%%%--------------------------------------------------------------------------

-module(rest).
-author('Onno van Zinderen Bakker <onno01@chello.nl>').

-export([get_json/1]).


%%--------------------------------------------------------------------------
%% @doc Gets data from a resource that returns it in the JSON format. 
%%      The data is returned as a proplist. 
%% @spec get_json(Url::url()) -> Result::proplist()
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------
get_json(Url) ->
    case httpc:request(Url) of
        {ok, {{_, 200, "OK"}, _Headers, Body}} -> 
            {ok, normalized_json(mochijson:decode(Body))};
        {ok, {{_, StatusCode, Reason}, _Headers, _Body}} -> 
            {error, {StatusCode, Reason}};
        {error, Reason} -> 
            {error, Reason}
    end.


%%--------------------------------------------------------------------------
%% @doc Removes the struct and array atoms from a parsed json property list.
%% @spec 
%% where 
%%   
%% @end
%%--------------------------------------------------------------------------  
normalized_json({struct, L}) ->
    normalized_json(L);
    
normalized_json({array, L}) ->
    normalized_json(L);
    
normalized_json({K, V}) when is_list(K) ->
    {list_to_atom(K), normalized_json(V)};

normalized_json(L) when is_list(L) ->
    lists:map(fun normalized_json/1, L);

normalized_json(Other) -> 
    Other.
