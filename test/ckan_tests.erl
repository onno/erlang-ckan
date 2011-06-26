-module(ckan_tests).
-author('Onno van Zinderen Bakker <onno01@chello.nl').

-include_lib("eunit/include/eunit.hrl").

packages_test() ->
    {ok, _} = ckan:packages().