
-module(minecraft_lib_app).

-behaviour(application).

-export([start/2, start/0, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application callbacks


start() ->
    application:load(minecraft_lib).
    

start(_StartType, _StartArgs) ->
    {ok, spawn(
            fun() ->
                receive
                    _ -> ok
                end
            end )}.

stop(_State) ->
    ok.
