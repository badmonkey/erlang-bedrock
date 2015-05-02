
-module(bedrock_core_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start(_StartType, _StartArgs) ->
    lager:set_loglevel(lager_console_backend, debug),
    bedrock_core_sup:start_link().


stop(_State) ->
    ok.
    
