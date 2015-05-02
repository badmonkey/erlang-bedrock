
-module(bedrock_net_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start(_StartType, _StartArgs) ->
    sysconfig:load_app_config(bedrock_net),
    bedrock_net_sup:start_link().


stop(_State) ->
    ok.
    
