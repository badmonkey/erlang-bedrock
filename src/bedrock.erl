-module(bedrock).

-behaviour(application).


%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:ensure_all_started(bedrock),
    lager:info("Started Bedrock server"),
    application:load(bedrock).
    
    
start(_StartType, _StartArgs) ->
    bedrock_sup:start_link().

stop(_State) ->
    ok.
