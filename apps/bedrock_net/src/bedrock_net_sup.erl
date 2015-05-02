
-module(bedrock_net_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start_link/0, start_processor/1, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( bedrock_net_sup ).
    
    
start_processor(Args) ->
    lager:debug("Starting new processor: ~p", [Args]),
    supervisor:start_child(bedrock_net_processor_group, Args).    
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(bedrock_net_sup) ->
    Port = sysconfig:get_integer("listener.port"),
    { ok
    , { {one_for_one, 2, 5}
      , [ ?SERVICE_SPEC(client_listener, [Port])
        , ?CHILDVISOR_SPEC(bedrock_net_processor_group)
        ]
      }
    };
    

init(bedrock_net_processor_group) ->    
    { ok
    , ?GROUP_SUPERVISOR(protocol_split, 2, 5)
    }.
    
