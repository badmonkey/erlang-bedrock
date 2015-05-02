
-module(client_listener).

-behaviour(tcp_listener).


-export([start_link/1, start_link/2]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).
-export([ handle_connection/3, handle_error/3]).


-record(state, 
    {
    }).
    

    
%%%%% ------------------------------------------------------- %%%%%


start_link(Port) ->
    lager:info("Starting client_listener: ~p", [Port]),
    tcp_listener:start_link(?MODULE, Port).
    

start_link(Port, InitParams) ->
    tcp_listener:start_link(?MODULE, Port, InitParams).


%%%%% ------------------------------------------------------- %%%%%


init(_InitParams) ->
    {ok, #state{}}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_connection({_Local, _Remote, Socket}, _UserData, _State) ->
    bedrock_net_sup:start_processor(Socket).


%%%%% ------------------------------------------------------- %%%%%


handle_error({_Endpoint, _UserData}, Reason, State) ->
    {stop, Reason, State};
    

handle_error({_Local, _Remote, _Socket}, Reason, State) ->
    {stop, Reason, State}.


%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%


