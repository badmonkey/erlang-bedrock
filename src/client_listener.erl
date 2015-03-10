
-module(client_listener).

-behaviour(tcp_listener).


-export([start_link/1, start_link/2]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).
-export([ handle_connection/1, handle_error/3]).


-record(state, 
    {
    }).
    

    
%%%%% ------------------------------------------------------- %%%%%


start_link(Port) ->
    tcp_listener:start_link(?MODULE, Port).
    

start_link(Port, InitParams) ->
    tcp_listener:start_link(?MODULE, Port, InitParams).


%%%%% ------------------------------------------------------- %%%%%


init(_InitParams) ->
    {ok, #state{}}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, State) ->
    {stop, {bad_call_request, Request, From}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_cast(Msg, State) ->
    {stop, {bad_cast_request, Msg}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_info(Info, State) ->
    {stop, {bad_info_request, Info}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_connection({_Ipaddr, _Port, Socket, _UserData}) ->
    protocol_split:start_link(Socket, []).


%%%%% ------------------------------------------------------- %%%%%


handle_error({_Ipaddr, _Port, _UserData}, Reason, State) ->
    {stop, Reason, State}.


%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, #state{}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%


