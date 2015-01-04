
-module(protocol_mux).

-behaviour(erlx_tcp_server).
-behaviour(erlx_packet_processor).


-export([start_link/1, start_link/2]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).
-export([ handle_connection/1, handle_error/3]).
-export([ init/2, handle_data/2]).


-record(state, 
    {
    }).
    

    
%%%%% ------------------------------------------------------- %%%%%


start_link(Port) ->
    erlx_tcp_server:start_link(?MODULE, Port).
    

start_link(Port, InitParams) ->
    erlx_tcp_server:start_link(?MODULE, Port, InitParams).


%%%%% ------------------------------------------------------- %%%%%


init(_InitParams) ->
    {ok, #state{}}.
    
    
init(_Socket, _InitParams) ->
    {ok, #state{}, {packet, raw}}.
    

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
    erlx_packet_processor:start_link(?MODULE, Socket, []).


%%%%% ------------------------------------------------------- %%%%%


handle_error({_Ipaddr, _Port, _UserData}, Reason, State) ->
    {stop, Reason, State}.


%%%%% ------------------------------------------------------- %%%%%


%
% looking for 1.6.4 pinglist
handle_data({raw, Bytes}, State) when byte_size(Bytes) < 3 ->
    {more, 3 - byte_size(Bytes), State};
    
    
handle_data({raw, <<16#fe01:16, 16#fa:8, Data/binary>>}, State) ->
    xerlang:trace("protocol_mux::handle_data,raw"),
    case bindecoder:sequence([ bindecoder:match_utf16_string()
                             , bindecoder:match_packet_N(fun bindecoder:ushort/1)], Data) of

        {ok, [Chan, Info], _Rest} -> 
            xerlang:trace("protocol_mux::handle_data,CHANNEL", binencoder:utf8(Chan)),

            {ok, [V, H, P], Rest2} = bindecoder:sequence([ fun bindecoder:byte/1
                                                         , bindecoder:match_utf16_string()
                                                         , fun bindecoder:ulong/1], Info),
            xerlang:trace("protocol_mux::handle_data,INFO", {V, binencoder:utf8(H), P}),
            
            {NumPlayers, MaxPlayers} = bedrock_central:get_player_counts(),
            
            Reply = binencoder:utf16_list([[16#a7, $1], "47", "1.8.1", "Description", integer_to_list(NumPlayers), integer_to_list(MaxPlayers)]),
            Size = byte_size(Reply),
            {close, <<16#ff:8, Size:16/big-unsigned-integer, Reply/binary>>, State}
        
    ;   {more, Length}      ->
            {more, Length, State}
            
    ;   {error, Reason}     ->
            {stop, {packet_error, Reason}, State}
    end;
    

handle_data({raw, Bytes}, State) ->
    % doesn't match the 1.6.4 protocol so switch to the varint packet scheme
    {keep, Bytes, State, varint};

    
%
% looking for 1.7.x/1.8.1 netty packets
handle_data({packet, <<16#00, Data/binary>> = Bytes}, State) ->
    xerlang:trace("protocol_mux::Handshake"),
    case bindecoder:sequence([ fun bindecoder:varint/1
                             , bindecoder:match_packet_N(fun bindecoder:varint/1)
                             , fun bindecoder:ushort/1
                             , fun bindecoder:varint/1], Data) of

        {ok, [Version, _, _, _], _Rest} ->
            case Version of
                5   -> {replace_callback, protocol_17x, []}
            ;   47  -> {replace_callback, protocol_181, []}
            ;   _   -> {stop, {invalid_version, Version}, State}
            end
            
        
    ;   {more, Length}      ->
            {stop, {packet_error, {short_by, Length}}, State}
            
    ;   {error, Reason}     ->
            {stop, {packet_error, Reason}, State}
    end;
    
handle_data({packet, _Bytes}, State) ->
    xerlang:trace("protocol_mux::UNKNOWN"),
    {ok, State};
    
%
% closed/error
%    
    
handle_data({closed, Bytes}, State) ->
    xerlang:trace({"protocol_mux::CLOSE", binary_to_list(Bytes)}),
    {ok, State};

    
handle_data({error, _Reason, _Bytes}, State) ->
    {ok, State}.



%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, #state{}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%


