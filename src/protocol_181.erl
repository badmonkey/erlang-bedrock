
-module(protocol_181).

-behaviour(packet_processor).


-export([start_link/2]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).
-export([ init/2, handle_data/2]).


%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    { mode      = mc_handshake
    , socket    = undefined
    , zhandle   = undefined
    , threshold = 0
    }).
    
    
%%%%% ------------------------------------------------------- %%%%%


start_link(Socket, Args) ->
    packet_processor:start_link(?MODULE, Socket, Args).


%%%%% ------------------------------------------------------- %%%%%


init(_InitParams) ->
    {ok, #state{}}.
    
    
init(Socket, _InitParams) ->
    {ok, #state{socket = Socket, zhandle = zlib:open()}, {packet, varint}}.
    

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


%
% mode handshake
%

handle_data({packet, <<16#00, Data/binary>>}, #state{mode = mc_handshake} = State) ->
    xerlang:trace("protocol_181::Handshake"),
    case bindecoder:sequence([ fun bindecoder:varint/1
                             , bindecoder:match_packet_N(fun bindecoder:varint/1)
                             , fun bindecoder:ushort/1
                             , fun bindecoder:varint/1], Data) of

        {ok, [Version, _Address, _Port, NextMode], _Rest}
                when Version =:= 47 ->
            case NextMode of
                1 -> {ok, State#state{mode = mc_status}}
            ;   2 -> {ok, State#state{mode = mc_login}}
            ;   _ -> {stop, {invalid_mode, NextMode}, State}
            end
            
    ;   {ok, [Version, _Address, _Port, _NextMode], _Rest} ->
            {stop, {invalid_version, Version}, State}
        
    ;   {more, Length}      ->
            {stop, {packet_error, {short_by, Length}}, State}
            
    ;   {error, Reason}     ->
            {stop, {packet_error, Reason}, State}
    end;
    
    
handle_data({packet, _Data}, #state{mode = mc_handshake} = State) ->
    % ignore unknown handshake packets
    {ok, State};

    
%
% mode status
%

handle_data({packet, <<16#00>>}, #state{mode = mc_status} = State) ->
    xerlang:trace("Status:Request"),
    {NumPlayers, MaxPlayers} = bedrock_central:get_player_counts(),
    
    JsonReply = [{<<"version">>,
                    [{<<"name">>, list_to_binary(bedrock_central:get_name())},
                     {<<"protocol">>, 47}] },
                 {<<"players">>,
                    [{<<"max">>, MaxPlayers},
                     {<<"online">>, NumPlayers}] },
                 {<<"description">>,
                    [{<<"text">>, list_to_binary(bedrock_central:get_description())}] }
                ],
                    
    ReplyData = binencoder:json(JsonReply),
    xerlang:trace( binary_to_list(ReplyData) ),
    
    {reply, make_packet(16#00, ReplyData), State};

    
handle_data({packet, <<16#01, _Time:64/big-signed-integer>> = Bytes}, #state{mode = mc_status} = State) ->
    xerlang:trace("Status:Ping"),
    {close, xerlang:trace(make_packet(Bytes)), State};

    
handle_data({packet, _Bytes}, #state{mode = mc_status} = State) ->
    xerlang:trace("Status:Unknown"),
    {ok, State};
   
   
%
% mode login
%
   
handle_data({packet, _Bytes}, #state{mode = mc_login} = State) ->
    {ok, State};


%
% mode play
%

handle_data({packet, _Bytes}, #state{mode = mc_play} = State) ->
    {ok, State};

    
%
% closed/error
%    
    
handle_data({closed, Bytes}, State) ->
    xerlang:trace({"protocol_181::CLOSE", binary_to_list(Bytes)}),
    {ok, State};

    
handle_data({error, _Reason, _Bytes}, State) ->
    {ok, State}.



%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, #state{}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%


make_packet(Bytes) when is_binary(Bytes) ->
    binencoder:buffer(varint, <<Bytes/binary>>).
    
make_packet(Id, Bytes) when is_integer(Id), is_binary(Bytes) ->
    Header = binencoder:varint(Id),
    binencoder:buffer(varint, <<Header/binary, Bytes/binary>>).

    