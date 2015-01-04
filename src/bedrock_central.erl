
-module(bedrock_central).

-behaviour(gen_server).
-define(SERVER, ?MODULE).


-export([start_link/0]).
-export([get_player_counts/0, get_name/0, get_description/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

         
%%%%% ------------------------------------------------------- %%%%%


-record(state,
    {
    }).

    
%%%%% ------------------------------------------------------- %%%%%


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
get_player_counts() ->
    gen_server:call(?SERVER, {get_player_counts}).
    
    
get_name() ->
    gen_server:call(?SERVER, {get_name}).


get_description() ->
    gen_server:call(?SERVER, {get_description}).
    
    
%%%%% ------------------------------------------------------- %%%%%


init(_Args) ->
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call({get_player_counts}, _From, State) ->
    {reply, {1, 10000}, State};
    
    
handle_call({get_name}, _From, State) ->
    {reply, "Bedrock0", State};


handle_call({get_description}, _From, State) ->
    {reply, "A Bedrock test server", State};    

    
handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    {stop, invalid_case_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%

