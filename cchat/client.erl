-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, name = Nick }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    Data = {connect, self(), St#client_st.name},
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Data),
    NewSt = St#client_st{server=ServerAtom},
    {reply, ok, NewSt} ;

%% Disconnect from server
handle(St, disconnect) ->
    Data = {disconnect, self()},
    ServerAtom = St#client_st.server,
    Response = genserver:request(ServerAtom, Data),
    NewSt = St#client_st{server=""},
    {reply, ok, NewSt} ;

% Join channel
handle(St, {join, Channel}) ->
    Data = {join, Channel, self()},
    Response = genserver:request(St#client_st.server, Data),
    {reply, ok, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->

    {reply, ok, St} ;

%% Get current nick
handle(St, whoami) ->
    Name = St#client_st.name,
    {reply, Name, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    NewSt = St#client_st{name=list_to_atom(Nick)},
    %server:handle(NewSt,{nick,Nick}),
    {reply, ok, NewSt} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
