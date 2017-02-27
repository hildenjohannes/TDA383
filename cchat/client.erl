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
    case St#client_st.server of 
    undefined ->
       ServerAtom = list_to_atom(Server), 
       case catch(genserver:request(ServerAtom, Data)) of
        {'EXIT', _} ->
          {reply, {error, server_not_reached, "There is no server active with that name"}, St} ;
        ok ->
          NewSt = St#client_st{server=ServerAtom},
          {reply, ok, NewSt} ;
        Response ->
           {reply, Response, St}
        end;
    _ -> {reply, {error, user_already_connected, "You are already connected to a server"}, St}
    end;

%% Disconnect from server
handle(St, disconnect) ->
    case St#client_st.server of 
    undefined -> {reply, {error, user_not_connected, "You are not connected to a server."}, St};
    ServerAtom -> 
       Data = {disconnect, self()},
       case catch(genserver:request(ServerAtom, Data)) of
       {'EXIT', _} ->
          {reply, {error, server_not_reached, "There is no server active with that name"}, St} ;
       Response -> 
          case Response of
          ok -> NewSt = St#client_st{server=undefined},
                        {reply, Response, NewSt};
          _ -> {reply, Response, St}
          end 
       end
    end;

% Join channel
handle(St, {join, Channel}) ->
    Data = {join, Channel, self()},
    Response = genserver:request(St#client_st.server, Data),
    {reply, Response, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    Data = {leave, Channel, self()},
    Response = genserver:request(St#client_st.server, Data),
    {reply, Response, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Data = {msg_from_GUI, Channel, St#client_st.name, Msg, self()},
    Response = genserver:request(St#client_st.server, Data),
    {reply, Response, St} ;

%% Get current nick
handle(St, whoami) ->
    Name = St#client_st.name,
    {reply, Name, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    case St#client_st.server of 
    undefined -> 
      NewSt = St#client_st{name=Nick},
      {reply, ok, NewSt} ;
    _ ->
      {reply, {error, user_already_connected, "Disconnect from the server before changing nick."}, St}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
