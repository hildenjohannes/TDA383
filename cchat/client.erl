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
    undefined -> %User is not connected to a server
       {reply, {error, user_not_connected, "You are not connected to a server."}, St};
    ServerAtom ->
       case St#client_st.channels of
       [] -> %User is not connected to channels, free to leave
         Data = {disconnect, self()},
         case catch(genserver:request(ServerAtom, Data)) of
         {'EXIT', _} ->
            {reply, {error, server_not_reached, "There is no server active with that name"}, St} ;
         _ -> NewSt = St#client_st{server=undefined},
            {reply, ok, NewSt}
         end;
       _  -> %User is connected to channels
        {reply, {error, leave_channels_first, "You have to leave all channels before disconnecting."}, St}
       end
    end;

% Join channel
handle(St, {join, Channel}) ->
    case lists:filter(fun(X) -> X == Channel end, St#client_st.channels) of
    [] -> %User has not joined channel

      %Tell server to join channel
      Data = {join, Channel, self()},
      Response = genserver:request(St#client_st.server, Data),

      %Adds channel to list
      NewSt = St#client_st{channels = [Channel | St#client_st.channels]},
      {reply, Response, NewSt};
    _ -> %User has already joined channel
      {reply, {error, user_already_joined, "You are already in this channel"}, St}
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    case lists:partition(fun(X) -> X == Channel end, St#client_st.channels) of
    {[],_} -> %User not connected to channel
         {reply, {error, user_not_joined, "You are not in this channel"}, St};
    {_, Rest} -> %User connected to channel

         %Tell server to leave channel
         Data = {leave, Channel, self()},
         Response = genserver:request(St#client_st.server, Data),

         %Remove channel from list
         NewSt = St#client_st{channels = Rest},
         {reply, Response, NewSt}
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    case lists:filter(fun(X) -> X == Channel end, St#client_st.channels) of
    [] -> % User has not joined this channel
      {reply, {error, user_not_joined, "You are not connected to this channel"}, St};
    _ ->
      ChannelAtom = list_to_atom(Channel),
      genserver:request(ChannelAtom, {send, St#client_st.name, Msg, self()}),
      {reply, ok, St}
    end;

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
