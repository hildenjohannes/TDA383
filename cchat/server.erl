-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{ server = ServerName }.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Pid, Name}) ->
    case lists:filter(fun({_,_,N}) -> N == Name end, St#server_st.users) of
    [] ->
      NewUser = #user{pid = Pid, name = Name},
      NewSt = St#server_st{users = [NewUser | St#server_st.users]},
      {reply, ok, NewSt};
    _ ->
      {reply, {error, nick_taken, "Your nick is already taken, change nick."}, St}
    end;

handle(St, {disconnect, Pid}) ->
    NewUsers = lists:filter(fun({_,P,_}) -> Pid /= P end, St#server_st.users),
    NewSt = St#server_st{users = NewUsers},
    {reply, ok, NewSt};


handle(St, {leave, Channel, Pid}) ->
    case lists:partition(fun(X) -> X == Channel end, St#server_st.channels) of
    {[],_} -> %Channel does not exist
         {reply, {error, user_not_joined, "This channel does not exist"}, St};
    {_, Rest} ->
         %let user leave channel
         ChannelAtom = list_to_atom(Channel),
         genserver:request(ChannelAtom, {leave, Pid}),
         {reply, ok, St}
    end;

handle(St, {join, Channel, Pid}) ->
    case lists:partition(fun(X) -> X == Channel end, St#server_st.channels) of
    {[],_} -> %Channel does not exist on server

      %create and "start" channel
      ChannelAtom = list_to_atom(Channel),
      genserver:start(ChannelAtom, channel:initial_state(Channel, Pid), fun channel:handle/2 ),

      %Adds channel to list
      NewSt = St#server_st{channels = [Channel | St#server_st.channels]},
      {reply, ok, NewSt};
    _ -> %Channel exists on server
      %Tells channel to add user
      ChannelAtom = list_to_atom(Channel),
      genserver:request(ChannelAtom, {join, Pid}),
      {reply, ok, St}
    end.
