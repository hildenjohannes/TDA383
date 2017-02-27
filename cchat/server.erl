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

deleteUser(List, Pid) ->
    case List of
    [] -> [];
    [{P,_,_}|T] when P == Pid -> deleteUser(T,Pid);
    [F|T] -> [F | deleteUser(T,Pid)]
    end.


handle(St, {nick,Nick}) ->
    Response = Nick,
    io:fwrite("Nick set to: ~p~n", [Response]),
    {reply, Response, St};

handle(St, whoami) ->
    Response = "japp",
    io:fwrite("Your nick: ~p~n", [Response]),
    {reply, Response, St};

handle(St, {connect, Pid, Name}) ->
    case lists:filter(fun({_,_,N,_}) -> N == Name end, St#server_st.users) of
    [] ->
      OldUsers = St#server_st.users,
      NewUser = #user{pid = Pid, name = Name},
      NewSt = St#server_st{users = [NewUser | OldUsers]},
      {reply, ok, NewSt};
    _ -> 
      {reply, {error, nick_taken, "Your nick is already taken, change nick."}, St}
    end;

handle(St, {disconnect, Pid}) ->
    [User | _] = lists:filter(fun({_,P,_,_}) -> Pid == P end, St#server_st.users),
    case User#user.channel of
         [] -> NewUsers = deleteUser(St#server_st.users, Pid),
               NewSt = St#server_st{users = NewUsers},
               {reply, ok, NewSt};
          _ -> {reply, {error, leave_channels_first, "You have to leave all channels before disconnecting."}, St}
    end;


handle(St, {leave, Channel, Pid}) ->
    OldUsers = St#server_st.users,
    %get user with pid=Pid
    {[User | _], Rest} = lists:partition(fun({_,P,_,_}) -> Pid == P end, OldUsers),
    %add channel to list of channels
    case lists:partition(fun(X) -> X == Channel end, User#user.channel) of
    {[],_} -> {reply, {error, user_not_joined, "You are not in this channel"}, St};
    {_, OtherChannels} -> 
       UpdatedUser = User#user{channel = OtherChannels},
       NewSt = St#server_st{users = [UpdatedUser | Rest]},
       {reply, ok, NewSt}
    end;


handle(St, {join, Channel, Pid}) ->
    OldUsers = St#server_st.users,
    %get user with pid=Pid
    {[User | _], Rest} = lists:partition(fun({_,P,_,_}) -> Pid == P end, OldUsers),
    %add channel to list of channels
    case lists:filter(fun(X) -> X == Channel end, User#user.channel) of
    [] -> 
      UpdatedChannels = [Channel | User#user.channel],
      UpdatedUser = User#user{channel = UpdatedChannels},
      NewSt = St#server_st{users = [UpdatedUser | Rest]},
      {reply, ok, NewSt};
    _ -> {reply, {error, user_already_joined, "You are already in this channel"}, St}
    end;

handle(St, {msg_from_GUI, Channel, Name, Msg, Pid}) ->
    io:fwrite("Server trying to send msg: ~p~n", [Msg]),

    %Getting all users connected to the Channel
    Pred = fun(ChannelList) -> (lists:any(fun(X) -> X == Channel end,ChannelList)) end,
    Pred2 = fun(Usr) -> Pred(Usr#user.channel) end,
    UsersConnected = lists:filter(Pred2,St#server_st.users),

    %Remove the User who's sending from the list
    case lists:partition(fun(Usr)->Usr#user.pid == Pid end, UsersConnected) of
    {[], _} -> {reply, {error, user_not_joined, "You are not connected to this channel"}, St} ;
    {_, Users} ->
      %Start a process sending a message for each User who should receive
      Pred3 = fun(Usr) -> spawn(genserver, request, [Usr#user.pid, {incoming_msg, Channel, Name, Msg}]) end,
      lists:map(Pred3,Users),
      {reply, ok, St}
     end.
