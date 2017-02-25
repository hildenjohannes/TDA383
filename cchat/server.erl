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
    OldUsers = St#server_st.users,
    NewUser = #user{pid = Pid, name = Name},
    io:fwrite("Old: ~p~n", [OldUsers]),
    io:fwrite("New: ~p~n", [NewUser]),
    NewSt = St#server_st{users = [NewUser | OldUsers]},
    {reply, ok, NewSt};

handle(St, {disconnect, Pid}) ->
    NewUsers = deleteUser(St#server_st.users, Pid),
    NewSt = St#server_st{users = NewUsers},
    {reply, ok, NewSt};

handle(St, {join, Channel, Pid}) ->
    OldUsers = St#server_st.users,
    io:fwrite("Users: ~p~n", [OldUsers]),
    %get user with pid=Pid
    {[User | _], Rest} = lists:partition(fun({_,P,_,_}) -> Pid == P end, OldUsers),
    io:fwrite("User found: ~p~n", [User]),
    %add channel to list of channels
    UpdatedChannels = [Channel | User#user.channel],
    UpdatedUser = User#user{channel = UpdatedChannels},
    NewSt = St#server_st{users = [UpdatedUser | Rest]},
    io:fwrite("NewSt: ~p~n", [NewSt]),
    {reply, ok, NewSt};

handle(St, {msg_from_GUI, Channel, Name, Msg, Pid}) ->
    io:fwrite("Server trying to send msg: ~p~n", [Msg]),

    %Getting all users connected to the Channel
    Pred = fun(ChannelList) -> (lists:any(fun(X) -> X == Channel end,ChannelList)) end,
    Pred2 = fun(Usr) -> Pred(Usr#user.channel) end,
    UsersConnected = lists:filter(Pred2,St#server_st.users),

    %Remove the User who's sending from the list
    Users = lists:filter(fun(Usr)->Usr#user.pid /= Pid end, UsersConnected),

    %Start a process sending a message for each User who should receive
    Pred3 = fun(Usr) -> spawn(genserver, request, [Usr#user.pid, {incoming_msg, Channel, Name, Msg}]) end,
    lists:map(Pred3,Users),
    {reply, ok, St}.
