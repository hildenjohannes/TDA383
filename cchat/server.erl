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

%% Connect to server
%  Pid: the pid of client process
%  Name: the name of the user connecting to the server
handle(St, {connect, Pid, Name}) ->
    case lists:filter(fun({_,_,N}) -> N == Name end, St#server_st.users) of
    [] -> %Name is not taken on server - add user to server
      NewUser = #user{pid = Pid, name = Name},
      NewSt = St#server_st{users = [NewUser | St#server_st.users]},
      {reply, ok, NewSt};
    _ -> %Name is taken - return error
      {reply, {error, nick_taken, "Your nick is already taken, change nick."}, St}
    end;

%% Disconnect from server
%  Pid: the pid of client process
handle(St, {disconnect, Pid}) ->
    NewUsers = lists:filter(fun({_,P,_}) -> Pid /= P end, St#server_st.users),
    NewSt = St#server_st{users = NewUsers},
    {reply, ok, NewSt};

%% Leave channel
%  Channel: the channel to leave
%  Pid: the pid of client process who want to leave
handle(St, {leave, Channel, Pid}) ->
    case lists:partition(fun(X) -> X == Channel end, St#server_st.channels) of
    {[],_} -> %Channel does not exist
         {reply, {error, user_not_joined, "This channel does not exist"}, St};
    {_, Rest} -> %Channel exists - let user leave channel
         ChannelAtom = list_to_atom(Channel),
         genserver:request(ChannelAtom, {leave, Pid}),
         {reply, ok, St}
    end;

%% Join channel
%  Channel: the channel to join
%  Pid: the pid of client process who want to join
handle(St, {join, Channel, Pid}) ->
    case lists:partition(fun(X) -> X == Channel end, St#server_st.channels) of
    {[],_} -> %Channel does not exist on server - create and "start" channel
      ChannelAtom = list_to_atom(Channel),
      genserver:start(ChannelAtom, channel:initial_state(Channel, Pid), fun channel:handle/2 ),
      %Add channel to list
      NewSt = St#server_st{channels = [Channel | St#server_st.channels]},
      {reply, ok, NewSt};
    _ -> %Channel exists on server - tell channel to add user
      ChannelAtom = list_to_atom(Channel),
      genserver:request(ChannelAtom, {join, Pid}),
      {reply, ok, St}
    end;

%% Send job to clients
%  F: Function to calculate
%  Parameters: Parameters to function
%  Sender: Sender who will receive result of job
handle(St, {send_job, F, Parameters, Sender}) ->
  Pids = lists:map(fun({_,P,_}) -> P end, St#server_st.users), %get connected client pids
  Tasks = assign_tasks(Pids, Parameters),
  TasksWithRef = lists:map(fun({Pid,Task}) -> {make_ref(), Pid, Task} end, Tasks), %add ref to each task
  spawn(fun() ->
    Me = self(),
    lists:map(fun({Ref,Pid,Task}) -> %send each job to a client
      spawn(fun() ->
        Me ! genserver:request(Pid, {send_job, {Ref, F, Task}}, infinity)
      end) end, TasksWithRef),
    Jobs = gather(TasksWithRef),
    Sender ! Jobs
  end),
  {reply, ok, St}.

%% Gather all jobs done by clients
%  TasksWithRef: Tasks with reference
gather(TasksWithRef) ->
  [receive {Ref, Result} -> Result end || {Ref, _, _} <- TasksWithRef].

%% Assign tasks to users
%  Users: List of pids of clients
%  Tasks: List of tasks to do
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
