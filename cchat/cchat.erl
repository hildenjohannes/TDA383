% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

%% Sent job to clients of a server
%  Server: Server whos clients will do jobs
%  F: Function to calculate
%  Parameters: Parameters to function
send_job(Server, F, Parameters) ->
  Pids = genserver:request(list_to_atom(Server), get_pids), %get connected client pids from server
  Tasks = assign_tasks(Pids, Parameters),
  Tasks_Ref = lists:map(fun({Pid,Task}) -> {make_ref(), Pid, Task} end, Tasks), %add ref to each task
  Me = self(),
  lists:map(fun({Ref,Pid,Task}) -> %send each job to a client
    spawn(fun() ->
      Me ! genserver:request(Pid, {send_job, {Ref, F, Task}}, infinity)
    end) end, Tasks_Ref),
  gather(Tasks_Ref).

%% Gather all jobs done by clients
%  Tasks_Ref: Tasks with reference
gather(Tasks_Ref) ->
  [receive {Ref, Result} -> Result end || {Ref, _, _} <- Tasks_Ref].

%% Assign tasks to users
%  Users: List of pids of clients
%  Tasks: List of tasks to do
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
