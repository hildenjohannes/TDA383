-module(channel).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%%

% Produce initial state
initial_state(ChannelName, UserPid) ->
    #channel_st{ name = ChannelName, user_pids = [UserPid]}.

%%  Join a channel
%   Pid: the pid of the client process
handle(St, {join, Pid}) ->
    NewSt = St#channel_st { user_pids = [ Pid | St#channel_st.user_pids]},
    {reply, ok, NewSt};

%%  Leave a channel
%   Pid: the pid of the client process
handle(St, {leave, Pid}) ->
    NewUserPids = lists:delete(Pid,St#channel_st.user_pids),
    NewSt = St#channel_st { user_pids = NewUserPids},
    {reply, ok, NewSt};

%%  Send a message to all subscribers of a channel
%   Name: The name of the user/client sending the message
%   Msg: The message to be sent
%   Pid: the pid of the client process
handle(St, {send, Name, Msg, Pid}) ->
    case lists:partition(fun(X) -> X == Pid end, St#channel_st.user_pids) of
    {_, Receivers} -> %Send message to every receiver. Starts a process for each message sent.
       Pred = fun(P) -> spawn(genserver, request, [P, {incoming_msg, St#channel_st.name, Name, Msg}]) end,
       lists:map(Pred,Receivers),
       {reply, ok, St}
    end.
