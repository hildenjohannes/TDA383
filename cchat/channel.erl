-module(channel).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%%

% Produce initial state
initial_state(ChannelName, UserPid) ->
    #channel_st{ name = ChannelName, user_pids = [UserPid]}.


handle(St, {join, Pid}) ->
    NewSt = St#channel_st { user_pids = [ Pid | St#channel_st.user_pids]},
    {reply, ok, NewSt};

handle(St, {leave, Pid}) ->
    NewUserPids = lists:delete(Pid,St#channel_st.user_pids),
    NewSt = St#channel_st { user_pids = NewUserPids},
    {reply, ok, NewSt};

handle(St, {send, Name, Msg, Pid}) ->
    case lists:partition(fun(X) -> X == Pid end, St#channel_st.user_pids) of
    {[], _} -> %User has not joined channel
       {reply, {error, user_not_joined, "You are not connected to this channel"}, St};
    {_, Receivers} ->
       Pred = fun(P) -> spawn(genserver, request, [P, {incoming_msg, St#channel_st.name, Name, Msg}]) end,
       lists:map(Pred,Receivers),
       {reply, ok, St}
    end.
