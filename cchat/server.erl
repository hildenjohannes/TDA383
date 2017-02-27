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
      OldUsers = St#server_st.users,
      NewUser = #user{pid = Pid, name = Name},
      NewSt = St#server_st{users = [NewUser | OldUsers]},
      {reply, ok, NewSt};
    _ ->
      {reply, {error, nick_taken, "Your nick is already taken, change nick."}, St}
    end;

handle(St, {disconnect, Pid}) ->
    Pred = fun(PidList) -> (lists:any(fun(X) -> X == Pid end,PidList)) end,
    Pred2 = fun(Chn) -> Pred(Chn#channel.user_pids) end,
    ChannelsConnected = lists:filter(Pred2,St#server_st.channels),
    case ChannelsConnected of
         [] -> NewUsers = lists:filter(fun({_,P,_}) -> Pid /= P end, St#server_st.users),
               NewSt = St#server_st{users = NewUsers},
               {reply, ok, NewSt};
          _ -> {reply, {error, leave_channels_first, "You have to leave all channels before disconnecting."}, St}
    end;


handle(St, {leave, Channel, Pid}) ->
    case lists:partition(fun(X) -> X#channel.name == Channel end, St#server_st.channels) of
    {[],_} -> %Channel does not exist
         {reply, {error, user_not_joined, "This channel does not exist"}, St};
    {[Ch | _], Rest} ->

         %TODO: Maybe do this check in channel.erl

         case lists:partition(fun(X) -> X == Pid end, Ch#channel.user_pids) of
         {[],_} -> %Channel exists, user not joined
            {reply, {error, user_not_joined, "You are not in this channel"}, St};
         {_,UserPids}-> %Channel exists, user has joined
           %let user leave channel
           ChannelAtom = list_to_atom(Channel),
           genserver:request(ChannelAtom, {leave, Pid}),


            NewChannel = Ch#channel{user_pids = UserPids},
            NewSt = St#server_st{channels = [NewChannel | Rest]},
            {reply, ok, NewSt}
         end
    end;

handle(St, {join, Channel, Pid}) ->
    case lists:partition(fun(X) -> X#channel.name == Channel end, St#server_st.channels) of
    {[],_} -> %Channel does not exist on server
      %create channel
      ChannelAtom = list_to_atom(Channel),
      genserver:start(ChannelAtom, channel:initial_state(Channel, Pid), fun channel:handle/2 ),

      NewChannel = #channel{ name = Channel, user_pids=[Pid] },
      UpdatedChannels = [NewChannel | St#server_st.channels],
      NewSt = St#server_st{channels = UpdatedChannels},
      {reply, ok, NewSt};
    {[Ch | _], Rest} ->

      %TODO: Maybe do this check in channel.erl

      case lists:filter(fun(X) -> X == Pid end, Ch#channel.user_pids) of
      [] -> %Channel exists, user not joined
         ChannelAtom = list_to_atom(Channel),
         genserver:request(ChannelAtom, {join, Pid}),

         NewChannel = Ch#channel{user_pids = [Pid | Ch#channel.user_pids]},
         NewSt = St#server_st{channels = [NewChannel | Rest]},
         {reply, ok, NewSt};
      _ -> %Channel exists, user already joined
         {reply, {error, user_already_joined, "You are already in this channel"}, St}
      end
    end;

handle(St, {msg_from_GUI, Channel, Name, Msg, Pid}) ->
    io:fwrite("Server trying to send msg: ~p~n", [Msg]),
    case lists:filter(fun(X) -> X#channel.name == Channel end, St#server_st.channels) of
    [] -> %Channel does not exist
        {reply, {error, user_not_joined, "This channel does not exist"}, St} ;
    [Ch | _] ->

        %TODO: Maybe do this check in channel.erl

        case lists:partition(fun(X) -> X == Pid end, Ch#channel.user_pids) of
        {[], _} -> %Channel exist, user has not joined
           {reply, {error, user_not_joined, "You are not connected to this channel"}, St};
        {_, Receivers} ->
           Pred = fun(P) -> spawn(genserver, request, [P, {incoming_msg, Channel, Name, Msg}]) end,
           lists:map(Pred,Receivers),
           {reply, ok, St}
        end
     end.
