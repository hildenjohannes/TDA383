% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(client_st, {gui, name, server, channels=[]}).

% This record defines the structure of a user matching a pid with a username and a potential channel.
-record(user, {pid,name}).

-record(channel, {name,user_pids=[]}). %TODO: Maybe doesn't need user_pids

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {server,users=[],channels=[]}).

-record(channel_st, {name,user_pids=[]}).
