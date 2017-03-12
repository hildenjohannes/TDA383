% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   name: nick of the user
%   server: name of server
%   channels: list of names of channels connected to
-record(client_st, {gui, name, server, channels=[]}).

% This record defines the structure of a user matching a pid with a username and a potential channel.
% It contains the following fields:
%   pid: the pid of the client process
%   name: nick of the user
-record(user, {pid,name}).

% This record defines the structure of the server process.
% It contains the following fields:
%   server: name of server
%   users: users connected to server
%   channels: list of names of channels that exists
-record(server_st, {server,users=[],channels=[]}).

% This record defines the structure of a channel process.
% It contains the following fields:
%   name: name of the channel
%   user_pids: the pids of the clients connected to the channel
-record(channel_st, {name,user_pids=[]}).
