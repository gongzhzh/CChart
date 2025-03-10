-module(server).
-export([start/1, stop/1]).

-record(server_st, {
    % list of nicks
    nicks,
    % list of atoms of the channels in the server
    channels
}).

initial_state() ->
    #server_st{
        nicks = [],
        channels = []
    }.
% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state(), fun requestProcess/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:request(ServerAtom, quit),
    genserver:stop(ServerAtom),
    ok.

updateNicklist(State, Nick) ->
    case lists:member(Nick, State#server_st.nicks) of
        true -> Nicks = State#server_st.nicks;
        false -> Nicks = [Nick | State#server_st.nicks]
    end.

%process join request from client
requestProcess(State, {join, Channel, ClientId, Nick}) ->
    io:format("join--- ~p~n", [Channel]),
    case lists:member(Nick, State#server_st.nicks) of
        true -> Nicks = State#server_st.nicks;
        false -> Nicks = [Nick | State#server_st.nicks]
    end,
    % check if the channel already exists
    case lists:member(Channel, State#server_st.channels) of
        true ->
            %join the channel 
            Result = genserver:request(list_to_atom(Channel), {join, ClientId}),
            case Result of
                ok -> 
                    {reply, ok, State#server_st{nicks = Nicks, channels = [Channel | State#server_st.channels]}};
                user_already_joined ->
                    {reply, user_already_joined, Nicks}
            end;
        false ->
            % if the channel is not in list, create the channel and join
            %notice the parameter type here. ClientId is a list
            genserver:start(list_to_atom(Channel), [ClientId], fun channelProcess/2),
            {reply, ok, State#server_st{nicks = Nicks, channels = [Channel | State#server_st.channels]}}
    end.

channelProcess(ClientList, {join, Client}) ->
    case lists:member(Client, ClientList) of
        true ->
            {reply, user_already_joined, ClientList};
        false ->
            {reply, ok, [Client | ClientList]}
    end.
% Channel f
% Channel

% Send a request to a Pid and wait for a response
