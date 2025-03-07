-module(server).
-moudle(genserver).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = spawn(fun() -> loop(State, F) end),
    catch(unregister(ServerAtom)),
    register(ServerAtom, Pid),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom!stop,
    catch(unregister(ServerAtom)),
    ok.

loop(State, F) ->
  receive
    {request, From, Ref, Data} ->
      case catch(F(State, Data)) of
        {'EXIT', Reason} ->
          From!{exit, Ref, Reason},
          loop(State, F);
        {reply, R, NewState} ->
          From!{result, Ref, R},
          loop(NewState, F)
        end;
    {update, From, Ref, NewF} ->
      From ! {ok, Ref},
      loop(State, NewF);
    stop ->
      true
  end.

% Send a request to a Pid and wait for a response
request(Pid, Data) ->
  request(Pid, Data, 3000).

% Send a request to a Pid and wait for a response
% With a specified timeout.
% If Pid is an atom which is not registered: an "error:badarg" error is raised.
% If timeout expires: a "timeout_error" exception is thrown.
request(Pid, Data, Timeout) ->
  Ref = make_ref(),
  Pid ! {request, self(), Ref, Data},
  receive
    {result, Ref, Result} ->
      Result;
    {exit, Ref, Reason} ->
      exit(Reason)
  after Timeout ->
    throw(timeout_error)
  end.

% Update loop function
update(Pid, Fun) ->
  Ref = make_ref(),
  Pid!{update, self(), Ref, Fun},
  receive
    {ok, Ref} ->
      ok
  end.


