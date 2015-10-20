-module(ircbot_brain).
-author("tdfischer@hackerbots.net").

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

init([]) -> {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
