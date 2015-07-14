-module(brain).
-author("tdfischer@hackerbots.net").

-behaviour(gen_server).
-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([match/2, learn/2]).

-record(response, {pattern :: string(), 
                   replies=[] :: [string()]}).
-record(state, {responses=[] :: [#response{}]}).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

init([]) -> {ok, #state{}}.

-spec match(any(), string()) -> any().
match(Ref, Text) ->
    gen_server:call(Ref, {match, Text}).
-spec learn(any(), {string(), string()}) -> any().
learn(Ref, {Pattern, Response}) ->
    gen_server:call(Ref, {learn, {Pattern, Response}}).

append_response(State, Pattern, Response) ->
    NewState = State#state{responses =
                           [#response{pattern=Pattern,replies=[Response]}|
                            State#state.responses]},
    {{ok, appended}, NewState}.

handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call({match, Text}, _From, State) ->
    case find_match(Text, State#state.responses) of
      none ->
        {reply, none, State};
      "<reply>" ++ Response ->
        {reply, {privmsg, [Response]}, State};
      "<action>" ++ Response ->
        {reply, {action, [Response]}, State};
      Response ->
        {reply, {privmsg, [Text, " is ", Response]}, State}
    end;
handle_call({learn, {Pattern, Response}}, _From, State) ->
    {Reply, NewState} = append_response(State, Pattern, Response),
    {reply, Reply, NewState};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

-spec find_match(string(), [any()]) -> string() | none.
find_match(Text, [FirstResponse|More]) ->
    case re:run(Text, FirstResponse#response.pattern, [unicode]) of
        {match, _} ->
            [Resp|_] = FirstResponse#response.replies,
            Resp;
        _ ->
            find_match(Text, More)
    end;
find_match(_Text, []) ->
    none.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
