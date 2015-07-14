-module(ircbot_plugin_learn).
-author("tdfischer@hackerbots.net").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-record(state, {brain :: any()}).
-record(var, {name :: string(), value :: string()}).

init(_Args) ->
    {ok, Brain} = brain:start_link(),
    {ok, #state{brain = Brain}}.

-spec do_learn(any(), #state{}, binary(), {string(), string()}) -> any().
do_learn(Irc, State, Channel, {Pattern, Response}) ->
    case brain:learn(State#state.brain, {Pattern, Response}) of
      {ok, new} ->
          Irc:privmsg(<<"#", Channel/binary>>, ["Okay, ", Pattern, " means ", Response]);
      {ok, appended} ->
          Irc:privmsg(<<"#", Channel/binary>>, ["Okay, ", Pattern, " also means ", Response]);
      {error, _} ->
          Irc:privmsg(<<"#", Channel/binary>>, ["I could not learn :("])
    end.

process_vars(Message, [First|Rest]) ->
    io:format("Processing against ~p~n", [["$" ++ First#var.name,
                                          First#var.value]]),
    Subst = re:replace(Message, "\\$" ++ First#var.name, First#var.value, [global,
                                                                 {return,
                                                                  list}]),
    process_vars(Subst, Rest);
process_vars(Message, []) ->
    Message.

-spec do_match(any(), #state{}, binary(), string(), any()) -> any().
do_match(Irc, State, Channel, Text, Vars) ->
    case brain:match(State#state.brain, Text) of
        {privmsg, Reply} ->
            Irc:privmsg(<<"#", Channel/binary>>, [process_vars(Reply, Vars)]);
        {action, Reply} ->
            Irc:action(<<"#", Channel/binary>>, [process_vars(Reply, Vars)]);
        none -> ok
    end.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [Sender, _User, <<"PRIVMSG">>, <<"#",Channel/binary>>, Text]} ->
            case re:run(Text, "(.*) is (.*)", [unicode, {capture, all, list}]) of
                {match, [_, Pattern, Response]} ->
                    do_learn(Ref, State, Channel, {Pattern, Response});
                _ ->
                    Vars = [#var{name="nick", value=binary_to_list(Sender)}],
                    do_match(Ref, State, Channel, binary_to_list(Text), Vars)
            end,
            {ok, State};
        _ -> {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
