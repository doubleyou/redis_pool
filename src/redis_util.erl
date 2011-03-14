-module(redis_util).
-export([
    parse_options/1,
    parse_options/2
]).

-include_lib("../include/redis.hrl").

parse_options(Opts) ->
    parse_options(Opts, #state{}).

parse_options([], State) ->
    State;
parse_options([{ip, Ip} | Rest], State) ->
    parse_options(Rest, State#state{ip = Ip});
parse_options([{port, Port} | Rest], State) ->
    parse_options(Rest, State#state{port = Port});
parse_options([{db, Db} | Rest], State) ->
    parse_options(Rest, State#state{db = Db});
parse_options([{channel, Channel} | Rest], State) ->
    parse_options(Rest, State#state{channel = Channel});
parse_options([{callback, Callback} | Rest], State) ->
    parse_options(Rest, State#state{callback = Callback});
parse_options([{pass, Pass} | Rest], State) ->
    parse_options(Rest, State#state{pass = Pass}).
