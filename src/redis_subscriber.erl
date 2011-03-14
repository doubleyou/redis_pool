-module(redis_subscriber).
-export([
    start_link/1
]).

-include_lib("../include/redis.hrl").

start_link(Opts) ->
    State = redis_util:parse_options(Opts),
    {ok, proc_lib:spawn_link(fun() -> init(State) end)}.

init(#state{channel=Channel, ip=Ip, pass=Pass, port=Port}=State) ->
    case redis_net:connect(Ip, Port, Pass) of
        {ok, Socket} ->
            Packet = redis_proto:build(["subscribe", Channel]),
            case redis_net:send_recv(Socket, Ip, Port, Pass, Packet, 1) of
                {error, Error} ->
                    {stop, Error, State};
                {_Reply, NewSocket} ->
                    loop(State#state{socket=NewSocket})
            end;
        Error ->
            {stop, Error}
    end.

loop(#state{socket=Socket, callback=Callback}=State) ->
    case redis_net:read_resp(Socket) of
        {error, _} ->
            init(State);
        [<<"message">>, _BChannel, Message] ->
            case Callback of
                {M, F} -> erlang:apply(M, F, [Message]);
                Fun -> Fun(Message)
            end
    end,
    loop(State).
