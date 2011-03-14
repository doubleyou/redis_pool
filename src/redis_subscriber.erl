-module(redis_subscriber).
-export([
    start_link/1
]).

-record(state, {
    ip,
    port,
    pass,
    db,
    socket,
    channel,
    callback
}).

start_link(Opts) ->
    State = #state{
        ip = proplists:get_value(ip, Opts, "127.0.0.1"),
        port = proplists:get_value(port, Opts, 6379),
        pass = proplists:get_value(pass, Opts),
        db = proplists:get_value(db, Opts, 0),
        socket = proplists:get_value(socket, Opts),
        channel = proplists:get_value(channel, Opts),
        callback = proplists:get_value(callback, Opts)
    },
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

loop(#state{socket=Socket, channel=Channel, callback=Callback}=State) ->
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
