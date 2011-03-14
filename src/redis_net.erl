-module(redis_net).
-export([
    connect/3,
    disconnect/1,
    read_resp/1,
    read_resp/2,
    send_recv/6
]).

-define(NL, <<"\r\n">>).

connect(Ip, Port, Pass) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {keepalive, true}, {nodelay, true}]) of
        {ok, Sock} when Pass == undefined; Pass == <<>>; Pass == "" ->
            {ok, Sock};
        {ok, Sock} ->
            case redis_proto:send_auth(Sock, Pass) of
                true -> {ok, Sock};
                Err -> Err
            end;
        Err ->
            Err
    end.

disconnect(Socket) ->
    catch gen_tcp:close(Socket).

send_recv(_Socket, _Ip, _Port, _Pass, _Packet, 0) ->
    {error, closed};

send_recv(Socket, Ip, Port, Pass, Packet, Retries) when is_port(Socket) ->
    case gen_tcp:send(Socket, Packet) of
        ok ->
            case read_resp(Socket, pipelined_packet_count(Packet)) of
                {error, Err} ->
                    disconnect(Socket),
                    {error, Err};
                {redis_error, Err} ->
                    {{error, Err}, Socket};
                Reply ->
                    {Reply, Socket}
            end;
        {error, closed} ->
            disconnect(Socket),
            case connect(Ip, Port, Pass) of
                {ok, Socket1} ->
                    send_recv(Socket1, Ip, Port, Pass, Packet, Retries);
                _ ->
                    send_recv(Socket, Ip, Port, Pass, Packet, Retries-1)
            end;
        Error ->
            disconnect(Socket),
            Error
    end.

read_resp(Socket, undefined) ->
    read_resp(Socket);

read_resp(Socket, PipelinedPacketCount) ->
    read_resp(Socket, PipelinedPacketCount, []).

read_resp(_Socket, 0, Acc) ->
    lists:reverse(Acc);

read_resp(Socket, PipelinedPacketCount, Acc) ->
    read_resp(Socket, PipelinedPacketCount-1, [read_resp(Socket)|Acc]).

read_resp(Socket) ->
    inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"+", Rest/binary>>} ->
            strip_nl(Rest);
        {ok, <<"-", Rest/binary>>} ->
            {redis_error, strip_nl(Rest)};
        {ok, <<":", Rest/binary>>} ->
            Int = strip_nl(Rest),
            list_to_integer(binary_to_list(Int));
        {ok, <<"$", Size/binary>>} ->
            Size1 = list_to_integer(binary_to_list(strip_nl(Size))),
            read_body(Socket, Size1);
        {ok, <<"*", Rest/binary>>} ->
            Count = list_to_integer(binary_to_list(strip_nl(Rest))),
            read_multi_bulk(Socket, Count, []);
        {error, Err} ->
            disconnect(Socket),
            {error, Err}
    end.

read_body(_Socket, -1) ->
    undefined;

read_body(Socket, Size) ->
    inet:setopts(Socket, [{packet, raw}]),
    case gen_tcp:recv(Socket, Size + size(?NL)) of
        {error, Error} ->
            disconnect(Socket),
            {error, Error};
        {ok, <<Body:Size/binary, _/binary>>} ->
            Body
    end.

read_multi_bulk(_Socket, 0, Acc) ->
    lists:reverse(Acc);

read_multi_bulk(Socket, Count, Acc) ->
    Resp =
        case read_resp(Socket) of
            {redis_error, Err} -> {error, Err};
            Other -> Other
        end,
    read_multi_bulk(Socket, Count-1, [Resp|Acc]).

strip_nl(B) when is_binary(B) ->
    S = size(B) - size(?NL),
    <<B1:S/binary, _/binary>> = B,
    B1.

pipelined_packet_count(Packets) when is_list(Packets) ->
    length(Packets);

pipelined_packet_count(Packet) when is_binary(Packet) ->
    undefined.
