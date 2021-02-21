-module('2018113002_1').
-import(string, [concat/2]).

-export([main/1]).

%main(NumProc, TokenValue) ->
main(Args) ->
    [InFile, OutFile] = Args,
    {ok, Fh} = file:open(InFile, [read]),
    {ok, Words} = file:read_line(Fh),
    [NumProc, TokenValue] = lists:map(fun erlang:list_to_integer/1, 
                    lists:map(fun erlang:binary_to_list/1, 
                    re:split(string:strip(Words, right, 10), "\s+", [notempty]))),

    Procs = for(0, NumProc-1, []),
    SelfAtLast = tl(Procs) ++ [self()],
    ProcsList = [self()] ++ Procs,
    io:fwrite("~p\n~p\n~p\n", [SelfAtLast, ProcsList, TokenValue]),
    Next = hd(Procs),
    Next ! {self(), TokenValue, SelfAtLast, ProcsList, OutFile},
    life().


for(Min, Max, Procs) when Max =< Min ->
    Procs;

for(Min, Max, Procs) when Max > Min ->
    %Do something here

    NewProcs = Procs ++ [spawn_process()],
    for(Min+1, Max, NewProcs).

spawn_process() ->
    spawn(fun life/0).

life() ->
    %main life of the process. Waits and sends a message here.
    %
    receive
        {Sender, TokenValue, [], ProcsList, OutFile} ->
            SendId = string:str(ProcsList, [Sender]) - 1,
            SelfId = string:str(ProcsList, [self()]) - 1,
            {ok, Fh} = file:open(OutFile, [append]),
            io:format(Fh,"Process ~p received TokenValue ~p from ~p\n", [SelfId, TokenValue, SendId]),
            ok;
        {Sender ,TokenValue, Procs, ProcsList, OutFile} ->
            SendId = string:str(ProcsList, [Sender]),
            SelfId = string:str(ProcsList, [self()]),
            {ok, Fh} = file:open(OutFile, [append]),
            io:format(Fh,"Process ~p received TokenValue ~p from ~p\n", [SelfId, TokenValue, SendId]),
            ListLeft = tl(Procs),
            Next = hd(Procs),
            Next ! {self(), TokenValue, ListLeft, ProcsList, OutFile},
            life()
    end.





