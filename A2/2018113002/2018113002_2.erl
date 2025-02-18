-module('2018113002_2').

-export([main/1]).

main(Args) ->
    [InFile, OutFile] = Args,
    {ok, Fd} = file:open(InFile, [read]),
    
    %Reading the number of processes
    {ok, Line} = file:read_line(Fd),
    [P] = lists:map(fun erlang:list_to_integer/1, 
                        lists:map(fun erlang:binary_to_list/1,    
                        re:split(string:strip(Line, right, 10), "\s+", [notempty]))),

    %Reading N and M
    {ok, Line2} = file:read_line(Fd),
    [N, M] = lists:map(fun erlang:list_to_integer/1, 
                        lists:map(fun erlang:binary_to_list/1,    
                        re:split(string:strip(Line2, right, 10), "\s+", [notempty]))), 

    %Reading all the edges
    ReadOneEdge = fun(FdNew) ->
                            {ok, Line3} = file:read_line(FdNew),
                            [X, Y, W] = lists:map(fun erlang:list_to_integer/1, 
                                                lists:map(fun erlang:binary_to_list/1,    
                                                re:split(string:strip(Line3, right, 10), "\s+", [notempty]))),
                            {X, Y, W}
                  end,

    Graph = for(0, M, ReadOneEdge, [], Fd),

    {ok, Line4} = file:read_line(Fd),
    [S] = lists:map(fun erlang:list_to_integer/1, 
                        lists:map(fun erlang:binary_to_list/1,    
                        re:split(string:strip(Line4, right, 10), "\s+", [notempty]))), 
    file:close(Fd),

    DistanceList = bellman_ford(Graph, N, M, S, P), 

    {ok, OutFd} = file:open(OutFile, [append]),
    printDistances(DistanceList, OutFd),
    ok.

printDistances([], _Fd) ->
    ok;
printDistances(DistanceList, Fd) ->
    [Cur | RestList] = DistanceList,
    {Node, Distance} = Cur,
    io:format(Fd, "~p ~p\n", [Node, Distance]),
    printDistances(RestList, Fd).

%For loop for taking input of the Graph 
for(Min, Max, _Func, Graph, _Fd) when Max =< Min ->
    Graph;

for(Min, Max, Func, Graph, Fd) when Max > Min ->
    %Do something here
    GraphNew = [Func(Fd) | Graph],
    {X, Y, W} = hd(GraphNew),
    GraphNew2 = [{Y, X, W} | GraphNew],
    for(Min+1, Max, Func, GraphNew2, Fd).

forMakeProcess(Min, Max, ProcsList) when Min >= Max ->
    ProcsList;
forMakeProcess(Min, Max, ProcsList) ->
    NewProcs = [spawn(fun subprocess_life/0) | ProcsList],
    forMakeProcess(Min+1, Max, NewProcs).


initialize_map(N, S) ->
    CurMap = maps:from_list([ {I, 100000} || I <- lists:seq(1,N)]),
    maps:put(S, 0, CurMap).

%correctWeight(From, To, Wt, CurMap) ->
    %U = maps:get(From, CurMap),
    %V = maps:get(To, CurMap),

    %if U + Wt =< V ->
            %U + Wt ;
    %true -> 
            %V 
    %end.

relax_these_edges([], _CurMap, NodeChanges) ->
    NodeChanges;
relax_these_edges(Edges, CurMap, NodeChanges) ->
    [CurEdge | RestEdges] = Edges,
    {From, To, Wt} = CurEdge,
    U = maps:get(From, CurMap),
    V = maps:get(To, CurMap),
    if U + Wt < V ->
           NewNodeCh = [{To, U + Wt} | NodeChanges];
    true -> 
           NewNodeCh = NodeChanges
    end,
    %NewMap = maps:put(To, correctWeight(From, To, Wt, CurMap), CurMap),

    relax_these_edges(RestEdges, CurMap, NewNodeCh).

updateMap([], CurMap) ->
    CurMap;
updateMap(NodeChanges, CurMap) ->
    %io:fwrite("~p\n", [NodeChanges]),
    %io:fwrite("~p\n", [CurMap]),
    [FirstCh | RestCh] = NodeChanges,
    {V, Val} = FirstCh,
    CurVal = maps:get(V, CurMap),
    if Val <  CurVal->
           NewMap = maps:put(V, Val, CurMap);
    true ->
           NewMap = CurMap
    end,

    updateMap(RestCh, NewMap).

sendData(_Edges, _CurMap, [], 0) ->
    ok;
sendData([], CurMap, ProcsList, NumProc) ->
    [CurProc | NextProcsList] = ProcsList,

    CurProc ! {self(), [], CurMap},
    sendData([], CurMap, NextProcsList, NumProc-1);
sendData(Edges, CurMap, ProcsList, NumProc) ->
    DivVal = length(Edges) div NumProc,
    
    [CurProc | NextProcsList] = ProcsList,

    if DivVal == 0 ->
           Len = 1;
    true ->
           Len = DivVal
    end,
    
    {CurList, NextList} = lists:split(Len, Edges),

    CurProc ! {self(), CurList, CurMap},
    sendData(NextList, CurMap, NextProcsList, NumProc-1).

receiveNodeChanges(0, NodeChanges) ->
    NodeChanges;
receiveNodeChanges(NumProc, NodeChanges) ->
    receive
        {[]} ->
            receiveNodeChanges(NumProc-1, NodeChanges);
        {NewChanges} ->
            AppendedChanges = NodeChanges ++ NewChanges,
            receiveNodeChanges(NumProc-1, AppendedChanges)
    end.


forRelax(Min, Max, _Edges, CurMap, _ProcsList, _NumProc) when Max =< Min ->
    CurMap;
forRelax(Min, Max, Edges, CurMap, ProcsList, NumProc) ->
    %NodeChanges = relax_these_edges(Edges, CurMap, []),
    %To Do:
    %Distribute the NodeChanges to the sub processes. DONE
    sendData(Edges, CurMap, ProcsList, NumProc),
    NodeChanges = receiveNodeChanges(NumProc, []),
    %io:fwrite("NodeChanges: ~p\n", [NodeChanges]),
    NewMap = updateMap(NodeChanges, CurMap),
    %To Do :
    %Have an updateMap(NodeChanges, CurMap) -> NewMap function. : DONE

    %Make changes here to :
        %- Divide Edges to different disjoint sets for each processor; DONE
        %- Send Map to each one of them. DONE
        %- They send list of updated vertices DONE
        %- Combine the received vertices updates to make new Map. DONE
        %- Send to all the processes again. DONE
    forRelax(Min+1, Max, Edges, NewMap, ProcsList, NumProc).

subprocess_life() ->
    receive
        {Server, [], _CurMap} ->
            %io:fwrite("Got empty list\n"),
            Server ! {[]},
            subprocess_life();
        {Server, EdgeList, CurMap} -> 
            %io:fwrite("Got the list: ~p\n", [EdgeList]),
            NodeChanges = relax_these_edges(EdgeList, CurMap, []),
            Server ! {NodeChanges},
            subprocess_life()
    end.  

%========================= OLD CODE =========================
%For the cases when the processor is only 1, I would have to use my old code. 
%Thankfully, erlang has function overloading based on prototype so I won't have to change much.
correctWeight(From, To, Wt, CurMap) ->
    U = maps:get(From, CurMap),
    V = maps:get(To, CurMap),
    if U + Wt =< V ->
        U + Wt ;
    true -> 
        V 
    end.

relax_these_edges([], CurMap) ->
    CurMap;
relax_these_edges(Edges, CurMap) ->
    [CurEdge | RestEdges] = Edges,
    {From, To, Wt} = CurEdge,
    NewMap = maps:put(To, correctWeight(From, To, Wt, CurMap), CurMap),
    relax_these_edges(RestEdges, NewMap).

forRelax(Min, Max, _Edges, CurMap) when Max =< Min ->
        CurMap;
forRelax(Min, Max, Edges, CurMap) ->
        NewMap = relax_these_edges(Edges, CurMap),
        forRelax(Min+1, Max, Edges, NewMap).
%==========================END OF OLD CODE=======================

bellman_ford(Graph, N, _M, S, P) ->
    CurMap = initialize_map(N, S),
    if P>1 ->
        ProcsList = forMakeProcess(0, P-1, []),
        NewMap = forRelax(1, N, Graph, CurMap, ProcsList, P-1);
    true ->
        NewMap = forRelax(1, N, Graph, CurMap)
    end,

    DistanceList = maps:to_list(NewMap),
    io:fwrite("~p\n", [DistanceList]),
    DistanceList.
