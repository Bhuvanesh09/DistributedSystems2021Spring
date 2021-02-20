-module('2018113002_2').

-export([main/2]).

main(InFile, OutFile) ->
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
    ReadOneEdge = fun(Fd) ->
                            {ok, Line3} = file:read_line(Fd),
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

    bellman_ford(Graph, N, M, S), 
    ok.

%For loop for taking input of the Graph 
for(Min, Max, Func, Graph, Fd) when Max =< Min ->
    Graph;

for(Min, Max, Func, Graph, Fd) when Max > Min ->
    %Do something here
    GraphNew = [Func(Fd) | Graph],
    for(Min+1, Max, Func, GraphNew, Fd).

initialize_map(N, S) ->
    CurMap = maps:from_list([ {I, 100000} || I <- lists:seq(1,N)]),
    maps:put(S, 0, CurMap).

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

forRelax(Min, Max, Edges, CurMap) when Max =< Min ->
    CurMap;
forRelax(Min, Max, Edges, CurMap) ->
    NewMap = relax_these_edges(Edges, CurMap),
    %Make changes here to :
        %- Divide Edges to different disjoint sets for each processor;
        %- Send Map to each one of them.
        %- They send list of updated vertices
        %- Combine the received vertices updates to make new Map. 
        %- Send to all the processes again.
    forRelax(Min+1, Max, Edges, NewMap).

bellman_ford(Graph, N, M, S) ->
    CurMap = initialize_map(N, S),
    NewMap = forRelax(1, N-1, Graph, CurMap),

    List = maps:to_list(NewMap),
    io:fwrite("~p\n", [List]).





    
