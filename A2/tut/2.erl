-module('2').

-export([main/0]).

main() ->
    Func = fun(In) -> 
                   io:fwrite("I have got ~p\n", [In]),
                   io:fwrite("Hah Ha sasldf ~p\n", [In*100]) 
           end,
    for(0, 10, Func),
    ok.


for(Min, Max, Func) when Max =< Min ->
    ok;

for(Min, Max, Func) when Max > Min ->
    %Do something here
    Func(Min),
    for(Min+1, Max, Func).
