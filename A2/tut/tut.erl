-module(tut).

-import(string, [len/1, concat/2]).

-export([main/0]).

main() ->
    io:fwrite("Ori ID: ~p", [self()]),
    spawner(),
    spawner().
    

atom_stuff() ->
    anAtom.

compare(A, B) ->
    A =:= B,
    A =/= B,
    A == B,
    A /= B,
    A > B,
    A < B,
    A >= B,
    A =< B,
    A and B, 
    A or B,
    not A,
    A xor B.

preschool() ->
    'Go to preschool'.

gradeschool() ->
    'Go to gradeschool'.

what_grade(X) ->
    if X < 5 -> preschool()
    ; X >= 5 -> gradeschool()
    end.

say_hello(X) ->
    case X of
        french -> 'Bonjour';
        german -> 'Guren Tag';
        english -> 'Hello World'
    end.


string_stuff() ->
    Str1 = "Random String",
    Str2 = "Another String",

    io:fwrite("String : ~p ~p\n", [Str1, Str2]),

    Str3 = io_lib:format("It is a ~s and ~s\n", [Str1, Str2]),
    io:fwrite(Str3).

tuple_stuff() ->
    My_Data = {1, 2, 3, 4},
    
    My_Data2 = {height, 6.25},
    {height , Ht} = My_Data2,
    Ht.

list_stuff() ->
    List1 = [1, 23, 4, 5],
    List2 = [4, 5, 6],
    List3 = List1 ++ List2, %Union of the two lists.

    hd(List3),
    tl(List3),

    List5 = [3 | List3],

    [Head|Tail] = List5,
    Head.

lc_stuff() ->
    % List Comprehension
    List1 = [1, 2, 3],
    List2 = [2*N || N <- List1],
    List2,
    
    List3 = [1, 2, 3, 4],
    List4 = [N || N <- List3, N rem 2 == 0],
    List4,

    City_Weather = [{pittsburgh, 50}, {'New York', 53}, {miami, 78}],

    Great_Temp = [{City, Temp} || {City, Temp} <- City_Weather, Temp >= 65],
    Great_Temp.

type_stuff() ->
    is_atom(name).

factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

find_factorial(X) ->
    Y = factorial(X),
    io:fwrite("Factorial : ~p\n", [Y]).

sum([], Sum) -> Sum;
sum([H|T], Sum) -> 
    io:fwrite("Sum: ~p \n", [Sum]),
    sum(T, H + Sum).


% How to write for loop

for(Min, Max) when Max =< Min ->
    ok;

for(Min, Max) when Max > Min ->
    %Do something here
    io:fwrite("Number here : ~p\n", [Min]),
    for(Min+1, Max).


%Higher order function

double(X) -> X * 2.
triple(X) -> X * 3.


do_math(List) ->
    lists:map(fun double/1, List).


write_txt(N) ->
    {ok, Fh} = file:open("myFile.txt", [write]),
    file:write(Fh, N).

append_txt(N) ->
    {ok, Fh} = file:open("myFile.txt", [append]),
    file:write(Fh, N).

read_txt(N) ->
    {ok, Fh} = file:open("myFile.txt", [read]),
    Words = file:read(Fh, 1024*1024),
    io:fwrite("~p\n", [Words]).

%Exception Handling:
%

error_stuff(N) ->
    try
        Ans = 2/N,
        Ans
    catch
        error:badarith ->
            "Can't divide by zero"
    end.

read_txt2() -> 
    try 
        {ok, File} = file:open("Nonsense File", [read]),
        Words = file:read(File, 1024*1024),
        io:fwrite("~p\n", [Words])
    catch
        _:_ ->
            "File Doesn't exist"
    end.

%Concurrency
%


get_id(M) ->
    io:fwrite("ID: ~p\n",[M]).

newFunc() ->
    MyId = self(),
    io:fwrite("My id is : ~p", [MyId]),
    for(1, 10).

spawner() ->
    spawn(fun newFunc/0).


