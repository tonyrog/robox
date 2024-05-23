%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Persisten queue handling functions
%%% @end
%%% Created : 22 Apr 2024 by Tony Rogvall <tony@rogvall.se>

-module(robox_queue).

-export([init/0]). %%  subscribe
-export([enq/1, enq/2, deq/0, deq/2]).
-export([make_filename/1]).
%%
-export([is_data_filename/2]).

init() ->
    application:load(robox),
    QDir = application:get_env(robox, queue_dir, default_queue_dir()),
    case filelib:is_dir(QDir) of
	false ->
	    file:make_dir(QDir);
	true ->
	    %% clear dir? when? (via web page)
	    ok    
    end.

%% put term data in to a file in the queue directory
enq(Data) -> enq($D,Data).
enq(Pfx,Data) when Pfx >= $A, Pfx =< $Z ->
    QDir = application:get_env(robox, queue_dir, default_queue_dir()),
    Format = application:get_env(robox, queue_file_format, json),
    Filename = make_filename([Pfx]),
    Encoded = case Format of
		  json -> jsone:encode(Data);
		  term -> term_to_binary(Data)
	      end,
    file:write_file(filename:join(QDir,Filename), Encoded).

%% slow dequeue 
%% deq first item and read/peek Max number of items following the first
deq() -> deq([],0).
deq(Pfx, Max) when Max >= 0 ->
    QDir = application:get_env(robox, queue_dir, default_queue_dir()),
    {ok, Fs} = file:list_dir(QDir),
    Fs1 = remove_non_data_files(Pfx,Fs),
    %% io:format("Fs1 = ~p\n", [Fs1]),
    Fs2 = lists:sort(Fs1),
    %% io:format("Fs2 = ~p\n", [Fs2]),
    if Fs2 =:= [] -> 
	    false;
       true ->
	    [HeadFileName|_] = Fs2,
	    case read_file_list_info(Fs2, QDir, Max+1) of
		{[],_} -> false;
		{[Head|Items], N} ->
		    Filepath = filename:join(QDir, HeadFileName),
		    file:delete(Filepath),
		    {Head, Items, N}
	    end
    end.

read_file_list_info(FilenameList, QDir, Max) ->
    read_file_list_info_(FilenameList, QDir, Max, []).

read_file_list_info_(Fs, _QDir, 0, Acc) ->
    {lists:reverse(Acc), length(Fs)};
read_file_list_info_([], _QDir, _, Acc) ->
    {lists:reverse(Acc), 0};
read_file_list_info_([Filename|Fs], QDir, I, Acc) ->
    case read_file(filename:join(QDir,Filename)) of
	_Error = {error, _} -> 
	    read_file_list_info_(Fs,QDir,I,Acc);
	{ok,Item} ->
	    read_file_list_info_(Fs,QDir,I-1,[Item|Acc])
    end.
	
read_file(Filename) ->
    case file:read_file(Filename) of
	Error={error, _} -> Error;
	{ok,Data = <<131,_/binary>>} -> 
	    {ok,binary_to_term(Data)};
	{ok,Data} ->
	    {ok,jsone:decode(Data)}
    end.

default_queue_dir() ->
    filename:join(code:priv_dir(robox), "queue").

remove_non_data_files(Pfx,[F|Fs]) ->
    case is_data_filename(Pfx,F) of
	true ->
	    [F|remove_non_data_files(Pfx,Fs)];
	false ->
	    remove_non_data_files(Pfx,Fs)
	end;
remove_non_data_files(_Pfx,[]) ->
    [].

%% prefix empty match all prefixes A-Z
is_data_filename([],F) ->
    case re:run(F, "^[A-Z][0-9A-F]+-[0-9A-F]+.dat") of
	{match, _} -> true;
	_ -> false
    end;
%% otherwise hard match
is_data_filename([P],[P|F]) ->
    case re:run(F, "^[0-9A-F]+-[0-9A-F]+.dat") of
	{match, _} -> true;
	_ -> false
    end;
is_data_filename(_, _) ->
    false.

make_filename(Pfx) when 
      Pfx =:= []; hd(Pfx) >= $A, hd(Pfx) =< $Z ->
    T = tl(integer_to_list((1 bsl 64) + erlang:system_time(seconds),16)),
    U = tl(integer_to_list((1 bsl 64) + erlang:unique_integer([positive]),16)),
    Pfx ++ T ++ "-" ++ U ++ ".dat".
