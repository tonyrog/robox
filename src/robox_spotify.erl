%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Spotify controller using DBus
%%% @end
%%% Created : 22 Apr 2024 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(robox_spotify).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([play/0, pause/0, next/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(ROBOX_URL, "http://www.rogvall.se/robox/robox.jpeg").
-define(ROBOX_URL_THUMB, "http://www.rogvall.se/robox/robox_thumb.jpeg").
-define(ROBOX_INTRO, "https://open.spotify.com/track/1AhDOtG9vPSOmsWgNW0BEY").

-record(state,
	{
	 is_playing = false :: boolean(),
	 remain_duration = 0 :: integer(),
	 timer_ref = undefined :: reference(),
	 con = undefined :: pid() %%: dbus connection
	}).

%%%===================================================================
%%% API
%%%===================================================================
play() ->
    ?SERVER ! play.

pause() ->
    ?SERVER ! pause.

next() ->
    ?SERVER ! next.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    os:cmd("killall spotify"),
    timer:sleep(3000),
    Result = os:cmd("(xvfb-run spotify --uri='"++?ROBOX_INTRO++"' 1>/dev/null 2>&1 &)"),
    %%  Result = os:cmd("(spotify --uri='"++?ROBOX_INTRO++"' 1>/dev/null 2>&1 &)"),
    io:format("Spotify started ~p\n", [Result]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    process_flag(trap_exit, true),
    ok = robox_queue:init(),
    {ok,Con} = dbus_connection:open(session),
    %% fixme: subsrcibe to signals / restart
    TRef = erlang:send_after(1000, self(), next_track),
    {ok, #state{ con=Con, timer_ref = TRef }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(pause, State) ->
    if State#state.is_playing, is_reference(State#state.timer_ref) ->
	    Remain = cancel_current_track(State#state.timer_ref),
	    dbus_spotify:pause(State#state.con),
	    {noreply, State#state { is_playing = false,
				    remain_duration = Remain,
				    timer_ref = undefined }};
       true ->
	    dbus_spotify:pause(State#state.con),
	    {noreply, State#state { is_playing = false }}
    end;

handle_info(play, State) ->
    if not State#state.is_playing, State#state.timer_ref =:= undefined ->
	    Duration = State#state.remain_duration,
	    if is_integer(Duration), Duration > 0 ->
		    dbus_spotify:play(State#state.con),
		    TRef = erlang:send_after(Duration, self(), next_track),
		    {noreply, State#state{ is_playing = true, 
					   timer_ref = TRef }};
	       true ->
		    dbus_spotify:stop(State#state.con),
		    TRef = erlang:send_after(1000, self(), next_track),
		    {noreply, State#state{ remain_duration = 0, 
					   timer_ref = TRef }}
	    end;
       true ->
	    {noreply, State}
    end;

handle_info(next, State) ->
    cancel_current_track(State#state.timer_ref),
    dbus_spotify:stop(State#state.con),
    TRef = erlang:send_after(1000, self(), next_track),
    {noreply, State#state{ remain_duration = 0, timer_ref = TRef }};

handle_info(next_track, State) ->
    io:format("next_track: ~p\n", [State#state.timer_ref]),
    cancel_current_track(State#state.timer_ref),
    case robox_queue:deq("T", 6) of  %% FIXME match 6 with robox.erl
	false ->
	    io:format("Queue empty\n"),
	    TRef = erlang:send_after(1000, self(), next_track),
	    {noreply, State#state { is_playing = false, timer_ref = TRef }};
	{Head, Items, Follow} ->
	    [TrackInfo|QueueInfo] = track_list_info([Head|Items], 640),
	    TrackUri = maps:get(<<"uri">>, Head),
	    Duration = maps:get(<<"duration_ms">>, Head),
	    dbus_spotify:open_uri(State#state.con, TrackUri),
	    io:format("TrackInfo: ~p\n", [TrackInfo]),
	    io:format("Follow: ~p\n", [Follow]),
	    case whereis(robox_page) of
		undefined ->
		    io:format("No robox_page defined\n", []);
		Pid -> 
		    Pid ! {current_track, TrackInfo, QueueInfo, Follow}
	    end,
	    TRef = erlang:send_after(Duration, self(), next_track),
	    {noreply, State#state { is_playing = true, timer_ref = TRef }};
	Item when is_map(Item) ->
	    Type = maps:get(Item,<<"type">>, <<"unknown">>),
	    io:format("Ignore queue item type ~p\n", [Type]),
	    TRef = erlang:send_after(1000, self(), next_track),
	    {noreply, State#state { timer_ref = TRef }};
	_Other ->
	    io:format("Ignore queue item ~p\n", [_Other]),
	    TRef = erlang:send_after(1000, self(), next_track),
	    {noreply, State#state { timer_ref = TRef }}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%
%% rearrange the track items for display
%%
track_list_info([Item|Items], PreferedHeight) ->
    TrackUri  = maps:get(<<"uri">>, Item),
    TrackName = maps:get(<<"name">>, Item),
    Duration  = maps:get(<<"duration_ms">>, Item),
    AristName = case maps:get(<<"artists">>, Item, []) of
		    [] -> "Unknown";
		    [#{ <<"name">> := AName }] -> AName
		end,
    Album = maps:get(<<"album">>, Item),
    AlbumName = maps:get(<<"name">>, Album),
    Images = maps:get(<<"images">>, Album),
    Image = find_image(PreferedHeight, Images),
    ImageUrl = case maps:get(<<"url">>, Image, undefined) of
		   undefined -> 
		       if PreferedHeight > 64 -> 
			       list_to_binary(?ROBOX_URL);
			  true ->
			       list_to_binary(?ROBOX_URL_THUMB)
		       end;
		   BinUrl -> BinUrl
	       end,
    ThumbHeight = 64,
    ThumbImage = find_image(ThumbHeight, Images),
    ThumbUrl = case maps:get(<<"url">>, ThumbImage, undefined) of
		   undefined -> list_to_binary(?ROBOX_URL_THUMB);
		   BinUrl1 -> BinUrl1
	       end,
    TrackInfo = #{<<"uri">> => TrackUri,
		  <<"track_name">> => TrackName,
		  <<"artist_name">> => AristName,
		  <<"album_name">> => AlbumName,
		  <<"album_cover_url">> => ImageUrl,
		  <<"album_thumb_url">> => ThumbUrl,
		  <<"duration">> => Duration
		 },
    [TrackInfo | track_list_info(Items, 64)];
track_list_info([], _) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cancel_current_track(undefined) ->
    0;
cancel_current_track(TRef) ->
    Remain = case erlang:cancel_timer(TRef) of
		 false -> 0;
		 R -> R
	     end,
    receive
	next_track -> Remain
    after 0 -> Remain
    end.

find_image(Height, ImageList=[Image|_]) ->
    H = maps:get(<<"height">>, Image) - Height,
    find_image_(Height, ImageList, H, Image).

find_image_(Height, [Image|ImageList], H, Best) ->
    H1 = maps:get(<<"height">>, Image) - Height,
    if abs(H1) < abs(H) -> find_image_(Height, ImageList, H1, Image);
       true -> find_image_(Height, ImageList, H, Best)
    end;
find_image_(_, [], _, Image) ->
    Image.
