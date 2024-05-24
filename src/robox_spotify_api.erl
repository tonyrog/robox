%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%   Spotify API for robox
%%% @end
%%% Created : 22 Apr 2024 by Tony Rogvall <tony@rogvall.se>

-module(robox_spotify_api).

-export([login/0]).
-export([get_tracks/1]).
-export([search_track/1]).
-export([search/2]).

-export([show_track/1]).
-export([show_search/1, show_search/2]).


get_tracks(ObjectOrID) -> 
    %% FIXME: check that Object is a track object or just hex numbers..
    ID = lists:last(string:split(ObjectOrID, ":", all)),
    case access_token() of
	{ok, AccessToken} ->
	    {ok,Config} = application:get_env(robox, spotify),
	    Market = proplists:get_value(market, Config, "SE"),
	    Url = compose_url(proplists:get_value(api, Config) ++ "/tracks/"++
				  ID,
			      [{market, Market}]),
	    case rester_http:wget(Url,
				  [{'Authorization',
				    "Bearer "++AccessToken}]) of
		{ok, _Resp, Body} ->
		    jsone:decode(Body);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

search_track(Query) -> search(Query,"track").

search(Query,Type) ->
    case access_token() of
	{ok, AccessToken} ->
	    {ok,Config} = application:get_env(robox, spotify),
	    Market = proplists:get_value(market, Config),
	    Url = compose_url(proplists:get_value(api, Config) ++ "/search",
			      [{q, Query}, {type, Type}, {market, Market},
			       {limit, 2}]),
	    case rester_http:wget(Url,
				  [{'Authorization',
				    "Bearer "++AccessToken}]) of
		{ok, _Resp, Body} ->
		    jsone:decode(Body);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.
    
compose_url(Base, Options) ->
    Base ++ "?" ++ rester_http:format_query(Options).

show_track(ID) ->
    case get_tracks(ID) of
	Error = {error, _} -> Error;
	Track ->
	    io:format("Tarack: ~s~n", [maps:get(<<"name">>, Track)]),
	    ArtistList = maps:get(<<"artists">>, Track),
	    io:format("Artist: ~s~n", [maps:get(<<"name">>, hd(ArtistList))]),
	    Album = maps:get(<<"album">>, Track),
	    io:format("Album: ~s~n", [maps:get(<<"name">>, Album)]),
	    Images = maps:get(<<"images">>, Album),
	    PreferedHeight = 100,
	    Image = find_image(PreferedHeight, Images),
	    io:format("Image[~wx~w]: ~s~n", 
		      [maps:get(<<"width">>, Image),
		       maps:get(<<"height">>, Image),
		       maps:get(<<"url">>, Image)])
    end.

show_search(Query) ->
    show_search(Query, "track").
show_search(Query,Type) ->
    case search(Query,Type) of
	Error = {error, _} -> Error;
	SearchResult ->
	    Types = case Type of
			"track" -> <<"tracks">>;
			"artist" -> <<"artists">>;
			"album" -> <<"albums">>;
			"playlist" -> <<"playlists">>
		    end,
	    Result = maps:get(Types, SearchResult),
	    Items = maps:get(<<"items">>, Result),
	    lists:foreach(
	      fun(Item) ->
		      io:format("ID: ~s~n", [maps:get(<<"id">>, Item)])
	      end, Items)
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

access_token() ->
    try persistent_term:get({?MODULE, access_token}) of
	AccessToken -> %% found access token check time
	    Expires = persistent_term:get({?MODULE, expires}),
	    case erlang:system_time(seconds) of
		Now when Now > Expires -> 
		    login(), access_token_();
		_ -> {ok, AccessToken}
	    end
    catch
	_:_ -> login(), access_token_()
    end.

access_token_() ->
    try persistent_term:get({?MODULE, access_token}) of
	AccessToken -> {ok,AccessToken}
    catch
	_:_ -> {error, login_failure}
    end.

%% loggin and store access_token and time stamp in presistent store

login() ->
    load(robox),
    load(rester),
    case application:get_env(robox, spotify) of
	{ok, Config} ->
	    application:ensure_all_started(rester),
	    Url = proplists:get_value(login_url, Config),
	    ClientId = proplists:get_value(client_id, Config),
	    ClientSecret = proplists:get_value(client_secret, Config),
	    case rester_http:wpost(Url,
				   [{'Content-Type',
				     "application/x-www-form-urlencoded"}],
				   [
				    {grant_type, "client_credentials"},
				    {client_id, ClientId},
				    {client_secret, ClientSecret}
				   ]) of
		{ok, _Resp, Data} -> %% fixme: check response!
		    TokenData = jsone:decode(Data),
		    AccessToken = maps:get(<<"access_token">>, TokenData),
		    ExpiresIn = maps:get(<<"expires_in">>, TokenData),
		    persistent_term:put({?MODULE, access_token}, AccessToken),
		    persistent_term:put({?MODULE, expires}, 
					erlang:system_time(seconds)+ExpiresIn),
		    ok;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

load(App) ->	   
    case application:load(App) of
	ok -> ok;
	{error, {already_loaded,App}} -> ok
    end.
