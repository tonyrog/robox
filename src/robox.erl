%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%      Simple spotify robot jukebox
%%% @end
%%% Created : 21 Apr 2024 by Tony Rogvall <tony@rogvall.se>

-module(robox).

-export([start/0, start/1]).
-export([enq/1, enq_list/1]).
-export([run/2]).
-export([populate/0]).
-export([format_duration/1]).
-export([make_smsto/0, make_smsto/1]).
-export([make_mailto/0, make_mailto/2]).
-export([make_webto/0, make_webto/1]).

-export([make_wifi_access/0, make_wifi_access/2]).
%%-export([make_mail_qr/0]).
%%-export([make_web_qr/0]).

-define(WS_PORT, 1235).
-define(ROBOX_URL, "http://www.rogvall.se/robox/robox.jpeg").
-define(THUMB_URL, "http://www.rogvall.se/robox/robox_thumb.jpeg").
-define(NBSP, [16#A0]).

-define(PROGRESS(F,A), io:format((F),(A))).

populate() ->
    robox:enq_list(dbus_spotify:play_list()).

start() -> start([]).  %% [kiosk,private]
start(Mode) when is_atom(Mode) -> start([Mode]);
start(Mode) when is_list(Mode) ->
    wse_server:start(1235),
    HtmlFile = filename:join(code:priv_dir(robox), "robox.html"),
    Options = 
	case lists:member(kiosk, Mode) of
	    true -> "--kiosk ";
	    false -> ""
	end ++
	case lists:member(private, Mode) of
	    true -> "--private-window ";
	    false -> ""
	end,
    os:cmd("(firefox "++Options++" "++HtmlFile++" 1>/dev/null 2>&1 &)").

%% enqueue a track from a spotify link
enq(SpotifyUrl) ->
    Object = dbus_spotify:object(SpotifyUrl),
    Data = robox_spotify_api:get_tracks(Object),
    case maps:get(<<"type">>, Data) of
	<<"track">> -> robox_queue:enq($T,Data);
	<<"playlist">> -> robox_queue:enq($P,Data);
	<<"album">> -> robox_queue:enq($A,Data)
    end.

enq_list([Url|Urls]) ->
    enq(Url), enq_list(Urls);
enq_list([]) ->
    ok.

%%
%% Web page callback & loop
%%
-record(s, {
	    id,
	    pause_event,  %% named event "pause"
	    next_event,   %% named event "next"
	    pause = false,
	    parent, 
	    album_cover_img,
	    queue_table, 
	    pause_button
	   }).

run(Ws, Where=[ID, ID_AlbumCoverImg, ID_Queue, ID_Pause]) ->
    %% install some data!
    io:format("robox:run ~p\n", [Where]),
    Parent = wse:id(ID),  %% Top Table element
    io:format("PARENT = ~p\n", [Parent]),
    {ok,AlbumCoverImg} = wse:getElementById(Ws, ID_AlbumCoverImg),
    {ok,QueueTable} = wse:getElementById(Ws, ID_Queue),
    {ok,PauseButton} = wse:getElementById(Ws, ID_Pause),
    case whereis(robox_page) of
	undefined ->
	    register(robox_page, self());
	Pid ->
	    exit(Pid, kill),
	    register(robox_page, self())
    end,
    ?PROGRESS("robox:registered 'robox_page' = ~p\n", [whereis(robox_page)]),
    {ok,PauseRef} = wse:create_user_event(Ws, "robox_pause", all, "Pause"),
    {ok,NextRef} = wse:create_user_event(Ws, "robox_next", all, "Next"),
    %% now create spotify backen
    Res = robox_spotify:start_link(),
    ?PROGRESS("robox: spotify backend created ~p\n", [Res]),
    ?PROGRESS("robox: wait 5s\n", []),
    timer:sleep(5000),
    ?PROGRESS("robox: running\n", []),

    St = #s {id=ID, 
	     parent=Parent,
	     pause_event = PauseRef, 
	     next_event = NextRef, 
	     album_cover_img=AlbumCoverImg, 
	     queue_table=QueueTable, 
	     pause_button=PauseButton},
    run_loop(Ws, St).

%% FIXME: 
%%   3 - add webcam control interface to read qr codes
%%   4 - add SMS interface to add tracks
%%   5 - add Entry for tracks
%%
run_loop(Ws, S) ->
    receive
	{notify, Event, _Local, _Data} when S#s.pause_event =:= Event ->
	    %% FIXME Read current state from player
	    case S#s.pause of
		true ->
		    robox_spotify:play(),
		    set_button_text(Ws, S#s.pause_button, "Pause");
		false ->
		    robox_spotify:pause(),
		    set_button_text(Ws, S#s.pause_button, "Play")
	    end,
	    run_loop(Ws, S#s{pause = not S#s.pause});
	{notify, Event, _Local, _Data} when S#s.next_event =:= Event ->
	    robox_spotify:next(),
	    run_loop(Ws, S);
	Msg = {notify,_ID,_Local,_Data} ->
	    io:format("Notification: ~p\n", [Msg]),
	    run_loop(Ws, S);
	Current = {current_track, TrackInfo, QueueInfo, QueueFollow} ->
	    %% FIXME: get event for no more tracks (disable next button)
	    io:format("Current: ~p\n", [Current]),
	    ImageUrl = maps:get(<<"album_cover_url">>, TrackInfo, ?ROBOX_URL),
	    wse:set(Ws, S#s.album_cover_img, "src", ImageUrl),
	    %% FIXME: display current track info above table in bigger text
	    %% and have a duration timer ticking
	    try update_queue_table(Ws, S#s.queue_table, 6,
				   [TrackInfo|QueueInfo], QueueFollow) of
		_Res -> ok
	    catch
		_:_:Stack -> 
		    io:format("Error updating queue table\n"),
		    io:format("Crash in\n~p\n", [Stack])
	    end,
	    %% Update queue table with QueueInfo
	    run_loop(Ws, S#s {pause = false})
    end.

update_queue_table(Ws, _TableID, N, QueueInfo, QueueFollow) ->
    %% {ok,TBody} = wse:firstElementChild(Ws, TableID),
    %% TR0 = wse:firstElementChild(Ws, TBody), %% dummy row
    %% TR1 = wse:nextElementSibling(Ws, TR0),  %% header row
    %% TR  = wse:nextElementSibling(Ws, TR1),  %% first "real" row
    {ok,TR} = wse:getElementById(Ws, "robox_queue_first_row"),
    update_queue_table_(Ws, TR, N, QueueInfo, QueueFollow).

update_queue_table_(Ws, TR, 0, _, QueueFollow) ->
    if QueueFollow > 0 ->
	    {ok,TD} = wse:firstElementChild(Ws, TR),
	    FollowString = [$+|integer_to_list(QueueFollow)],
	    set_queue_row_(Ws, TD, [FollowString,?NBSP,?NBSP,?NBSP,?NBSP]);
       true ->
	    {ok,TD} = wse:firstElementChild(Ws, TR),
	    set_queue_row_(Ws, TD, [?NBSP,?NBSP,?NBSP,?NBSP,?NBSP])
    end;
update_queue_table_(Ws, TR, I, [Item|Queue], QueueFollow) ->
    {ok,TD} = wse:firstElementChild(Ws, TR),
    update_queue_row_(Ws, TD, Item),
    {ok,NextTR} = wse:nextElementSibling(Ws, TR),
    update_queue_table_(Ws, NextTR, I-1, Queue, QueueFollow);
update_queue_table_(Ws, TR, I, [], QueueFollow) ->
    {ok,TD} = wse:firstElementChild(Ws, TR),
    set_queue_row_(Ws, TD, [?THUMB_URL,
			    ?NBSP,?NBSP,?NBSP,?NBSP]),
    {ok,NextTR} = wse:nextElementSibling(Ws, TR),
    update_queue_table_(Ws, NextTR, I-1, [], QueueFollow).


update_queue_row_(Ws, TD, Item) ->
    ThumbUrl = maps:get(<<"album_thumb_url">>, Item, ?THUMB_URL),
    TrackName = maps:get(<<"track_name">>, Item, <<"Unknown">>),
    ArtistName = maps:get(<<"artist_name">>, Item, <<"Unknown">>),
    AlbumName = maps:get(<<"album_name">>, Item, <<"Unknown">>),
    Duration = maps:get(<<"duration">>, Item, 0),
    set_queue_row_(Ws, TD, [ThumbUrl,
			    TrackName,
			    ArtistName,
			    AlbumName, 
			    Duration]).

set_queue_row_(Ws, TD0, [Thumb,Track,Artist,Album,Duration]) ->
    io:format("Track = ~p\n", [Track]),
    io:format("Artist = ~p\n", [Artist]),

    case Thumb of
	<<"http",_/binary>> -> %% http/https
	    {ok,ImgNode} = wse:firstChild(Ws, TD0),
	    io:format("Thumb = ~p\n", [Thumb]),
	    wse:set(Ws, ImgNode, "src", Thumb);
	"http"++_ ->  %% http/https
	    {ok,ImgNode} = wse:firstChild(Ws, TD0),
	    io:format("Thumb = ~p\n", [Thumb]),
	    wse:set(Ws, ImgNode, "src", Thumb);
	_ ->
	    %% should be number of tracks ramain in queue
	    {ok,ThumbNode} =  wse:firstChild(Ws, TD0),
	    wse:set(Ws, ThumbNode, "nodeValue", Track)
    end,
    {ok,TD1} = wse:nextElementSibling(Ws, TD0),
    {ok,TrackNode} =  wse:firstChild(Ws, TD1),
    wse:set(Ws, TrackNode, "nodeValue", Track),
    
    {ok,TD2} = wse:nextElementSibling(Ws, TD1),
    {ok,ArtistNode} = wse:firstChild(Ws, TD2),
    wse:set(Ws, ArtistNode, "nodeValue", Artist),

    {ok,TD3} = wse:nextElementSibling(Ws, TD2),
    {ok,AlbumNode} = wse:firstChild(Ws, TD3),
    wse:set(Ws, AlbumNode, "nodeValue", Album),

    {ok,TD4} = wse:nextElementSibling(Ws, TD3),
    {ok,DurationNode} = wse:firstChild(Ws, TD4),
    wse:set(Ws, DurationNode, "nodeValue", format_duration(Duration)).

format_duration(DurationMs) when is_integer(DurationMs) ->
    DurationS = (DurationMs+500) div 1000,
    Min0 = DurationS div 60,
    Sec = DurationS rem 60,
    Hours = Min0 div 60,
    Min = Min0 rem 60,
    if Hours > 0 ->
	    integer_to_list(Hours)++":"++
		tl(integer_to_list(100+Min)) ++ ":" ++ 
		tl(integer_to_list(100+Sec));
       true ->
	    integer_to_list(Min) ++ ":" ++ tl(integer_to_list(100+Sec))
    end;
format_duration(Other) ->
    io:format("Duration is no a number ~p\n", [Other]),
    Other.


set_button_text(Ws, Button, Text) ->
    io:format("Button = ~p\n", [Button]),
    case wse:firstChild(Ws, Button) of
	{ok,TextNode} ->
	    io:format("set: TextNode = ~p, text=~p\n", [TextNode, Text]),
	    wse:set(Ws, TextNode, "nodeValue", Text);
	Other ->
	    io:format("set: Other = ~p\n", [Other])
    end.

%%
%% Create QR codes
%%
make_smsto() ->
    make_smsto(<<"0727483718">>).
make_smsto(Number) ->
    %% FIXME: get from config 'sms_phonenumber'
    SMSTO = <<"SMSTO:",Number/binary,":">>,
    QRCode = qrcode:encode(SMSTO),
    Image = simple_png_encode(QRCode),
    Filename = "smsto.png",
    file:write_file(Filename, Image).

make_mailto() ->
    make_mailto(<<"robox@rovall.se">>, <<"Enqueue">>).
make_mailto(Addr, Subject) ->
    Mail = <<"MAILTO:", Addr/binary, "?subject=", Subject/binary>>,
    QRCode = qrcode:encode(Mail),
    Image = simple_png_encode(QRCode),
    Filename = "mailto.png",
    file:write_file(Filename, Image).

make_webto() ->
    %% FIXME: get from config 'robox_url'
    make_webto("http://robox.local/enque").
make_webto(Link) ->
    WebTo = list_to_binary([Link]),
    QRCode = qrcode:encode(WebTo),
    Image = simple_png_encode(QRCode),
    Filename = "webto.png",
    file:write_file(Filename, Image).

make_wifi_access() ->
    %% FIXME: get from config
    make_wifi_access(<<"Tele2_b1264f">>, <<"atq2memn">>).
make_wifi_access(SSID, Password) ->
    Wifi = <<"WIFI:T:WPA;S:",SSID/binary, ";P:", Password/binary, ";;">>,
    QRCode = qrcode:encode(Wifi),
    Image = simple_png_encode(QRCode),
    Filename = "wifi.png",
    file:write_file(Filename, Image).


-include_lib("qrcode/include/qrcode.hrl").

%% Very simple PNG encoder for demo purposes
simple_png_encode(#qrcode{dimension = Dim, data = Data}) ->
	MAGIC = <<137, 80, 78, 71, 13, 10, 26, 10>>,
	Size = Dim * 8,
	IHDR = png_chunk(<<"IHDR">>, <<Size:32, Size:32, 8:8, 2:8, 0:24>>), 
	PixelData = get_pixel_data(Dim, Data),
	IDAT = png_chunk(<<"IDAT">>, PixelData),
	IEND = png_chunk(<<"IEND">>, <<>>),
	<<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.

png_chunk(Type, Bin) ->
	Length = byte_size(Bin),
	CRC = erlang:crc32(<<Type/binary, Bin/binary>>),
	<<Length:32, Type/binary, Bin/binary, CRC:32>>.

get_pixel_data(Dim, Data) ->
	Pixels = get_pixels(Data, 0, Dim, <<>>),
	zlib:compress(Pixels).

get_pixels(<<>>, Dim, Dim, Acc) ->
	Acc;
get_pixels(Bin, Count, Dim, Acc) ->
	<<RowBits:Dim/bits, Bits/bits>> = Bin,
	Row = get_pixels0(RowBits, <<0>>), % row filter byte
	FullRow = binary:copy(Row, 8),
	get_pixels(Bits, Count + 1, Dim, <<Acc/binary, FullRow/binary>>).
	
get_pixels0(<<1:1, Bits/bits>>, Acc) ->
	Black = binary:copy(<<0>>, 24),
	get_pixels0(Bits, <<Acc/binary, Black/binary>>);
get_pixels0(<<0:1, Bits/bits>>, Acc) ->
	White = binary:copy(<<255>>, 24),
	get_pixels0(Bits, <<Acc/binary, White/binary>>);
get_pixels0(<<>>, Acc) ->
	Acc.
