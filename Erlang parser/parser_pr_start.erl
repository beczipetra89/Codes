-module(parser_pr_start).
-compile(export_all).
%%-import(parser_pr, [parse_entry_body/2]).

get_info() ->
	inets:start(),

	Place = "profilrestauranger",
	Adress = "Göteborg",
	Name = [],
	Description = [],
	Time = [],
	Date = [],
	Picture = [],
	Event = [Name, Description, Time, Date, Picture],

	%spawn(parser_pr_start, init, [Event]).
	init(Event).

init(Event) ->
	{ok, {{Version, 200, ReasonPhrase}, Header, Body}} =
		httpc:request("http://www.profilrestauranger.se/evenemang/kategori/goteborg/"),
		io:format("Getting the web page...~n"),
	find_entry_urls(Body, Event).

find_entry_urls("<td><strong><a href="++[$"|Rest], Event) -> 
 	parse_entry(Rest, [], Event);
find_entry_urls([H|T], Event) -> 
 	find_entry_urls(T, Event);
find_entry_urls([],Event) ->  Event.

parse_entry([$"|Rest], UrlReversed, Event) ->
	Url = lists:reverse(UrlReversed),
	io:format("Processing entry:" ++ Url ++ "~n"),
	{ok, {{Version, 200, ReasonPhrase}, Header, Body}} =
		httpc:request(Url),
	parser_pr:parse_entry_body(Body, Event),
% 	spawn(parser_pr, parse_entry_body, [Body, Event]),
	find_entry_urls(Rest, Event);
parse_entry([H|T], UrlReversed, Event) ->
	parse_entry(T, [H|UrlReversed], Event).