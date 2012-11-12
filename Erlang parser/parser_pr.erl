%% Author: Petra Beczi
%% Email: beczipetra89@gmail.com
%% www.profilrestauranger.se parser

-module(parser_pr).
-compile(export_all).

parse_entry_body("<h1>"++Rest, Event) -> 			%%Check event name
	grab_name(Rest, [], Event);
parse_entry_body("Datum: <strong>"++Rest, Event) -> %%Check event date
	grab_date(Rest, [], Event);
parse_entry_body("ppnar: "++Rest, Event) -> 		%%Check event time
	grab_time(Rest, [], Event);
parse_entry_body("/images/sized/images/uploads/"++Rest, Event) ->    %%Check event picture and description
	grab_image_and_description(Rest, [], Event);
parse_entry_body("<p><a href="++Rest, Event) ->
	parse_entry_body([], Event);
parse_entry_body("<p><strong>"++Rest, Event) ->
	parse_entry_body(Rest, Event);
parse_entry_body("<p>"++Rest, Event) ->				%%Check description
	grab_description(Rest, [], Event);
parse_entry_body([H|T], Event) -> 
	parse_entry_body(T, Event);
parse_entry_body([], Event) -> 
	io:format("ProfilRestauranger parsing done!!!!!!"), 
	Event.

grab_name([$<|Rest], NewEventNameReversed, [Name|T]) ->
	NewName = lists:reverse(NewEventNameReversed),
	io:format("Event Name: "++NewName++"~n"),
	parse_entry_body(Rest, [[Name|NewName]|T]);
grab_name([H|T], NewEventNameReversed, Event) -> grab_name(T, [H|NewEventNameReversed], Event).

grab_date([$<|Rest], NewDateReversed, [Date|T]) ->
	NewDate = lists:reverse(NewDateReversed),
	io:format("Event Date: "++NewDate++"~n"),
	parse_entry_body(Rest, [[Date|NewDateReversed]|T]);
grab_date([H|T], NewDateReversed, Event) -> grab_date(T, [H|NewDateReversed], Event).

grab_time("<br />" ++ Rest, NewTimeReversed, [Time|T]) ->
	NewTime = lists:reverse(NewTimeReversed),
	io:format("Event Time:" ++ NewTime ++ "~n"),
	parse_entry_body(Rest, [[Time|NewTimeReversed]|T]);
grab_time([H|T], NewTimeReversed, Event) -> grab_time(T, [H|NewTimeReversed], Event).

grab_image_and_description([$"|Rest], NewImageReversed, [Picture|T]) ->
	NewImage = "http://www.profilrestauranger.se/images/sized/images/uploads/" 
		++ lists:reverse(NewImageReversed),
	io:format("Event Picture: " ++ NewImage ++ "~n"),
	find_description(Rest, [], [[NewImage|Picture]|T]);
grab_image_and_description([H|T], NewImageReversed, Event) -> 
	grab_image_and_description(T, [H|NewImageReversed], Event).

find_description("<p>" ++ Rest, [], Event) ->  
	grab_description(Rest, [], Event);
find_description([H|T], [], Event) ->
	find_description(T, [], Event).

grab_description("</p>" ++ Rest, NewDescriptionReversed, [Name,Description|T]) ->
	NewDescription = lists:reverse(NewDescriptionReversed) ++ "\n",
	io:format("NewDescription: ~ts~n",[NewDescription]),
	parse_entry_body(Rest, [Name,[Description|[NewDescription]]|T]);
grab_description("<br />" ++ Rest, NewDescriptionReversed, Event) ->
	grab_description(Rest, [lists:reverse("\n")|NewDescriptionReversed], Event);
grab_description("<a href=" ++ Rest, NewDescriptionReversed, Event) ->
	grab_description(omit_link(Rest), NewDescriptionReversed, Event);
grab_description("</a>" ++ Rest, NewDescriptionReversed, Event) ->
	grab_description(Rest, NewDescriptionReversed, Event);
grab_description([H|T], NewDescriptionReversed, Event) ->
	grab_description(T, [H|NewDescriptionReversed], Event).


omit_link("</a>" ++ Rest) ->
	Rest;
omit_link([H|T]) ->
	omit_link(T).