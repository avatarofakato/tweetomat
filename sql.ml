(* file: sql.ml *)
open String
open Pervasives
open Printf
open Parser

exception Sql_error of string

(* TODO exceptions *)
let cut_quot_marks str =
	(* if (length str < 3) then *)
		(* str *)
	(* else if (get str 0 != '"') then *)
		(* str *)
	(* else if (get str ((length str) - 1) != '"') then *)
		(* str *)
	(* else *)
	sub str 1 ((String.length str) - 2)

let string_of_table = function
	| `TwitterUsers -> "TwitterUsers"
	| `Tweet -> "Tweet"
	| `Tag -> "Tag"
	| `Flag -> "Flag"

let string_of_column = function
	| `ID -> "ID"
	| `Name-> "Name"
	| `Followers -> "Followers"
	| `TweetsNumber-> "ID"
	| `Priority -> "Priority"
	| `UserID -> "UserID"
	| `Content -> "Content"
	| `Date -> "Date"
	| `Retweets -> "Retweets"
	| `Favorites -> "Favorites"
	| `Score -> "Score"
	| `Everything -> "*"
	| `Count -> "Count(*)"

let element_in_set ~column ~set =
	string_of_column column ^
	" in (" ^
	String.concat "," set ^
	")"

let get_field_from_tweet tweet field =
	match tweet with
	| { tweetID = t; userID = u; content = c; date = d; retweets = r; favorites = f; score = s } ->
		(match field with
		| `ID -> string_of_int t
		| `UserID -> string_of_int u
		| `Content -> c
		| `Date -> d
		| `Retweets -> string_of_int r
		| `Favorites -> string_of_int f
		| `Score -> string_of_int s)

(* Returns list of rows each containing column (as string) *)
let get_from_sql ~dbh ?(what = [`Everything]) ~from ?(where = "1=1") () =
	let what = String.concat "," (List.map string_of_column what) in
	(* let from = String.concat "," (List.map string_of_table from) in *)
	let where = match where with
	| "" -> ""
	| _ -> " where " ^ where in
	let goal = ref [] in
	let parse src acc =
		acc := (List.map 
			(function
				| None -> "NULL"
				| Some str -> sprintf "%S" str) src)::(!acc)in
	let query = "select " ^ what ^
				" from " ^ (string_of_table from) ^
				where in
	printf "\n%s\n\n" query;
	flush stdout;
	ignore (PGOCaml.prepare dbh ~query ());
	PGOCaml.cursor dbh ~params:[] 
	(fun row -> parse row goal);
	List.map (List.map cut_quot_marks) !goal

let get_subscribers_ids dbh =
	List.flatten (get_from_sql ~dbh:dbh ~what:[`ID] ~from:`TwitterUsers ())

let get_subscribers_names dbh =
	List.flatten (get_from_sql ~dbh:dbh ~what:[`Name] ~from:`TwitterUsers ())

let get_tweets_dates dbh subs =
	let where = match subs with
	| [] -> ""
	| _ -> element_in_set ~column:`UserID ~set:subs in
	List.flatten (get_from_sql ~dbh:dbh ~what:[`Date] ~from:`Tweet
		~where:where ())

let get_tweets_ids dbh subs =
	let where = match subs with
	| [] -> ""
	| _ -> element_in_set ~column:`UserID ~set:subs in
	List.flatten (get_from_sql ~dbh:dbh ~what:[`ID] ~from:`Tweet
		~where:where ())

let does_db_contain dbh table id =
	match (get_from_sql ~dbh:dbh ~what:[`Count] ~from:table
		~where:(element_in_set ~column:`ID ~set:[id]) ()) with
		| [[h]] -> (match h with
			| "0" -> false
			| "1" -> true
			| _ -> raise (Sql_error ("does_db_contain(" ^ id ^ ")")))
		| _ -> raise (Sql_error ("does_db_contain(" ^ id ^ ")"))

let get_id_of_name dbh name =
	match (get_from_sql ~dbh:dbh ~what:[`ID] ~from:`TwitterUsers
		~where:((string_of_column `Name) ^ "='" ^ name ^ "'") ()) with
		| [[h]] -> h
		| _ -> raise (Sql_error ("get_id_of_name(" ^ name ^ ")"))

(* TODO string concatenation instead of sprintf *)
let make_string_of_tweet_row row =
	match row with
	| t::u::c::d::r::f::n::[] -> sprintf
		"tweetID = %s\nuserID = %s\ncontent = %s\ndate = %s\nretweets = %s\nfavorites = %s\nscore = %s\n"
		t u c d r f n
	| _ -> "Error\n"

let get_tweet_of_id dbh id =
	match
	(get_from_sql ~dbh:dbh ~what:[`Everything] ~from:`Tweet
		~where:((string_of_column `ID) ^ "='" ^ id ^ "'") ()) with
	| h::t -> make_string_of_tweet_row h
	| [] -> make_string_of_tweet_row []

(* Adds user to database *)
let add_subscription entry dbh () =
	let query = "insert into TwitterUsers (ID, Name, Followers, TweetsNumber, Priority) values ($1, $2, $3, $4, $5)" in
	ignore (PGOCaml.prepare dbh ~query ());
	match (get_user ~user:entry#text) with
	| { id = i; name = n; followers = f; tweetsNumber = t; priority = p } ->
		ignore (PGOCaml.execute dbh 
		~params:
			[Some (string_of_int i);
			 Some n;
			 Some (string_of_int f);
			 Some (string_of_int t);
			 Some (string_of_int p)]
			());
	()

(* TODO value too long for type character varying(140) *)
let add_tweet tweet dbh () =
	let query = "insert into Tweet (ID, userID, Content, Date, Retweets, Favorites, Score) values ($1, $2, $3, $4, $5, $6, $7)" in
	ignore (PGOCaml.prepare dbh ~query ());
	match tweet with
	| { tweetID = t; userID = u; content = c; date = d; retweets = r; favorites = f; score = s } ->
		ignore (PGOCaml.execute dbh 
		~params:
			[Some (string_of_int t);
			 Some (string_of_int u);
			 Some c;
			 Some d;
			 Some (string_of_int r);
			 Some (string_of_int f);
			 Some (string_of_int s)]
			());
	()
