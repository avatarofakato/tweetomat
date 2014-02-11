(* TODO why does 'let open * in' syntax don't work, make *.mli *)
(* TODO tags & flags types *)
open Yojson.Basic.Util

exception No_posts of string
exception Twitter_error

(* TODO some type for dates (maybe something already exists?), check ID scope
 * for possible overflows, move to *.mli, include hashtags and tags to content *)
type tweet =
{
	tweetID: int;
	userID : int;
	content : string;
	date : string;
	retweets : int;
	favorites : int;
	score : int
}

type user =
{
	id : int;
	name : string;
	followers : int;
	tweetsNumber : int;
	priority : int
}

(* TODO move to algorithm module *)
let basic_score retweets favorites =
	2 * retweets + favorites

let parse_user json =
	{
		id = json |> member "user" |> member "id" |> to_int;
		name = json |> member "user" |> member "screen_name" |> to_string;
		followers = json |> member "user" |> member "followers_count" |> to_int;
		tweetsNumber = json |> member "user" |> member "statuses_count" |> to_int;
		priority = 42
	}

let parse_tweet calculate json =
	let _retweets = json |> member "retweet_count" |> to_int in
	let _favorites = json |> member "favorite_count" |> to_int in
	{
		tweetID = json |> member "id" |> to_int;
		userID = json |> member "user" |> member "id" |> to_int;
		content = json |> member "text" |> to_string;
		date = json |> member "created_at" |> to_string;
		retweets = _retweets;
		favorites = _favorites;
		score = calculate _retweets _favorites
	}

let string_of_tweet tweet_to_parse =
	match tweet_to_parse with
	| { tweetID = t; userID = u; content = c; date = d; retweets = r; favorites = f; score = s } ->
		Printf.sprintf
		"\ntweetID = %d\nuserID = %d\ncontent = %s\ndate = %s\nretweets = %d\nfavorites = %d\nscore = %d\n"
		t u c d r f s
	(* | _ -> raise Twitter_error *)

let string_of_user user_to_parse =
	match user_to_parse with
	| { id = i; name = n; followers = f; tweetsNumber = t; priority = p } ->
		Printf.sprintf
		"\nID = %d\nname = %s\nfollowers = %d\ntweetsNumber = %d\npriority = %d\n"
		i n f t p
	(* | _ -> raise Twitter_error *)

let get_parsed_tweets ~number_of_tweets:num ~user:user =
	let json_raw = Connection.get_user_tweets ~number_of_tweets:num ~user:user in
	let json = (Yojson.Basic.from_string json_raw) in
	let json_list = try Some (json |> to_list) with _ -> None in
	match json_list with
	| Some x -> List.map (parse_tweet basic_score) x
	| None -> raise Twitter_error

let get_user ~user:user =
	let json_raw = Connection.get_user_tweets ~number_of_tweets:1 ~user:user in
	let json = (Yojson.Basic.from_string json_raw) in
	let json_list = try Some (json |> to_list) with _ -> None in
	match json_list with
	| Some x ->
		begin
			match x with
			| h::t -> parse_user h
			| _ -> raise (No_posts user)
		end
	| None -> raise Twitter_error
