(* TODO Pervasives' channels instead of String (?) *)
open Printf

let pr x =
	(* try *)
	printf "**********\n%s\n***********\n" (Parser.string_of_tweet x);
	(* with *)
	(* | _ -> printf "BLAD W PROGRAMIE\n"; *)
	flush stdout;;
	(* ();; *)

let () =
	(* let json_raw = (Connection.get_user_tweets ~number_of_tweets:200 *)
	(* ~user:"PejaAdam") in *)
	(* let json = Yojson.Basic.from_string json_raw in *)
	(* Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json);; *)

	(* Printf.printf "%s\n" (Yojson.Basic.pretty_to_string	(Parser.get_parsed_tweets ~number_of_tweets:200 *)
	(* ~user:"PejaAdam"));; *)

	(* Printf.printf "%d\n" (List.length (Parser.get_parsed_tweets ~number_of_tweets:200 *)
	(* ~user:"PejaAdam"));; *)
	(* TODO check real number of tweets (remember about replies filter) *)
	(* try List.iter pr (Parser.get_parsed_tweets ~number_of_tweets:50 *)
	(* ~user:"PejaAdam22") with *)
	(* | _ -> printf "BLAD!!\n" *)

	try Printf.printf "%s" (Parser.string_of_user (Parser.get_user
	~user:"xxxxPejaAdam")) with
	| _ -> printf "BLAD!!\n"
