(* TODO Pervasives' channels instead of String (?) *)
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
	(* List.iter Parser.string_of_tweet *)
	(* (Parser.get_parsed_tweets ~number_of_tweets:50 ~user:"PejaAdam");; *)

	Printf.printf "%s" (Parser.string_of_user (Parser.get_user ~user:"xxxxPejaAdam"));;
