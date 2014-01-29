(* TODO why does 'let open * in' syntax don't work, make *.mli *)
(* TODO tags & flags types *)
open Yojson.Basic.Util

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

(* TODO move to algorithm module *)
let basic_score retweets favorites =
    2 * retweets + favorites

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

(* FIXME ~atributes *)
let get_parsed_tweets ~number_of_tweets:num ~user:user =
    let json_raw = Connection.get_user_tweets ~number_of_tweets:num ~user:user in
    let json = (Yojson.Basic.from_string json_raw) in
    let json_list = json |> to_list in
    List.map (parse_tweet basic_score) json_list
