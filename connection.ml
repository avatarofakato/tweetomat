(* TODO exceptions *)
let fetch url params f =
	let oau = (Twitter_oauth.get_twitter_oauth url params)::[] in
	let connection = Curl.init () in
	Curl.setopt connection (Curl.CURLOPT_VERBOSE true);
	Curl.setopt connection (Curl.CURLOPT_HTTPGET true);
	Curl.setopt connection (Curl.CURLOPT_HTTPHEADER oau);
	Curl.set_url connection url;
	Curl.set_followlocation connection true;
	Curl.set_writefunction connection f;
	Curl.perform connection;
	Curl.cleanup connection

let process buf_ref tmp =
	Buffer.add_string !buf_ref tmp;
	String.length tmp

let get url params =
	let buf = Buffer.create 65536 in
	let buf_ref = ref buf in
	(* let raw_text: int ref = ref 0 in *)
	fetch url params (process buf_ref);
	buf_ref

(* TODO global constant (move to *.mli?) *)
let user_timeline_url =
	"https://api.twitter.com/1.1/statuses/user_timeline.json"

(* TODO test chanells instead of buffer evaluate (or test for overflows of
 * string while using Buffer.contents *)
let get_user_tweets ~number_of_tweets:num ~user:u =
	let params = ("count", string_of_int num)::("screen_name", u)::
				 ("trim_user", "false")::("exclude_replies", "true")::
				 ("include_rts", "false")::[] in
	let raw_query = List.fold_left
				(fun acc (x, y) -> x ^ "=" ^ y ^ "&" ^ acc) "" params in
	let query = "?" ^ (String.sub raw_query 0 ((String.length raw_query) - 1)) in
	let buf_ref = get (user_timeline_url ^ query) params in
	Buffer.contents !buf_ref
