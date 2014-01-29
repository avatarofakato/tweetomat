(* TODO clean and delete finally unused fragments *)
let (|>) x f = f x

exception Error of Nethttp.http_status * string

let opt_param name param =
  match param with
    | None -> []
    | Some p -> [name, p]

let rng = Cryptokit.Random.device_rng "/dev/random"

let rfc3986_encode s = Netencoding.Url.encode s
let rfc3986_decode s = Netencoding.Url.decode s

let string_of_http_method = function
  | `Get -> "GET"
  | `Post -> "POST"
  | `Head -> "HEAD"

let string_of_signature_method = function
  | `Plaintext -> "PLAINTEXT"
  | `Hmac_sha1 -> "HMAC-SHA1"
  | `Rsa_sha1 _ -> "RSA-SHA1"

let signature_method_of_string rsa_key = function
  | "PLAINTEXT" -> `Plaintext
  | "HMAC-SHA1" -> `Hmac_sha1
  | "RSA-SHA1"  -> `Rsa_sha1 (rsa_key ())
  | _ -> raise Not_found

let normalize_url url =
  let url = Neturl.parse_url ~enable_fragment:true url in
  let url = Neturl.remove_from_url ~query:true ~fragment:true url in
  Neturl.string_of_url url

let string_of_timestamp t =
  let s = string_of_float t in
  String.sub s 0 (String.length s - 1)

let make_timestamp () = Unix.time ()

let make_nonce () =
  Cryptokit.Random.string rng 16 |>
      Cryptokit.transform_string (Cryptokit.Hexa.encode ())

let base64_encode v =
  let b64 = Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) v in
  b64 ^ "="

let base64_decode v =
  Cryptokit.transform_string (Cryptokit.Base64.decode ()) v

let hmac_sha1_hash text key =
  text |>
      Cryptokit.hash_string (Cryptokit.MAC.hmac_sha1 key) |>
          base64_encode

let sha1_digest_info h =
  "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14" ^ h

let pkcs1_pad rsa_key v =
  let tLen = String.length v in
  let emLen = rsa_key.Cryptokit.RSA.size / 8 in
  "\x00\x01" ^ String.make (emLen - tLen - 3) '\xff' ^ "\x00" ^ v

let rsa_sha1_hash text rsa_key =
  text |>
      Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) |>
          sha1_digest_info |>
              pkcs1_pad rsa_key |>
                  Cryptokit.RSA.sign rsa_key |>
                      base64_encode

let check_rsa_sha1_hash text rsa_key signature =
  try
    (text |>
        Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) |>
            sha1_digest_info |>
                pkcs1_pad rsa_key) =
    (signature |>
        base64_decode |>
            Cryptokit.RSA.unwrap_signature rsa_key)
  with _ -> false

let signature_base_string
    ~http_method ~url
    ~oauth_signature_method
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?(params = [])
    () =

  let params = [
    "oauth_signature_method", string_of_signature_method oauth_signature_method;
    "oauth_consumer_key", oauth_consumer_key;
    "oauth_timestamp", string_of_timestamp oauth_timestamp;
    "oauth_nonce", oauth_nonce;
      "oauth_version", oauth_version;
  ] @
    opt_param "oauth_token" oauth_token @
    List.filter (fun (k, v) -> k <> "oauth_signature") params in

  List.map rfc3986_encode
    [
      string_of_http_method http_method;
      normalize_url url;

      params |>
          List.map (fun (k, v) -> rfc3986_encode k, rfc3986_encode v) |>
              List.sort (fun (k,v) (k',v') ->
                match String.compare k k' with
                  | 0 -> String.compare v v'
                  | c -> c) |>
                  List.map (fun (k,v) -> k ^ "=" ^ v) |>
                      String.concat "&"
    ] |> String.concat "&"



let sign
    ~http_method ~url
    ~oauth_signature_method
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?params
    () =

  let key =
    (rfc3986_encode oauth_consumer_secret ^ "&" ^
        match oauth_token_secret with
          | None -> ""
          | Some s -> rfc3986_encode s) in

  let signature_base_string =
    signature_base_string
      ~http_method ~url
      ~oauth_signature_method
      ~oauth_consumer_key ~oauth_consumer_secret
      ?oauth_token ?oauth_token_secret
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?params
      () in

  match oauth_signature_method with
    | `Plaintext -> rfc3986_encode key
    | `Hmac_sha1 -> hmac_sha1_hash signature_base_string key
    | `Rsa_sha1 rsa_key -> rsa_sha1_hash signature_base_string rsa_key



let check_signature
    ~http_method ~url
    ~oauth_signature_method ~oauth_signature
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?params
    () =

  let key =
    (rfc3986_encode oauth_consumer_secret ^ "&" ^
        match oauth_token_secret with
          | None -> ""
          | Some s -> rfc3986_encode s) in

  let signature_base_string =
    signature_base_string
      ~http_method ~url
      ~oauth_signature_method
      ~oauth_consumer_key ~oauth_consumer_secret
      ?oauth_token ?oauth_token_secret
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?params
      () in

  match oauth_signature_method with
    | `Plaintext -> rfc3986_encode key = oauth_signature
    | `Hmac_sha1 -> hmac_sha1_hash signature_base_string key = oauth_signature
    | `Rsa_sha1 rsa_key -> check_rsa_sha1_hash signature_base_string rsa_key oauth_signature

let request
    ?(http_method = `Post)
    ~url
    ?(headers = [])
    ?(params = [])
    ?body
    () =
  let call =
    match http_method, body with
      | `Post, None ->
          new Http_client.post url params
      | `Post, Some (content_type, body) ->
          let query = Netencoding.Url.mk_url_encoded_parameters params in
          let url = url ^ (if query <> "" then "?" ^ query else "") in
          let call = new Http_client.post_raw url body in
          (call#request_header `Base)#update_field "Content-type" content_type;
          call
      | `Get, _ | `Head, _ ->
          let query = Netencoding.Url.mk_url_encoded_parameters params in
          let url = url ^ (if query <> "" then "?" ^ query else "") in
          match http_method with
            | `Get  -> new Http_client.get url
            | `Head -> new Http_client.head url
            | `Post -> assert false in

  let h = call#request_header `Base in
  List.iter (fun (k,v) -> h#update_field k v) headers;

  let pipeline = new Http_client.pipeline in

  pipeline#add call;
  pipeline#run();

  (call#response_status, call#response_header#fields, call#response_body#value)

let authorization_header
      ~oauth_version ~oauth_signature_method ~oauth_signature
      ~oauth_consumer_key ?oauth_token
      ~oauth_timestamp ~oauth_nonce
      () =
    let params =
      [
        (* "OAuth realm", ""; *)
        "oauth_version", oauth_version;
        "oauth_signature_method", string_of_signature_method oauth_signature_method;
        "oauth_signature", oauth_signature;
        "oauth_consumer_key", oauth_consumer_key;
        "oauth_timestamp", string_of_timestamp oauth_timestamp;
        "oauth_nonce", oauth_nonce;
      ] @
        opt_param "oauth_token" oauth_token in

    "Authorization",
    (params |>
        List.map (fun (k, v) -> k ^ "=\"" ^ String.escaped (rfc3986_encode v) ^ "\"") |>
            String.concat ",")


  let access_resource
      ?(http_method = `Post) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_consumer_key ~oauth_consumer_secret
      ~oauth_token ~oauth_token_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?params ?body
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_token ~oauth_token_secret
        ~oauth_timestamp ~oauth_nonce
        ?params
        () in

    let headers = [
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_consumer_key ~oauth_token
        ~oauth_timestamp ~oauth_nonce
        ()
    ] in

    let res =
      request
        ~http_method
        ~url
        ~headers
        ?params
        ?body
        () in

    match res with
      | (`Ok, _, res) -> res
      | (status, _, res) -> raise (Error (status, res))

(* TODO exclude that somewhere (the best thing would be
 * to force user to enter the data on the first run). *)
let ock = "t0BV6tMZCYfOCEMxkv38sA"
let ocs = "id4eFPQ4JM6l3MCm7UJ2OtxOGBLl1Wu2fNA4ywzIzQ"
let ot = "2273906498-DPipv1KN04H3xVdxhISBfaLeoOx6Uobv4WWgO5t"
let ots = "W3VKYFX8q27SGtPviD0BAztaYJ2LUH8D8nb0KHoj3HgKR"

(* TODO exceptions and change arguments to ~arg syntax *)
let get_twitter_oauth url params =
    let oauth_timestamp = (make_timestamp ()) in
    let oauth_nonce = (make_nonce ()) in
    let oauth_signature = sign
        ~http_method:`Get ~url:url
        ~oauth_signature_method:`Hmac_sha1
        ~oauth_consumer_key:ock ~oauth_consumer_secret:ocs
        ~oauth_token:ot ~oauth_token_secret:ots
        ~oauth_timestamp:oauth_timestamp ~oauth_nonce:oauth_nonce
        ~oauth_version:"1.0"
        ~params:params () in
    let headers = authorization_header
        ~oauth_version:"1.0"
        ~oauth_signature_method:`Hmac_sha1
        ~oauth_signature:oauth_signature
        ~oauth_consumer_key:ock
        ~oauth_token:ot
        ~oauth_timestamp:oauth_timestamp ~oauth_nonce:oauth_nonce () in
    "Authorization: OAuth " ^ (match headers with (_, y) -> y)

(* TODO clean a little *)
(* let url = "https://api.twitter.com/1.1/statuses/user_timeline.json" *)
(* let param = ("screen_name", "sikorskiradek")::("count", "1")::[] *)

(* let str = get_twitter_oauth url param *)

(* let () = *)
    (* Printf.printf "%s\n" str; *)

(*************************************************************************)
(* let oauth_timestamp = (make_timestamp ()) *)
(* let oauth_nonce = (make_nonce ()) *)
(* let oauth_signature = sign *)
    (* ~http_method:`Get ~url:url *)
    (* ~oauth_signature_method:`Hmac_sha1 *)
    (* ~oauth_consumer_key:ock ~oauth_consumer_secret:ocs *)
    (* ~oauth_token:ot ~oauth_token_secret:ots *)
    (* ~oauth_timestamp:oauth_timestamp ~oauth_nonce:oauth_nonce *)
    (* ~oauth_version:"1.0" *)
    (* ~params:param () *)

(* let headers = authorization_header *)
    (* ~oauth_version:"1.0" *)
    (* ~oauth_signature_method:`Hmac_sha1 *)
    (* ~oauth_signature:oauth_signature *)
    (* ~oauth_consumer_key:ock *)
    (* ~oauth_token:ot *)
    (* ~oauth_timestamp:oauth_timestamp ~oauth_nonce:oauth_nonce () *)

(* let data = *)
    (* let raw = List.fold_left (fun acc (x, y) -> x ^ "=" ^ y ^ "&" ^ acc) "" param in *)
    (* String.sub raw 0 ((String.length raw) - 1) *)

(* let outstr = "curl --get '" ^ url ^ "' --data '" ^ data ^ "' --header 'Authorization: OAuth " ^ (match headers with (_, y) -> y) ^ "' --verbose" *)
(* ;; *)
(* Printf.printf "oauth_signature = %s\n\nheaders = (%s, %s)\n\n" oauth_signature *)
(* (match headers with (x, y) -> x) (match headers with (x, y) -> y);; *)

(* Printf.printf "out =\n%s\n\n" outstr;; *)
