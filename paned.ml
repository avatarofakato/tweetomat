(* file: paned.ml *)
open Printf
open String
open List
open Parser
open Pervasives
open Sql

let cols_subs = new GTree.column_list
let str_col_subs = cols_subs#add Gobject.Data.string

let cols_tweets = new GTree.column_list
let str_col_tweets = cols_tweets#add Gobject.Data.string

let refresh_subs_list dbh (model:#GTree.list_store) =
	model#clear();
	List.iter (fun str ->
		let it = model#append () in
		model#set ~row:it ~column:str_col_subs str;) (get_subscribers_names dbh)

let refresh_tweets_list dbh (model:#GTree.list_store) sub_ids =
	printf "refresh_tweets_list called\n";
	model#clear();
	List.iter (fun str -> printf "****\n%s\n****\n" str; flush stdout;
		let it = model#append () in
		model#set ~row:it ~column:str_col_tweets str) (get_tweets_ids dbh sub_ids)

let selection_changed_subs dbh (model1:#GTree.list_store) (model2:#GTree.list_store) selection () =
	let pr path =
		let row = model1#get_iter path in
		let name = model1#get ~row ~column:str_col_subs in
		Printf.sprintf "%s" name;
	in
	let selected_users = List.map pr selection#get_selected_rows in
	List.iter (fun x -> printf "NICK: %s\n" x; flush stdout) selected_users;
	refresh_tweets_list dbh model2 (List.map (get_id_of_name dbh) selected_users)

let selection_changed_tweets dbh (model:#GTree.list_store) selection (buffer:#GText.buffer) () =
	let pr path =
		let row = model#get_iter path in
		let name = model#get ~row ~column:str_col_tweets in
		(* Printf.printf "Tweet clicked()\n" name; *)
		Printf.printf "Tweet clicked\n";
		flush stdout;
		buffer#delete (buffer#get_iter `START) (buffer#get_iter `END);
		let iter = buffer#get_iter `START in
		(* buffer#insert ~iter (sprintf "%s\n" name) *)
		buffer#insert ~iter (get_tweet_of_id dbh name)
	in
	List.iter pr selection#get_selected_rows

(* vbox #1 *)
let create_list_subs dbh vpaned1 model2 () =
	(* Create a new scrolled window, with scrollbars only if needed *)
	let scrolled_window = GBin.scrolled_window
		~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () ~height:300 in

	let model = GTree.list_store cols_subs in
	let treeview = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) () in

	List.iter (fun str ->
		let it = model#append () in
		model#set ~row:it ~column:str_col_subs str;) (get_subscribers_names dbh);
	let renderer = GTree.cell_renderer_text [] in
	let column = GTree.view_column ~title:"Subscriptions"
		~renderer:(renderer, ["text", str_col_subs]) () in
	treeview#append_column column;
	treeview#selection#set_mode `MULTIPLE;
	treeview#selection#connect#changed
		~callback:(selection_changed_subs dbh model model2 treeview#selection);
	vpaned1#add1 scrolled_window#coerce;
	model

(* vbox #2 *)
let create_list_tweets dbh vpaned2 buffer () =
	(* Create a new scrolled window, with scrollbars only if needed *)
	let scrolled_window = GBin.scrolled_window
		~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () ~height:300 in

	let model = GTree.list_store cols_tweets in
	let treeview = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) () in

	List.iter 
		(fun str ->
		let it = model#append () in
		model#set ~row:it ~column:str_col_tweets str;)
		(get_tweets_ids dbh (get_subscribers_ids dbh));

	let renderer = GTree.cell_renderer_text [] in
	let column = GTree.view_column ~title:"Tweets"
		~renderer:(renderer, ["text", str_col_subs]) () in
	treeview#append_column column;
	treeview#selection#set_mode `SINGLE;
	treeview#selection#connect#changed
		~callback:(selection_changed_tweets dbh model treeview#selection buffer);
	vpaned2#add1 scrolled_window#coerce;
	model


(* vbox #3 *)
let insert_text (buffer: GText.buffer) =
	let iter = buffer#get_iter `START in
	buffer#insert ~iter (
		"From: pathfinder@nasa.gov\n" ^
		"To: mom@nasa.gov\n" ^
		"Subject: Made it!\n" ^
		"\n" ^
		"We just got in this morning. The weather has been\n" ^
		"great - clear but cold, and there are lots of fun sights.\n" ^
		"Sojourner says hi. See you soon.\n" ^
		" -Path\n")

let rem_single_sub entry () =
	printf "rem_single_sub called %s\n" entry#text;
	flush stdout;
	()

let download_tweets dbh () =
	(* let parsed_tweets = get_parsed_tweets ~number_of_tweets:200 ~user:entry#text in *)
	(* List.iter (fun tweet -> add_tweet tweet dbh ()) parsed_tweets; *)
	(* printf "download_tweets called %s\n" entry#text; *)
	printf "download_tweets called\n";
	flush stdout;
	()

(* Create a scrolled text area that displays a "message" *)
let create_text buffer scrolled_window () =
	insert_text buffer;
	scrolled_window#coerce

(* let make_first_column hbox dbh vbox1 = *)
	(* entry *)
let add_user entry dbh model () =
	add_subscription entry dbh ();
	refresh_subs_list dbh model

(* let make_second_column hbox dbh buffer = *)
let make_third_column hbox buffer scrolled_window =
	let vbox3 = GPack.vbox ~spacing:5 ~packing:hbox#add ~width:335 () in
	let text = create_text buffer scrolled_window () in
	vbox3#add text

let main () =
	let dbh = PGOCaml.connect ~host:"localhost"  ~user:"adam" 
	~password:"adamb.93" ~database:"adam" () in
	let window = GWindow.window ~title:"Tweetomat" ~border_width:10
		~width:650 ~height:400 () in
	window#connect#destroy ~callback:GMain.Main.quit;

	let scrolled_window3 = GBin.scrolled_window
		~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
	let view3 = GText.view ~packing:scrolled_window3#add () in
	let buffer3 = view3#buffer in
	let hbox = GPack.hbox ~spacing:5 ~packing:window#add () in

	let vbox1 = GPack.vbox ~spacing:5 ~packing:hbox#add ~width:150 () in
	let vbox2 = GPack.vbox ~spacing:5 ~packing:hbox#add ~width:165 () in
	
	(* #2 column *)
	let vpaned2 = GPack.paned `VERTICAL ~packing:vbox2#add () in
	(* Button *)
	let button1 = GButton.button ~label:"Get tweets" ~packing:vbox2#add () in
	ignore (button1#connect#clicked ~callback:(download_tweets dbh));
	(* let model2 = create_list_tweets dbh vpaned2 () in *)
	let model2 = create_list_tweets dbh vpaned2 buffer3 () in

	(* #1 column *)
	let vpaned1 = GPack.paned `VERTICAL ~packing:vbox1#add () in
	(* Entry *)
	let entry = GEdit.entry ~text:"Type nick here..." ~max_length:140 ~packing:vbox1#add () in
	(* Scrolled list *)
	let model1 = create_list_subs dbh vpaned1 model2 () in
	(* Buttons *)
	let button1 = GButton.button ~label:"Subscribe" ~packing:vbox1#add () in
	ignore (button1#connect#clicked ~callback:(add_user entry dbh model1));
	let button2 = GButton.button ~label:"Unsubscribe" ~packing:vbox1#add () in
	ignore (button2#connect#clicked ~callback:(rem_single_sub entry));
	(* vpaned1#add1 _list; *)

	(* let entry = make_first_column hbox dbh vbox1 in *)
	(* make_second_column hbox dbh buffer3; *)
	make_third_column hbox buffer3 scrolled_window3;

	window#show ();
	GMain.Main.main ()

let _ = Printexc.print main ()
