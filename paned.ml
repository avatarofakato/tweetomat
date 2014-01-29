(* file: paned.ml *)
open Printf
open String
open List

let cols_subs = new GTree.column_list
let str_col_subs = cols_subs#add Gobject.Data.string

let cols_tweets = new GTree.column_list
let str_col_tweets = cols_tweets#add Gobject.Data.string

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

let create_model_subs data =
	let store = GTree.list_store cols_tweets in
	let append name =
		let row = store#append () in
		store#set ~row ~column:str_col_tweets name;
	in
	List.iter append data;
	store

let add_single_sub entry (store:#GTree.list_store) () =
	let insert = entry#text in
		let append name =
			let row = store#append () in
			store#set ~row ~column:str_col_tweets name in
	append insert;
	printf "%s\n" insert;
	flush stdout;
	()

let selection_changed_subs (model:#GTree.model) selection () =
	let pr path =
		let row = model#get_iter path in
		let name = model#get ~row ~column:str_col_subs in
		Printf.printf "Nick: %s\n" name;
		flush stdout
	in
	List.iter pr selection#get_selected_rows

let selection_changed_tweets (model:#GTree.model) selection (buffer: GText.buffer) () =
	let pr path =
		let row = model#get_iter path in
		let name = model#get ~row ~column:str_col_tweets in
		let iter = buffer#get_iter `START in
		buffer#insert ~iter ("HUEHEU\n")
	in
	List.iter pr selection#get_selected_rows

let create_view_subs buffer ~model ~packing () =
	(* let scrolled_window = GBin.scrolled_window *)
	(* ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () ~height:300 in *)
	(* let view = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) () in *)
	let view = GTree.view ~model ~packing () in
	let renderer = GTree.cell_renderer_text [] in
	let col = GTree.view_column ~title:"Subscribtions"
		~renderer:(renderer, ["text", str_col_tweets]) () in
	view#append_column col;
	view#selection#set_mode `SINGLE;
	(* TODO dodac opcje multiple w przypadku userow *)
	(* view#selection#set_mode `MULTIPLE; *)
	view#selection#connect#changed ~callback:(selection_changed_tweets model
		view#selection buffer);
	view

(* Returns list of subscribers *)
let get_subscribers dbh =
	let goal = ref [] in
	let parse src acc =
		acc := (List.map 
			(function
				| None -> "NULL"
				| Some str -> sprintf "%S" str) src)::(!acc)in
	let query = "select name from TwitterUsers" in
	(* let name = "get_subscribers" in *)
	ignore (PGOCaml.prepare dbh ~query ());
	PGOCaml.cursor dbh ~params:[] 
	(fun row -> parse row goal);
	List.map cut_quot_marks (List.flatten !goal)

let get_user_list dbh goal =
	let parse src goal =
		printf "xD\n";
		flush stdout;
		goal := (List.map 
			(function
				| None -> "NULL"
				| Some str -> sprintf "%S" str) src)::(!goal) in
	let query = "select name from TwitterUsers" in
	let name = "TwitterUsers" in
	ignore (PGOCaml.prepare dbh ~query ~name ());
	PGOCaml.cursor dbh ~name ~params:[] (fun row -> parse row goal);
	()

(* vbox #1 *)
let create_list dbh () =
	(* Create a new scrolled window, with scrollbars only if needed *)
	let scrolled_window = GBin.scrolled_window
		~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () ~height:300 in

	let model = GTree.list_store cols_subs in
	let treeview = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) () in

	List.iter (fun str ->
		let it = model#append () in
		model#set ~row:it ~column:str_col_subs str;) (get_subscribers dbh);
	let renderer = GTree.cell_renderer_text [] in
	let column = GTree.view_column ~title:"Subscriptions"
		~renderer:(renderer, ["text", str_col_subs]) () in
	treeview#append_column column;
	treeview#selection#set_mode `SINGLE;
	treeview#selection#connect#changed
		~callback:(selection_changed_subs model treeview#selection);
	scrolled_window#coerce

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

let download_tweets dbh () =
	printf "add_single_sub called\n";
	flush stdout;
	List.fold_left ( fun n str -> printf "#%d: %s\n" n (cut_quot_marks str); flush stdout; n + 1) 1 (get_subscribers dbh);
	()

let add_single_sub entry () =
	printf "add_single_sub called %s\n" entry#text;
	flush stdout;
	()

let rem_single_sub entry () =
	printf "rem_single_sub called %s\n" entry#text;
	flush stdout;
	()

(* Create a scrolled text area that displays a "message" *)
let create_text buffer scrolled_window () =
	insert_text buffer;
	scrolled_window#coerce

let make_first_column hbox dbh =
	let vbox1 = GPack.vbox ~spacing:5 ~packing:hbox#add ~width:150 () in
	let vpaned = GPack.paned `VERTICAL ~packing:vbox1#add () in
	let entry = GEdit.entry ~text:"Type nick here..." ~max_length:140 ~packing:vbox1#add () in
	let button1 = GButton.button ~label:"Subscribe" ~packing:vbox1#add () in
	ignore (button1#connect#clicked ~callback:(add_single_sub entry));
	let button2 = GButton.button ~label:"Unsubscribe" ~packing:vbox1#add () in
	let list = create_list dbh () in
	vpaned#add1 list;
	ignore (button2#connect#clicked ~callback:(rem_single_sub entry))

let make_second_column hbox dbh buffer =
	let vbox2 = GPack.vbox ~spacing:5 ~packing:hbox#add ~width:150 () in
	let goal = ref [] in
	get_user_list dbh goal;
	let model = create_model_subs (List.concat !goal) in
	let view = create_view_subs buffer ~model ~packing:vbox2#pack () in
	let button1 = GButton.button ~label:"Get tweets" ~packing:vbox2#add () in
	ignore (button1#connect#clicked ~callback:(download_tweets dbh))

let make_third_column hbox buffer scrolled_window =
	let vbox3 = GPack.vbox ~spacing:5 ~packing:hbox#add ~width:350 () in
	let text = create_text buffer scrolled_window () in
	vbox3#add text

let main () =
	let dbh = PGOCaml.connect ~host:"localhost"  ~user:"adam" 
	~password:"adamb.93" ~database:"adam" () in
	let window = GWindow.window ~title:"Tweetomat" ~border_width:10

		~width:650 ~height:400 () in
	window#connect#destroy ~callback:GMain.Main.quit;

	let scrolled_window = GBin.scrolled_window
		~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
	let view = GText.view ~packing:scrolled_window#add () in
	let buffer = view#buffer in

	let hbox = GPack.hbox ~spacing:5 ~packing:window#add () in
	make_first_column hbox dbh;
	make_second_column hbox dbh buffer;
	make_third_column hbox buffer scrolled_window;

	window#show ();
	GMain.Main.main ()

let _ = Printexc.print main ()
