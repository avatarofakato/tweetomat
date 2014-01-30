(* file selection.ml *)
open Gobject.Data
open Printf
open Gtk
open GtkBase
open GtkTree

let cols_subs = new GTree.column_list
let col_nick = cols_subs#add Gobject.Data.string

let create_model_subs data =
  let store = GTree.list_store cols_subs in
  let append name =
    let row = store#append () in
    store#set ~row ~column:col_nick name;
  in
  List.iter append data;
  store

let add_single_sub entry (store:#GTree.list_store) () =
  let insert = entry#text in
  (* let store = GTree.list_store cols_subs in *)
    let append name =
      let row = store#append () in
      store#set ~row ~column:col_nick name in
  append insert;
  printf "%s\n" insert;
  flush stdout;
  ()

(* let unsubscribe_group (model:#GTree.model) (store:#GTree.list_store) (view:#GTree.view) () = *)
   (* let pr path = *)
    (* let row = model#get_iter path in *)
    (* let name = model#get ~row ~column:col_nick in *)
    (* Printf.printf "Nick: %s\n" name; *)
    (* flush stdout *)
  (* in *)
  (* List.iter (fun x -> store#remove (TreeModel.get_iter model x); ()) view#selection#get_selected_rows *)

let selection_changed_subs (model:#GTree.model) selection () =
  let pr path =
    let row = model#get_iter path in
    let name = model#get ~row ~column:col_nick in
    Printf.printf "Nick: %s\n" name;
    flush stdout
  in
  List.iter pr selection#get_selected_rows

let create_view_subs ~model ~packing () =
  let view = GTree.view ~model ~packing () in
  let col = GTree.view_column ~title:"Subscribtions"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_nick]) () in
  view#append_column col;
  view#selection#set_mode `MULTIPLE;
  view#selection#connect#changed ~callback:(selection_changed_subs model view#selection);
  view

(* let cols = new GTree.column_list *)
(* let col_name = cols#add Gobject.Data.string *)
(* let col_age = cols#add Gobject.Data.int *)

(* let create_model data = *)
  (* let store = GTree.list_store cols in *)
  (* let append (name, age) = *)
    (* let row = store#append () in *)
    (* store#set ~row ~column:col_name name; *)
    (* store#set ~row ~column:col_age age; *)
  (* in *)
  (* List.iter append data; *)
  (* store *)

(* let selection_changed (model:#GTree.model) selection () = *)
  (* let pr path = *)
    (* let row = model#get_iter path in *)
    (* let name = model#get ~row ~column:col_name in *)
    (* let age = model#get ~row ~column:col_age in *)
    (* Printf.printf "XD (%s, %d)\n" name age; *)
    (* flush stdout *)
  (* in *)
  (* List.iter pr selection#get_selected_rows *)

(* let create_view ~model ~packing () = *)
  (* let view = GTree.view ~model ~packing () in *)
  (* let col = GTree.view_column ~title:"Name" *)
      (* ~renderer:(GTree.cell_renderer_text [], ["text", col_name]) () in *)
  (* view#append_column col; *)
  (* let col = GTree.view_column ~title:"Age" *)
      (* ~renderer:(GTree.cell_renderer_text [], ["text", col_age]) () in *)
  (* view#append_column col; *)
  (* view#selection#set_mode `MULTIPLE; *)
  (* view#selection#connect#changed ~callback:(selection_changed model view#selection); *)
  (* view *)

(* let data = [("Kim YooSin", 51); ("King Sejong", 23); *)
  (* ("Kim Goo", 91); ("Jang Young Sil", 44)] *)

(* let count = ref 0 *)

(* let enter_cb entry context dbh () = *)
  (* incr count; *)
  (* let query = *)
    (* "insert into TwitterUsers (id, name, followers, tweetsnumber, priority) *)
    (* values ($1, $2, $3, $4, $5)" in *)
  (* ignore (PGOCaml.prepare dbh ~query ()); *)

  (* let text = entry#text in *)
  (* context#push (Printf.sprintf "Added subscription of %s" text); *)
  (* ignore (PGOCaml.execute dbh *)
         (* ~params:[Some (Printf.sprintf "%d" !count); Some text; Some "42"; Some "42"; Some "42"] ()); *)
  (* () *)


(* let push_item context () = *)
  (* incr count; *)
  (* context#push (Printf.sprintf "item %d" !count); *)
  (* () *)

(* let pop_item context () = *)
  (* context#pop (); *)
  (* () *)

let get_user_list dbh goal =
  let parse src goal =
    goal := (List.map (function
      | None -> "NULL"
      | Some str -> sprintf "%S" str) src)::(!goal) in
  let query = "select name from TwitterUsers" in
  let name = "TwitterUsers" in
  (* let goal = ref [] in *)
  ignore (PGOCaml.prepare dbh ~query ~name ());
  PGOCaml.cursor dbh ~name ~params:[] 
  (fun row -> parse row goal);
  ()

let main () =
  (* Connect with database *)
  let dbh = PGOCaml.connect ~host:"localhost"  ~user:"adam" 
  ~password:"adamb.93" ~database:"adam" () in
  let window = GWindow.window ~title:"Labels" ~border_width:5 () in
  ignore (window #connect#destroy ~callback:GMain.Main.quit);

  let hbox = GPack.hbox ~spacing:5 ~packing:window#add () in

  let vbox1 = GPack.vbox ~spacing:5 ~packing:hbox#add () in
  (* let model = create_model data in *)
  (* :#GTree.model)let view = create_view ~model ~packing:vbox1#pack () in *)

  let statusbar = GMisc.statusbar ~packing:vbox1#add () in
  let context = statusbar#new_context ~name:"Statusbar example" in

  let vbox2 = GPack.vbox ~spacing:5 ~packing:hbox#add () in
  let goal = ref [] in
  get_user_list dbh goal;
  let model = create_model_subs (List.concat !goal) in
  let view = create_view_subs ~model ~packing:vbox2#pack () in

  let entry = GEdit.entry ~text:"hello" ~max_length:500 ~packing:vbox2#add () in
  (* entry#connect#activate ~callback:(enter_cb entry context dbh); *)
  (* entry#connect#activate ~callback:(add_single_sub entry model); *)
  (* let tmp_pos = entry#text_length in *)
  (* entry#insert_text " world" tmp_pos; *)
  (* entry#select_region ~start:0 ~stop:entry#text_length; *)

  let statusbar = GMisc.statusbar ~packing:vbox2#add () in
  let context = statusbar#new_context ~name:"Statusbar example" in

  let button = GButton.button ~label:"Subscribe" ~packing:vbox2#add () in
  ignore (button#connect#clicked ~callback:(add_single_sub entry model));

  (* let button = GButton.button ~label:"Unsubscribe" ~packing:vbox2#add () in *)
  (* ignore (button#connect#clicked ~callback:(unsubscribe_group model model view)); *)

  (* let goal2 = ref [] in *)
  (* get_user_list dbh goal2; *)
  (* printf "%s" (String.concat "# " (List.map (String.concat "$ ") !goal2)); *)
  (* flush stdout; *)
  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main ()
