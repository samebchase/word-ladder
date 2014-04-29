open Core.Std
open Graph
open BatString

let alphabet = "abcdefghijklmnopqrstuvwxyz"
let file_path = "../lisp/wordsEn.txt"  

let load_words_into_set file_path =
  open_in file_path
  |> Std.input_list
  |> List.map ~f:(fun str -> String.drop_suffix str 1)
  |> String.Set.of_list      
;;

let dictionary = load_words_into_set file_path

let is_valid_dictionary_word word =
  String.Set.mem dictionary word
;;

let drop_char_from_string c str =
  String.filter str ~f:(fun string_char -> string_char <> c)
;;  

let word_neighbours word =
  let neighbours = Hash_set.Poly.create () in
  BatString.iteri
    (fun word_idx word_char ->
      BatString.iter
        (fun alphabet_char ->
          let copied_string = (String.copy word) in
          String.set copied_string word_idx alphabet_char;
          if is_valid_dictionary_word copied_string
          then Hash_set.add neighbours copied_string)
        (drop_char_from_string word_char alphabet))
    word;
  neighbours
;;

module Vertex = struct
  type t = string
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Edge = struct
  type t = int
  let compare = Pervasives.compare
  let default = 1
end

module Weight = struct
  type label = int
  type t = int
  let weight x = x
  let compare = Pervasives.compare
  let add = (+)
  let zero = 0
end

module G = Imperative.Graph.ConcreteLabeled(Vertex)(Edge)

let word_graph = G.create ()

module Dij = Path.Dijkstra(G)(Weight)

let load_nodes_into_graph word =
  G.add_vertex word_graph (G.V.create word);
  
  let equal_length_words =
    String.Set.filter dictionary
      ~f:(fun dictionary_word ->
        phys_equal (String.length word) (String.length dictionary_word)) in
  String.Set.iter
    ~f:(fun neighbour_word -> G.add_vertex word_graph (G.V.create neighbour_word))
    equal_length_words;
;;                  

let load_edges_into_graph word =
  load_nodes_into_graph word;
  G.iter_vertex
    (fun word_vertex ->
      Hash_set.iter
        ~f:(fun neighbour_word ->
          G.add_edge_e word_graph (G.E.create word_vertex 1 neighbour_word))
        (word_neighbours word_vertex))
    word_graph
;;

let load_words_into_graph word =
  load_edges_into_graph word
;;

let generate_word_ladder word_a word_b =
  load_words_into_graph word_a;
  Dij.shortest_path word_graph word_a word_b
;;

let () =
  List.iter (fst (generate_word_ladder Sys.argv.(1) Sys.argv.(2)))
    ~f:(fun edge ->
      match edge with
          (src, _weight, dest) ->
            Core.Std.printf "%s -> %s, " src dest;);
  print_newline ()
;;
