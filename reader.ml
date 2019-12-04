#use "pc.ml";;
open PC
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;

(******* Boolean Parser******)
let nt_hashtag  = 
 const (fun ch ->  ch == '#' ) ;;
let nt_false  = 
 const (fun ch ->  ch == 'f' || ch=='F' )  ;;
let nt_true  = 
 const (fun ch ->  ch == 't' || ch='T' ) ;;

 let nt_boolean  =
  pack (caten nt_hashtag (disj nt_false nt_true)  )
  (fun ((e1,e2)) -> 
  let e2= lowercase_ascii e2 in
   match e2 with 'f' -> Bool(false) 
  |'t'-> Bool(true) |  _ -> raise X_no_match )
   ;;  
(*******End of Boolean Parser******)
 (****Numbers Parser ******) 
 let nt_plus  = 
  const (fun ch ->  ch == '+')  ;;
 let nt_minus  = 
  const (fun ch ->  ch == '-' ) ;;

(***** Integer Parser ******)
let make_left nt_left nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  nt;;


let nt_uinteger =
  let make_nt_digit ch_from ch_to =
    let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) in
		      fun ch -> (Char.code ch) - delta) in
    nt in
  let nt = (make_nt_digit '0' '9') in
  let nt = plus nt in
  let nt = pack nt (fun digits ->
        List.fold_left (fun a b ->  10* a + b
        ) 0 digits) in
  let nt = caten (word_ci "") nt in
  let nt = pack nt (function (_, e) -> e) in
  nt;;
  



  let nt_leading_zeros=  star (char '0');;

  let nt_integer str = 
    let (e,s) = (maybe(disj nt_plus nt_minus) str) in
    match e with
     Some('-') -> pack nt_uinteger (fun e1-> Number(Int(-e1))) s
    | _ -> pack nt_uinteger (fun e1->  Number(Int(e1))) s;;
     

 (***** End Of Integer Parser ****)

   (*** Float Parser***)
   let nt_frac =
    let make_nt_digit ch_from ch_to =
      let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
      let nt = pack nt (let delta = (Char.code ch_from) in
            fun ch -> (Char.code ch) - delta) in
      nt in
    let nt = (make_nt_digit '0' '9') in
    let nt = plus nt in
    let nt = pack nt (fun digits ->
          List.fold_right (fun a b ->0.1*. b +. a
          )  (List.map (fun i -> (float_of_int i)) digits) 0.) in
    let nt = caten (word_ci ".") nt in
    let nt = pack nt (function (_, e) -> e) in
    nt;;
    
    
    let nt_float str =
      let (e,s) = (maybe(disj nt_plus nt_minus) str) in
       match e with 
       Some('-') -> pack (caten nt_uinteger nt_frac)
       (fun (e1,e2) ->  Number(Float(-.((float_of_int e1)+.(e2*.0.1))))) s 
        | _  -> pack (caten nt_uinteger nt_frac)
       (fun (e1,e2) ->  Number(Float((float_of_int e1)+.(e2*.0.1)))) s ;;

      (***** End Of Float Parser *****)

      
      (****End Of Numbers Parser ****)  

      (***** Symbol Parser ****)


      let nt_digit  = (fun ch -> (range '0' '9') ch) ;;
      let nt_lowercaseLetter  = (fun ch -> (range 'a' 'z') ch) ;;
      let nt_uppercaseLetter  = (fun ch -> (range 'A' 'Z') ch) ;;
      let nt_simanKria  = const (fun ch -> ch = '!') ;;
      let nt_dollar  = const (fun ch -> ch = '$') ;;
      let nt_power  = const (fun ch -> ch = '^') ;;
      let nt_star  = const (fun ch -> ch = '*') ;;
      let nt_makaf  = const (fun ch -> ch = '-') ;;
      let nt_underscore  = const (fun ch -> ch = '_') ;;
      let nt_equal  = const (fun ch -> ch = '=') ;;
      let nt_gadol  = const (fun ch -> ch = '<') ;;
      let nt_katan  = const (fun ch -> ch = '>') ;;
      let nt_sheala  = const (fun ch -> ch = '?') ;;
      let nt_slash  = const (fun ch -> ch = '/') ;;
      let nt_dots  = const (fun ch -> ch = ':') ;;      
      let nt_note  = const (fun ch -> (lowercase_ascii ch) != 'e') ;;  
      let nt_notr  = const (fun ch -> (lowercase_ascii ch) != 'r') ;;  
      let nt_whitespace  = const (fun ch -> ch == ' ') ;;

      let nt_symbolchar  =
      pack 
      (disj_list [nt_digit; nt_lowercaseLetter; nt_uppercaseLetter; nt_simanKria; nt_dollar;
      nt_power; nt_star; nt_makaf; nt_underscore; nt_equal; nt_plus; nt_gadol; nt_katan; nt_sheala; nt_slash; nt_dots]) 
      (fun ch -> lowercase_ascii ch);;
    

  
        
      let nt_number  = 
      not_followed_by (disj nt_float nt_integer) (disj_list [nt_lowercaseLetter; nt_uppercaseLetter; nt_simanKria; nt_dollar;
      nt_power; nt_star; nt_makaf; nt_underscore; nt_equal; nt_plus; nt_gadol; nt_katan; nt_sheala; nt_slash; nt_dots])  ;;
       
      let nt_symbol = 
        pack (plus nt_symbolchar) (fun x -> Symbol(list_to_string x));;

        let nt_symbol_string = 
          pack (plus nt_symbolchar) (fun x -> list_to_string x);;
        
      (**** End Of Symbol ****)

      (* Char Parser *)
      let nt_char_prefix = (word_ci "#\\");;
      
      let nt_vis_char = 
        pack (const (fun ch -> ch > ' ')) (fun x -> Char(x));;
    
      let nt_newline = pack (word_ci "newline") (fun _ -> Char('\n'));;
      
      let nt_nul = pack (word_ci "nul") (fun _ -> Char((char_of_int 0)));;
      
      let nt_page = pack (word_ci "page") (fun _ -> Char((char_of_int 12)));;
      
      let nt_return = pack (word_ci "return") (fun _ -> Char('\r'));;
      let nt_space = pack (word_ci "space") (fun _ -> Char(' '));;
      let nt_tab = pack (word_ci "tab") (fun _ -> Char('\t'));;

      let nt_named_char str = disj_list [nt_newline;nt_nul;nt_page;nt_return;nt_space;nt_tab;] str ;;
      
      let nt_char = pack (caten nt_char_prefix (disj_list [nt_named_char;nt_vis_char;])) (fun (x1,x2)->x2);;
      (*End Of Char Parser*)

 
   
      (**String Parser**)
     let nt_par = pack (const (fun ch -> ch == '"')) (fun x-> Char('"'));;


      let make_paired nt_left nt_right nt =
        let nt = caten nt_left nt in
        let nt = pack nt (function (_, e) -> e) in
        let nt = caten nt nt_right in
        let nt = pack nt (function (e, _) -> e) in
        nt;;

        
        let nt_meta_slash_quote = pack (word "\\\"") (fun  x->char_of_int 34 );;

        let nt_double_metaslash = pack (word "\\\\") (fun  x->char_of_int 92 );;
        let nt_metaT = pack (word "\\t") (fun  x-> char_of_int 9 );;
        let nt_metaR = pack (word "\\r") (fun  x->char_of_int 13);;
        let nt_metaF = pack (word "\\f") (fun  x->char_of_int 12);;
        let nt_metaN = pack (word "\\n") (fun  x->char_of_int 10);;
        let nt_meta_char = disj_list [nt_double_metaslash;nt_metaT;nt_metaR;nt_metaF;nt_metaN;nt_meta_slash_quote;];;

        let nt_literal_char = 
        pack (const (fun ch -> ch != '\\' && ch!='"')) (fun ch -> if((Char.code ch)<(Char.code ' ')) then char_of_int 32 else ch);;
     

        let nt_string_char = disj nt_literal_char nt_meta_char;;

        let nt_string = pack (make_paired nt_par nt_par (star nt_string_char)) (fun x-> String(list_to_string x));;      
      (***End of String Parser***)
      let nt_whitespaces= star(char ' '  );;
       let tok_lparen= char '(';;
       let tok_rparen= char ')' ;;
        

       let nt_dot = disj (word ". ") (word ".");;

       let rec paircreator x = match x with
       [] -> Nil
      | first::rest-> Pair(first,(paircreator rest));;
      
      let rec paircreatordotted x y = match x with
      [] -> y
     | first::rest-> Pair(first,(paircreatordotted rest y));;


     let rec checkRef x y = match y with
     TaggedSexpr(name1, expr1) -> if (x = name1) then false else true
     | Pair(a,b) -> (checkRef x a) && (checkRef x b)
     | _ -> true;;


      let  nt_spaces nt = 
      make_paired nt_whitespaces nt_whitespaces nt;;

      let quoted = char (char_of_int (39));;
      let quasiQuoted = (char '`');;
      let unquoted = (char ',');;
      let unquoteAndSpliced = (word ",@");;
      
      let tok_lparenTurbo = char '{';;
      let tok_rparenTurbo = char '}' ;;

      let nt_tag = pack (caten nt_hashtag (make_paired tok_lparenTurbo tok_rparenTurbo nt_symbol_string) ) (fun (x,y) -> TagRef(y) );;
      let nt_tag_str = pack (caten nt_hashtag (make_paired tok_lparenTurbo tok_rparenTurbo nt_symbol_string) ) (fun (x,y) -> y );;

       (**** Scientific Number ***) 
      let nt_Lscinumber  = 
      pack (caten (not_followed_by (disj nt_float nt_integer) nt_note) (char_ci 'e')) (fun (x,y)->x) ;;
      
      let nt_sciNumber = pack (caten nt_Lscinumber nt_integer) (fun (x,y) -> 
      match x,y with 
      Number(Int n1), Number(Int n2) -> Number(Float((float_of_int n1) *. (10.0 ** float_of_int n2)))
      | Number(Float n1), Number(Int n2) -> Number(Float(n1 *. (10.0 ** float_of_int n2)))
      |  _ -> raise X_no_match ) ;;
      (*** End Of Scientific Number ***) 
      (**** Radix Notation ***) 
      
  
        let list_of_chars_base_convertor_to_int ch_from ch_to displacement =
          let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
          let nt = pack nt (let delta = (Char.code ch_from) - displacement in
                fun ch -> (Char.code ch) - delta) 
                in
          nt;;

        let rangeConvertor  = 
            (disj_list [(list_of_chars_base_convertor_to_int '0' '9' 0);
             (list_of_chars_base_convertor_to_int 'A' 'Z' 10);
             (list_of_chars_base_convertor_to_int 'a' 'z' 10)]);;
            
          
       let nt_getBase =
       pack (caten (pack (not_followed_by (caten nt_hashtag nt_uinteger)   nt_notr) (fun (x,y)-> y)) (char_ci 'r')) (fun (x,y)-> x) ;;



       let nt_numericBB  =
        let nt = rangeConvertor in
        let nt = plus nt in
        let nt = caten (word_ci "") nt in
        let nt = pack nt (function (_0x, e) -> e) in
        nt ;;

      let nt_radixIntegers = 
        pack (caten nt_getBase nt_numericBB) (fun (base,digits)-> List.fold_left (fun a b -> if(b<base) then (base * a + b) else raise X_no_match) 0 digits);;
       
      let nt_radixFloats = pack (caten (caten nt_getBase nt_numericBB) (caten nt_dot nt_numericBB)) 
      (fun ((base,digits),(dot,fracDigits)) -> float_of_int(List.fold_left (fun a b -> if(b<base) then (base * a + b) else raise X_no_match) 0 digits)
       +. List.fold_right (fun a b -> if((float_of_int base)>a) then (a /. (float_of_int base) +. b) else raise X_no_match)  (List.map (fun i -> (float_of_int i)) fracDigits) 0.);;


      (**** Radix Notation ***) 
              (******main function*********)
      let  rec nt_sexps s = nt_spaces (disj_list [nt_sexpr_comment;nt_semicolon ;nt_boolean; nt_char; nt_number; nt_string; nt_symbol; nt_list; nt_dottedList; nt_quoted;
      nt_quasiQuoted; nt_unquoted; nt_unquoteAndSpliced; nt_taggedExpr; nt_tag])  s

       and nt_sexps_no_comments s = disj_list [nt_boolean; nt_char; nt_number; nt_string; nt_symbol; nt_list; nt_dottedList; nt_quoted;
       nt_quasiQuoted; nt_unquoted; nt_unquoteAndSpliced; nt_taggedExpr; nt_tag]  s
        and nt_list s = pack (make_paired tok_lparen tok_rparen ((star  (nt_spaces nt_sexps)))) 
        (fun x-> paircreator(x) ) s

        (*** End list ***)

   
        
        and nt_dottedList s = pack (caten (make_paired tok_lparen nt_dot (plus (nt_spaces nt_sexps) )) 
                          (make_paired nt_epsilon tok_rparen nt_sexps)) (fun (x,y)-> (paircreatordotted x y) ) s

        and nt_quoted s = pack (caten quoted nt_sexps) (fun (x,y)-> Pair(String("quote"),y)) s
        and nt_quasiQuoted s = pack (caten quasiQuoted nt_sexps) (fun (x,y)-> Pair(String("quasiquote"),y)) s
        and nt_unquoted s = pack (caten unquoted nt_sexps) (fun (x,y)-> Pair(String("unquote"),y)) s
        and nt_unquoteAndSpliced s = pack (caten unquoteAndSpliced nt_sexps) (fun (x,y)-> Pair(String("unquote-splicing"),y)) s
        
        and nt_taggedExpr s = pack (caten nt_tag_str (caten (nt_spaces (char '=')) nt_sexps )) (fun (x,(y1,y2))-> let ans = (checkRef x y2) in
        match ans with 
        true -> TaggedSexpr(x, y2)
        | _ -> raise X_this_should_not_happen) s
        
        
        and nt_semicolon_end s = pack (caten (nt_spaces nt_sexps_no_comments) (make_paired (char ';') nt_end_of_input (star nt_sexps))) (fun (x,y) -> x) s
        and nt_semicolon_newline s = pack (caten (nt_spaces nt_sexps_no_comments) (make_paired (char ';') (char '\n') (star nt_sexps))) (fun (x,y) -> x) s
        and nt_semicolon s =  disj nt_semicolon_newline nt_semicolon_end s

        
        and nt_sexpr_comment s = 
        pack (caten (pack (caten (word "#;") 
        (caten (disj (delayed (fun _ -> nt_sexpr_comment))
        (pack nt_epsilon (fun _ -> Nil)))
        (nt_spaces nt_sexps)))
        (fun _ -> Nil )) (nt_spaces nt_sexps)) (fun (x,y)-> y) s;;
  (****** end of main function*********)
        
     let lior = (star nt_sexps);; 
module Reader: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let read_sexpr string = 
  let (e,s)= (nt_sexps (string_to_list (string))) in e;;

let read_sexprs string = 
  let (e,s)= (star nt_sexps (string_to_list (string))) in e;;
  
end;; (* struct Reader *)
