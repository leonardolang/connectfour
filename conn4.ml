open Owl;;
open Printf;;

(* TODO:
 * - print column headers
 * - ocamldoc for utility functions
 * - revise names?
 *)

module M = Dense.Matrix.S;;

(* Shift table by specific number of rows and/or columns.
 * A positive shift increases the position of the values,
 * a negative one decreases.
 * New rows/columns are added with NaN by default, which
 * can be changed by the "fill" parameter. *)

let shift ?(rows=0) ?(cols=0) ?(fill=nan) mat =
    let (rownum, colnum) = M.shape mat in

    let newrows = fun n -> M.create n colnum fill in
    let newcols = fun n -> M.create rownum n fill in
    let upto n offset = Array.init n (fun e -> e + offset) in

    let mat =
        match rows, (abs rows) with
          (rows, arows) when rows > 0 ->
            let indexes = (upto (rownum - arows) 0) in
            M.concat_vertical (newrows arows) (M.rows mat indexes)
        | (rows, arows) when rows < 0 ->
            let indexes = (upto (rownum - arows) arows) in
            M.concat_vertical (M.rows mat indexes) (newrows arows)
        | (_, _) -> mat
    in
        match cols, (abs cols) with
          (cols, acols) when cols > 0 ->
            let indexes = (upto (colnum - acols) 0) in
            M.concat_horizontal (newcols acols) (M.cols mat indexes)
        | (cols, acols) when cols < 0 ->
            let indexes = (upto (colnum - acols) acols) in
            M.concat_horizontal (M.cols mat indexes) (newcols acols)
        | (_, _) -> mat
;;

let shift_rows mat count =
    shift ~rows:count mat
;;

let shift_cols mat count =
    shift ~cols:count mat
;;

(* Perform "logic and" on all elements of two tables *)

let elt_and_op a b =
    M.map2 (fun a b -> match (a,b) with (1.,1.) -> 1. | _ -> 0.) a b
;;

(* Apply fold-like operation recursively with "exponential" shifts, i.e.:
 *
 *   op(op(op(shift(0,0), shift(rows,cols)), shift(rows*2, cols*2)), ...)
 *
 * Returns the resulting state ofter "count" applications.
 *)

let fold_exp_op op state ?(rows=0) ?(cols=0) count =
    let rec loop state count rows cols =
        match count with
          0 -> state
        | _ ->
            let newstate = shift ~rows:rows ~cols:cols state in
            loop (op newstate state) (count-1) (rows*2) (cols*2)
    in
        loop state count rows cols
;;

(** Game code starts here **)

let check_sequence state rows cols =
    M.exists ((=) 1.0) (fold_exp_op elt_and_op state ~rows:rows ~cols:cols 2)
;;

let check_state state =
    let tbl = [
        ((1,  0 ), "vertical");
        ((0,  1 ), "horizontal");
        ((1,  1 ), "diagonal (\\)");
        ((1,(-1)), "diagonal (/)")
    ] in
        List.fold_left begin fun res ((r,c), str) ->
            if check_sequence state r c then str :: res else res
        end [] tbl

let check_winner board num name =
    let status = M.elt_equal_scalar board (float_of_int num) in
    match check_state status with
      [] -> false
    | ls -> printf "WINNER: \"%s\" by %s" name (String.concat ", " ls);
            true
;;

let drop board player col =
    let rec loop pos =
        if (M.get board pos col) = 0.0
        then begin
            M.set board pos col (float_of_int player);
            true
        end else begin
            if pos > 0
            then loop (pos-1)
            else begin
                prerr_endline "error: column is full - choose another one";
                false
            end
        end
    in
        loop 5
;;

let turn board num name =
    printf "%s's turn, column to drop? " name;
    try let col = read_int () in
        if col > 6 || col < 0
        then begin
            prerr_endline "error: number out of range [0-5]";
            false
        end else
            drop board num col
    with _ ->
        prerr_endline "error: invalid input - not an integer";
        false
;;

print_endline "";;

let players =
    List.map begin fun snum ->
        printf "Player %d name: " snum;
        read_line ();
    end [ 1; 2 ]
;;

let repeat_while fn lst =
    let rec loop ilst inum =
        match ilst with
          []          -> loop lst 1
        | elt :: ilst ->
            if (fn inum elt)
            then ()
            else loop ilst (inum+1)
    in
        loop lst 1
;;

let board = M.create 6 7 0.0;;

M.print board;;

print_endline "";;

repeat_while begin fun num name ->
    while not (turn board num name) do
        print_endline("")
    done;
    M.print board;
    print_endline("");
    check_winner board num name
end players
;;
