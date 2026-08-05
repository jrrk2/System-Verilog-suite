(* Compile-time evaluation of procedural `initial` blocks.
 *
 * Synthesizable Verilog may precompute lookup tables in `initial` blocks
 * (rgmii_lfsr builds its CRC-32 mask matrices with triple-nested loops);
 * real synthesis tools interpret the block at elaboration time and fold
 * the resulting values as constants.  verible_to_behavioral parses such
 * blocks into BCombinational processes named "__initial__"; this pass
 * (run inside svd.unroll, i.e. after generate expansion so array reads
 * carry literal indices) interprets them and substitutes constant array
 * reads throughout the module, then drops the __initial__ processes.
 *
 * Substitution is deliberately restricted to ARRAY element reads with
 * literal indices (BSelect{BVar a, BConst k}) where the array is written
 * ONLY by initial processes — scalar temporaries (loop vars, state_val)
 * stay untouched.  A dynamic read of an initial-written array is
 * reported loudly: it would need a ROM, not a substitution. *)

open Behavioral_ir

exception Unsupported of string

let fuel = ref 0
let burn () =
  incr fuel;
  if !fuel > 20_000_000 then raise (Unsupported "initial-block step budget exceeded")

let mask_w w v = Z.logand v (Z.sub (Z.shift_left Z.one w) Z.one)

(* OUT-OF-RANGE SHIFTS AND BIT SELECTS.
 *
 * Z.shift_right/shift_left reject a negative count outright, so a single
 * out-of-range index anywhere in an initial block aborts the whole evaluation
 * -- rgmii_lfsr died with `Z.shift_right: count argument must be positive` and
 * took its CRC mask tables with it.  Verilog does not fault here: a shift's
 * count is unsigned, so a "negative" one is astronomically large and the
 * result is 0, and an out-of-range bit select reads x (0 in a 2-state
 * evaluator).  Both agree on 0, which is also what the array-index path a few
 * lines below already does.  Cap the magnitude too, so a wild count cannot
 * turn into a multi-gigabyte Z value. *)
let shift_cap = 1 lsl 20
let safe_shr v n = if n < 0 || n > shift_cap then Z.zero else Z.shift_right v n
let safe_shl v n = if n < 0 || n > shift_cap then Z.zero else Z.shift_left v n
let to_shift z = try Z.to_int z with _ -> shift_cap + 1

type env = {
  widths  : (string, int) Hashtbl.t;                (* scalar widths *)
  arrays  : (string, Z.t array) Hashtbl.t;          (* array name -> elems *)
  elemw   : (string, int) Hashtbl.t;                (* array elem width *)
  scalars : (string, Z.t) Hashtbl.t;
  awrites : (string, unit) Hashtbl.t;               (* arrays written *)
}

let width_of env e =
  let rec go = function
    | BConst { width; _ } -> width
    | BVar nm -> (try Hashtbl.find env.widths nm with Not_found -> 1)
    | BSlice { msb; lsb; _ } -> abs (msb - lsb) + 1
    | BConcat es -> List.fold_left (fun a e -> a + go e) 0 es
    | BReplicate { count; value } -> count * go value
    | BBinOp { result_type = BInt { width; _ }; _ } -> width
    | BBinOp { lhs; rhs; _ } -> max (go lhs) (go rhs)
    | BUnOp { result_type = BInt { width; _ }; _ } -> width
    | BUnOp { op = (BRedAnd | BRedOr | BRedXor); _ } -> 1
    | BUnOp { operand; _ } -> go operand
    | BCond { then_val; _ } -> go then_val
    | BSelect { array = BVar a; _ } ->
        (try Hashtbl.find env.elemw a with Not_found -> 1)
    | _ -> 32
  in go e

let rec eval env (e : bexpr) : Z.t =
  burn ();
  match e with
  | BConst { value; _ } -> value
  | BVar nm ->
      (match Hashtbl.find_opt env.scalars nm with
       | Some v -> v
       | None -> Z.zero)
  | BBinOp { op; lhs; rhs; result_type } ->
      let a = eval env lhs and b = eval env rhs in
      let bool z = if z then Z.one else Z.zero in
      (* VERILOG ARITHMETIC IS MODULO THE RESULT WIDTH, and the result width is
         the expression's own, not 63-bit OCaml or unbounded Z.  Verilator
         constant-folds `m[i][32-j-1]` into FIVE-BIT arithmetic -- the tree is
         literally SUB(NEGATE(j[4:0]), 5'h1) -- so at j=0 the index is
         (-0)-1 = -1, which is 31 mod 32 and exactly the bit meant.  Evaluating
         that over Z gave a genuine -1: it used to raise
         `Z.shift_right: count argument must be positive` and, once that was
         guarded, silently read bit "-1" as 0.  Every reversed row of
         rgmii_lfsr's CRC mask matrix then lost bit 0 (17 of 32 rows wrong) --
         a WRONG CRC, which is worse than no CRC because it looks alive.
         BUnOp BNeg already masks; do the same for the operators that can leave
         the range, using the dtype-derived result width rather than width_of's
         inference.  Bitwise ops of in-range values stay in range, and the
         comparisons yield 0/1, so neither needs it. *)
      let wrap v = match result_type with
        | BInt { width = w; _ } when w > 0 && w <= 65536 -> mask_w w v
        | _ -> v in
      (match op with
       | BAdd -> wrap (Z.add a b) | BSub -> wrap (Z.sub a b)
       | BMul -> wrap (Z.mul a b)
       | BDiv -> if Z.equal b Z.zero then Z.zero else Z.div a b
       | BMod -> if Z.equal b Z.zero then Z.zero else Z.rem a b
       | BAnd -> Z.logand a b | BOr -> Z.logor a b | BXor -> Z.logxor a b
       | BShl -> wrap (safe_shl a (to_shift b))
       | BShr | BAshr -> safe_shr a (to_shift b)
       | BEq -> bool (Z.equal a b) | BNe -> bool (not (Z.equal a b))
       | BLt -> bool (Z.lt a b) | BLe -> bool (Z.leq a b)
       | BGt -> bool (Z.gt a b) | BGe -> bool (Z.geq a b))
  | BUnOp { op; operand; _ } ->
      let w = width_of env operand in
      let v = mask_w w (eval env operand) in
      (match op with
       | BNot -> mask_w w (Z.lognot v)
       | BNeg -> mask_w w (Z.neg v)
       | BRedAnd -> if Z.equal v (mask_w w (Z.minus_one)) then Z.one else Z.zero
       | BRedOr -> if Z.equal v Z.zero then Z.zero else Z.one
       | BRedXor -> if Z.popcount v land 1 = 1 then Z.one else Z.zero)
  | BSelect { array = BVar a; index } ->
      let i = Z.to_int (eval env index) in
      (match Hashtbl.find_opt env.arrays a with
       | Some arr -> if i >= 0 && i < Array.length arr then arr.(i) else Z.zero
       | None ->
           (* bit-select of a scalar *)
           let v = eval env (BVar a) in
           Z.of_int (Z.to_int (Z.logand (safe_shr v i) Z.one)))
  | BSelect _ -> raise (Unsupported "BSelect on non-var")
  | BSlice { signal; msb; lsb } ->
      let lo = min msb lsb and hi = max msb lsb in
      let v = eval env signal in
      mask_w (hi - lo + 1) (safe_shr v lo)
  | BConcat es ->
      (* MSB-first *)
      List.fold_left (fun acc e ->
        let w = width_of env e in
        Z.logor (Z.shift_left acc w) (mask_w w (eval env e))) Z.zero es
  | BReplicate { count; value } ->
      let w = width_of env value in
      let v = mask_w w (eval env value) in
      let rec rep n acc = if n = 0 then acc
        else rep (n - 1) (Z.logor (Z.shift_left acc w) v) in
      rep count Z.zero
  | BCond { condition; then_val; else_val } ->
      if not (Z.equal (eval env condition) Z.zero)
      then eval env then_val else eval env else_val
  | BCall { func; _ } -> raise (Unsupported ("BCall " ^ func))

let set_scalar env nm v =
  let w = try Hashtbl.find env.widths nm with Not_found -> 64 in
  Hashtbl.replace env.scalars nm (mask_w w v)

let set_array env a i v =
  (match Hashtbl.find_opt env.arrays a with
   | Some arr ->
       if i >= 0 && i < Array.length arr then begin
         let w = try Hashtbl.find env.elemw a with Not_found -> 64 in
         arr.(i) <- mask_w w v;
         Hashtbl.replace env.awrites a ()
       end
   | None -> raise (Unsupported ("array write to unknown array " ^ a)))

let insert_bits env nm ~hi ~lo v =
  let cur = try Hashtbl.find env.scalars nm with Not_found -> Z.zero in
  let w = hi - lo + 1 in
  let m = Z.shift_left (Z.sub (Z.shift_left Z.one w) Z.one) lo in
  let nv = Z.logor (Z.logand cur (Z.lognot m))
                   (Z.logand (Z.shift_left (mask_w w v) lo) m) in
  set_scalar env nm nv

let bracket_re = Str.regexp {|^\(.*\)\[\([0-9]+\)\]$|}

let rec exec env (s : bstmt) : unit =
  burn ();
  match s with
  | BAssign { lhs; rhs } ->
      let v = eval env rhs in
      if Str.string_match bracket_re lhs 0 then begin
        let base = Str.matched_group 1 lhs in
        let bit = int_of_string (Str.matched_group 2 lhs) in
        if Hashtbl.mem env.arrays base then set_array env base bit v
        else insert_bits env base ~hi:bit ~lo:bit v
      end else
        set_scalar env lhs v
  | BIf { condition; then_stmts; else_stmts } ->
      if not (Z.equal (eval env condition) Z.zero)
      then List.iter (exec env) then_stmts
      else List.iter (exec env) else_stmts
  | BCase { selector; cases; default } ->
      let sv = eval env selector in
      let rec pick = function
        | [] -> List.iter (exec env) default
        | (c, body) :: rest ->
            if Z.equal (eval env c) sv then List.iter (exec env) body
            else pick rest
      in pick cases
  | BWhile { condition; body } ->
      while not (Z.equal (eval env condition) Z.zero) do
        burn (); List.iter (exec env) body
      done
  | BFor { init; condition; update; body } ->
      exec env init;
      while not (Z.equal (eval env condition) Z.zero) do
        burn ();
        List.iter (exec env) body;
        exec env update
      done
  | BBlock ss -> List.iter (exec env) ss
  | BCallStmt { func = "@mem_write"; args = [BVar a; idx; v] } ->
      let i = Z.to_int (eval env idx) in
      if Hashtbl.mem env.arrays a then set_array env a i (eval env v)
      else
        (* single-bit write on a VECTOR reg (state_val[j] = x) parses as
           @mem_write too — insert the bit *)
        insert_bits env a ~hi:i ~lo:i (eval env v)
  | BCallStmt { func = "@slice_write"; args = [BVar nm; m; l; v] } ->
      let mi = Z.to_int (eval env m) and li = Z.to_int (eval env l) in
      let hi = max mi li and lo = min mi li in
      if Hashtbl.mem env.arrays nm && hi = lo then
        set_array env nm hi (eval env v)
      else insert_bits env nm ~hi ~lo (eval env v)
  | BCallStmt { func = "@part_sel_write_up"; args = [BVar nm; base; w; v] } ->
      let b = Z.to_int (eval env base) and wi = Z.to_int (eval env w) in
      insert_bits env nm ~hi:(b + wi - 1) ~lo:b (eval env v)
  | BCallStmt { func = "@part_sel_write_down"; args = [BVar nm; base; w; v] } ->
      let b = Z.to_int (eval env base) and wi = Z.to_int (eval env w) in
      insert_bits env nm ~hi:b ~lo:(b - wi + 1) (eval env v)
  | BCallStmt { func; _ } ->
      (* $display / $error / $finish inside untaken branches, etc. *)
      if String.length func > 0 && func.[0] = '$' then ()
      else raise (Unsupported ("call " ^ func))
  | _ -> raise (Unsupported "statement form")

(* ---- per-module driver ---- *)

let is_initial_proc = function
  | BCombinational { name; _ } ->
      String.length name >= 11 && String.sub name 0 11 = "__initial__"
  | _ -> false

let proc_body = function
  | BCombinational { body; _ } -> body
  | _ -> []

let eval_module (m : bmodule) : bmodule =
  let inits = List.filter is_initial_proc m.processes in
  if inits = [] then m
  else begin
    fuel := 0;
    let env = {
      widths = Hashtbl.create 64; arrays = Hashtbl.create 16;
      elemw = Hashtbl.create 16; scalars = Hashtbl.create 64;
      awrites = Hashtbl.create 16;
    } in
    List.iter (fun (s : bsignal) ->
      match s.stype with
      | BArray { element = BInt { width; _ }; size } ->
          Hashtbl.replace env.arrays s.name (Array.make size Z.zero);
          Hashtbl.replace env.elemw s.name width
      | BArray { element = BBool; size } ->
          Hashtbl.replace env.arrays s.name (Array.make size Z.zero);
          Hashtbl.replace env.elemw s.name 1
      | BInt { width; _ } -> Hashtbl.replace env.widths s.name width
      | BBool -> Hashtbl.replace env.widths s.name 1
      | _ -> ()) m.signals;
    match
      List.iter (fun p -> List.iter (exec env) (proc_body p)) inits
    with
    | () ->
        (* SVS_INITEVAL_DEBUG=<n> dumps the first n elements (default 4) --
           four is enough to see a table is populated, nowhere near enough to
           diff two front ends against each other. *)
        (match Sys.getenv_opt "SVS_INITEVAL_DEBUG" with
         | None -> ()
         | Some s ->
           let n = match int_of_string_opt s with Some n when n > 0 -> n | _ -> 4 in
           Hashtbl.iter (fun a () ->
             let arr = Hashtbl.find env.arrays a in
             Printf.eprintf "[initeval] %s.%s[0..%d] = %s\n%!" m.name a
               (min n (Array.length arr) - 1)
               (String.concat " " (List.map (Z.format "%x")
                  (Array.to_list (Array.sub arr 0 (min n (Array.length arr)))))))
             env.awrites);
        (* substitute constant array reads; count dynamic leftovers *)
        let dynamic = ref [] in
        let rec sub e =
          match e with
          | BSelect { array = BVar a; index = BConst { value; _ } }
            when Hashtbl.mem env.awrites a ->
              let i = Z.to_int value in
              let arr = Hashtbl.find env.arrays a in
              let w = try Hashtbl.find env.elemw a with Not_found -> 1 in
              if i >= 0 && i < Array.length arr then
                BConst { value = arr.(i); width = w }
              else BConst { value = Z.zero; width = w }
          | BSelect { array = BVar a; index }
            when Hashtbl.mem env.awrites a ->
              dynamic := a :: !dynamic;
              BSelect { array = BVar a; index = sub index }
          | BSelect { array; index } ->
              BSelect { array = sub array; index = sub index }
          | BBinOp r -> BBinOp { r with lhs = sub r.lhs; rhs = sub r.rhs }
          | BUnOp r -> BUnOp { r with operand = sub r.operand }
          | BSlice r -> BSlice { r with signal = sub r.signal }
          | BConcat es -> BConcat (List.map sub es)
          | BReplicate r -> BReplicate { r with value = sub r.value }
          | BCond r -> BCond { condition = sub r.condition;
                               then_val = sub r.then_val;
                               else_val = sub r.else_val }
          | BCall r -> BCall { r with args = List.map sub r.args }
          | BVar _ | BConst _ -> e
        in
        let rec sub_stmt st =
          match st with
          | BAssign r -> BAssign { r with rhs = sub r.rhs }
          | BIf r -> BIf { condition = sub r.condition;
                           then_stmts = List.map sub_stmt r.then_stmts;
                           else_stmts = List.map sub_stmt r.else_stmts }
          | BCase r -> BCase { selector = sub r.selector;
                               cases = List.map (fun (c, b) ->
                                 (sub c, List.map sub_stmt b)) r.cases;
                               default = List.map sub_stmt r.default }
          | BWhile r -> BWhile { condition = sub r.condition;
                                 body = List.map sub_stmt r.body }
          | BFor r -> BFor { init = sub_stmt r.init;
                             condition = sub r.condition;
                             update = sub_stmt r.update;
                             body = List.map sub_stmt r.body }
          | BBlock ss -> BBlock (List.map sub_stmt ss)
          | BCallStmt r -> BCallStmt { r with args = List.map sub r.args }
          | other -> other
        in
        let sub_proc = function
          | BCombinational r ->
              BCombinational { r with body = List.map sub_stmt r.body }
          | BSequential r ->
              BSequential { r with body = List.map sub_stmt r.body }
          | other -> other
        in
        let processes =
          List.filter (fun p -> not (is_initial_proc p)) m.processes
          |> List.map sub_proc in
        let instances =
          List.map (fun (i : binstance) ->
            { i with port_connections =
                List.map (fun (p, e) -> (p, sub e)) i.port_connections })
            m.instances in
        (match List.sort_uniq compare !dynamic with
         | [] -> ()
         | ds ->
             Printf.eprintf
               "[initeval] WARNING module %s: dynamic reads of \
                initial-written array(s) [%s] left unsubstituted (would \
                need ROM lowering)\n%!" m.name (String.concat "," ds));
        { m with processes; instances }
    | exception Unsupported what ->
        Printf.eprintf
          "[initeval] WARNING module %s: initial block NOT evaluated (%s) — \
           dropping it; precomputed tables stay zero\n%!" m.name what;
        { m with processes =
            List.filter (fun p -> not (is_initial_proc p)) m.processes }
  end

(* ---- constant-control partial evaluation of comb vector @mem_write ----
 *
 * The LOOP style of rgmii_lfsr unrolls (post-generate) to a comb process of
 * the shape
 *     @mem_write(state_out_reg, 3, 0)
 *     if (CONST) @mem_write(state_out_reg, 3, state_out_reg[3] ^ state_in[7])
 *     if (CONST) @mem_write(state_out_reg, 3, state_out_reg[3] ^ data_in[2])
 * Blocking read-modify-write accumulation through @mem_write is not threaded
 * by any downstream pass — only the LAST write per bit survived, reducing
 * the CRC to a bare shift.  When every branch condition in such a process is
 * CONSTANT and every @mem_write index is CONSTANT on a VECTOR signal, we can
 * symbolically execute the body (per-bit symbolic XOR expressions) and
 * replace the process with a single whole-vector BAssign. *)

let lower_const_comb_memwrites (m : bmodule) : bmodule =
  let vec_w : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let is_arr : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    match s.stype with
    | BInt { width; _ } -> Hashtbl.replace vec_w s.name width
    | BBool -> Hashtbl.replace vec_w s.name 1
    | BArray _ -> Hashtbl.replace is_arr s.name ()
    | _ -> ()) m.signals;
  let exception Bail in
  let bail_why = ref "" in
  let bail w = bail_why := w; raise Bail in
  let try_lower body =
    (* store: vec -> per-bit symbolic exprs (None = untouched) *)
    let store : (string, bexpr option array) Hashtbl.t = Hashtbl.create 4 in
    let slot v =
      match Hashtbl.find_opt store v with
      | Some a -> a
      | None ->
          let w = try Hashtbl.find vec_w v with Not_found -> bail ("slot " ^ v) in
          let a = Array.make w None in
          Hashtbl.add store v a; a in
    let rec subst e =
      match e with
      | BSelect { array = BVar v; index = BConst { value; _ } }
        when Hashtbl.mem vec_w v && Hashtbl.mem store v ->
          let a = slot v in
          let i = Z.to_int value in
          if i >= 0 && i < Array.length a then
            (match a.(i) with
             | Some se -> se
             | None -> BSlice { signal = BVar v; msb = i; lsb = i })
          else BConst { value = Z.zero; width = 1 }
      | BVar v when Hashtbl.mem store v -> bail ("whole-vec read " ^ v)
      | BBinOp r -> BBinOp { r with lhs = subst r.lhs; rhs = subst r.rhs }
      | BUnOp r -> BUnOp { r with operand = subst r.operand }
      | BSelect { array; index } -> BSelect { array = subst array; index = subst index }
      | BSlice r -> BSlice { r with signal = subst r.signal }
      | BConcat es -> BConcat (List.map subst es)
      | BReplicate r -> BReplicate { r with value = subst r.value }
      | BCond r -> BCond { condition = subst r.condition;
                           then_val = subst r.then_val;
                           else_val = subst r.else_val }
      | _ -> e in
    let rec const_of e =
      match e with
      | BConst { value; _ } -> Some value
      | BBinOp { op; lhs; rhs; _ } ->
          (match const_of lhs, const_of rhs with
           | Some a, Some b ->
               let bool z = if z then Z.one else Z.zero in
               Some (match op with
                 | BAnd -> Z.logand a b | BOr -> Z.logor a b
                 | BXor -> Z.logxor a b
                 | BAdd -> Z.add a b | BSub -> Z.sub a b | BMul -> Z.mul a b
                 | BDiv -> if Z.equal b Z.zero then Z.zero else Z.div a b
                 | BMod -> if Z.equal b Z.zero then Z.zero else Z.rem a b
                 | BShl -> safe_shl a (to_shift b)
                 | BShr | BAshr -> safe_shr a (to_shift b)
                 | BEq -> bool (Z.equal a b) | BNe -> bool (not (Z.equal a b))
                 | BLt -> bool (Z.lt a b) | BLe -> bool (Z.leq a b)
                 | BGt -> bool (Z.gt a b) | BGe -> bool (Z.geq a b))
           | _ -> None)
      | BUnOp { op = BNot; operand; _ } ->
          Option.map Z.lognot (const_of operand)
      | BSlice { signal; msb; lsb } ->
          let lo = min msb lsb and hi = max msb lsb in
          Option.map (fun v ->
            Z.logand (safe_shr v lo)
              (Z.sub (Z.shift_left Z.one (hi - lo + 1)) Z.one))
            (const_of signal)
      | _ -> None in
    let saw_memwrite = ref false in
    let rec go = function
      | BCallStmt { func = "@mem_write";
                    args = [BVar v; idx; rhs] } when Hashtbl.mem vec_w v ->
          (match const_of idx with
           | Some zi ->
               saw_memwrite := true;
               let a = slot v in
               let i = Z.to_int zi in
               if i >= 0 && i < Array.length a then
                 a.(i) <- Some (subst rhs)
           | None -> bail "mem_write dynamic index")
      (* Same per-bit accumulation, spelled differently by the verilator front
         end.  A `state_out_reg[i] = ...` LHS is a Sel on a VECTOR, which
         verilator_to_behavioral lowers to @slice_write (constant index) or
         @part_sel_write_up (dynamic index, constant after unrolling), never to
         @mem_write -- verible's spelling.  Accepting only @mem_write bailed on
         the whole process, the blocking read-modify-write chain was left
         unthreaded, and only the LAST write to each bit survived: state_out
         came out a CONSTANT 3 whatever the inputs, exactly the failure this
         pass was written to prevent. *)
      | BCallStmt { func = ("@slice_write" | "@part_sel_write_up") as func;
                    args = [BVar v; p1; p2; rhs] } when Hashtbl.mem vec_w v ->
          (match const_of p1, const_of p2 with
           | Some z1, Some z2 ->
               let i1 = Z.to_int z1 and i2 = Z.to_int z2 in
               let lo, hi =
                 if func = "@slice_write" then (min i1 i2, max i1 i2)
                 else (i1, i1 + i2 - 1)   (* base, width *) in
               saw_memwrite := true;
               (* substitute BEFORE updating any slot: the rhs reads the
                  pre-assignment values of this same vector *)
               let r = subst rhs in
               let a = slot v in
               for b = lo to hi do
                 if b >= 0 && b < Array.length a then
                   a.(b) <- Some (if hi = lo then r
                                  else BSlice { signal = r;
                                                msb = b - lo; lsb = b - lo })
               done
           | _ -> bail (func ^ " non-const bounds"))
      | BCallStmt { func; _ } when String.length func > 0 && func.[0] = '$' -> ()
      | BAssign { lhs; _ } -> bail ("BAssign " ^ lhs)
      | BIf { condition; then_stmts; else_stmts } ->
          (match const_of (subst condition) with
           | Some c ->
               if Z.equal c Z.zero then List.iter go else_stmts
               else List.iter go then_stmts
           | None -> bail "if with non-const condition")
      | BBlock ss -> List.iter go ss
      | other -> bail ("stmt " ^ (match other with
          | BCallStmt { func; _ } -> "call " ^ func
          | BWhile _ -> "while" | BFor _ -> "for" | BCase _ -> "case"
          | _ -> "?")) in
    List.iter go body;
    if not !saw_memwrite || Hashtbl.length store = 0 then bail "no per-bit writes";
    (* A process need not drive the WHOLE vector.  Verilator splits `always @*`
       blocks per driven bit (V3Split), so rgmii_lfsr's output loop arrives as
       32 processes each accumulating ONE bit of state_out -- demanding all 32
       bailed on every one of them, and the read-modify-write chain went
       unthreaded exactly as if the pass did not exist.  Whole-vector coverage
       still collapses to a single BAssign (the verible shape); a partial one
       emits a constant-bounds @slice_write per driven bit, which keeps the
       untouched bits untouched instead of feeding them back into themselves. *)
    Hashtbl.fold (fun v a acc ->
      let bits = Array.to_list a in
      if List.for_all (fun b -> b <> None) bits then
        let msb_first = List.rev_map (function Some e -> e | None -> assert false) bits in
        BAssign { lhs = v; rhs = BConcat msb_first } :: acc
      else begin
        let stmts = ref acc in
        Array.iteri (fun i b -> match b with
          | None -> ()
          | Some e ->
            let ix = BConst { value = Z.of_int i; width = 32 } in
            stmts := BCallStmt { func = "@slice_write";
                                 args = [BVar v; ix; ix; e] } :: !stmts) a;
        !stmts
      end) store []
  in
  (* Which vectors does a body write? *)
  let rec writes acc s = match s with
    | BCallStmt { func = ("@mem_write" | "@slice_write" | "@part_sel_write_up");
                  args = BVar v :: _ } -> if List.mem v acc then acc else v :: acc
    | BAssign { lhs; _ } -> if List.mem lhs acc then acc else lhs :: acc
    | BIf r -> List.fold_left writes (List.fold_left writes acc r.then_stmts) r.else_stmts
    | BBlock ss -> List.fold_left writes acc ss
    | _ -> acc in
  let sole_target = function
    | BCombinational r ->
        (match List.fold_left writes [] r.body with
         | [v] when Hashtbl.mem vec_w v -> Some v
         | _ -> None)
    | _ -> None in
  (* MERGE VERILATOR'S PER-BIT SPLIT PROCESSES.
     Verilator's V3Split breaks `always @*` into one process per driven bit, so
     rgmii_lfsr's output loop arrives as 32 processes each accumulating a
     single bit of state_out.  Lowered separately they each cover 1/32 of the
     vector, and a partial result has to be written back with @slice_write --
     which no downstream pass threads through a COMBINATIONAL process, so the
     accumulation was lost all over again and state_out stayed a constant.
     Merging the group first restores the one-process/whole-vector shape the
     verible front end produces, and the existing whole-vector BAssign path
     then applies unchanged.  Only groups writing exactly ONE shared vector are
     merged, and only when the merged body lowers cleanly; otherwise every
     member is left exactly as it was. *)
  let merged : (string, bstmt list) Hashtbl.t = Hashtbl.create 4 in
  let groups : (string, int) Hashtbl.t = Hashtbl.create 4 in
  List.iter (fun p -> match sole_target p with
    | Some v -> Hashtbl.replace groups v (1 + (try Hashtbl.find groups v with Not_found -> 0))
    | None -> ()) m.processes;
  Hashtbl.iter (fun v n ->
    if n > 1 then begin
      let body = List.concat_map (fun p ->
        if sole_target p = Some v then (match p with BCombinational r -> r.body | _ -> [])
        else []) m.processes in
      match try_lower body with
      | stmts -> Hashtbl.replace merged v stmts
      | exception _ ->
        if Sys.getenv_opt "SVS_INITEVAL_DEBUG" <> None then
          Printf.eprintf "[initeval] %s: merged group for %s NOT lowered (%s)\n%!"
            m.name v !bail_why
    end) groups;
  let emitted : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  let processes = List.filter_map (fun p ->
    match sole_target p with
    | Some v when Hashtbl.mem merged v ->
        if Hashtbl.mem emitted v then None      (* folded into the first one *)
        else begin
          Hashtbl.replace emitted v ();
          match p with
          | BCombinational r ->
              Some (BCombinational { r with body = Hashtbl.find merged v })
          | other -> Some other
        end
    | _ ->
      Some (match p with
        | BCombinational r ->
            (match try_lower r.body with
             | stmts -> BCombinational { r with body = stmts }
             | exception _ ->
               if Sys.getenv_opt "SVS_INITEVAL_DEBUG" <> None then
                 Printf.eprintf "[initeval] %s: comb process %s NOT lowered (%s)\n%!"
                   m.name r.name !bail_why;
               p)
        | _ -> p)) m.processes in
  { m with processes }

let eval_program (p : bprogram) : bprogram =
  if Sys.getenv_opt "SVS_INITEVAL" = Some "0" then p
  else
    { p with modules =
        List.map (fun m -> lower_const_comb_memwrites (eval_module m))
          p.modules }
