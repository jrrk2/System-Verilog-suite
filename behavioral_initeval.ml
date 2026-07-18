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
  | BBinOp { op; lhs; rhs; _ } ->
      let a = eval env lhs and b = eval env rhs in
      let bool z = if z then Z.one else Z.zero in
      (match op with
       | BAdd -> Z.add a b | BSub -> Z.sub a b | BMul -> Z.mul a b
       | BDiv -> if Z.equal b Z.zero then Z.zero else Z.div a b
       | BMod -> if Z.equal b Z.zero then Z.zero else Z.rem a b
       | BAnd -> Z.logand a b | BOr -> Z.logor a b | BXor -> Z.logxor a b
       | BShl -> Z.shift_left a (Z.to_int b)
       | BShr | BAshr -> Z.shift_right a (Z.to_int b)
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
           Z.of_int (Z.to_int (Z.logand (Z.shift_right v i) Z.one)))
  | BSelect _ -> raise (Unsupported "BSelect on non-var")
  | BSlice { signal; msb; lsb } ->
      let lo = min msb lsb and hi = max msb lsb in
      let v = eval env signal in
      mask_w (hi - lo + 1) (Z.shift_right v lo)
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
        if Sys.getenv_opt "SVS_INITEVAL_DEBUG" <> None then
          Hashtbl.iter (fun a () ->
            let arr = Hashtbl.find env.arrays a in
            Printf.eprintf "[initeval] %s.%s[0..3] = %s\n%!" m.name a
              (String.concat " " (List.map (Z.format "%x")
                 (Array.to_list (Array.sub arr 0 (min 4 (Array.length arr)))))))
            env.awrites;
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
  let try_lower body =
    (* store: vec -> per-bit symbolic exprs (None = untouched) *)
    let store : (string, bexpr option array) Hashtbl.t = Hashtbl.create 4 in
    let slot v =
      match Hashtbl.find_opt store v with
      | Some a -> a
      | None ->
          let w = try Hashtbl.find vec_w v with Not_found -> raise Bail in
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
      | BVar v when Hashtbl.mem store v -> raise Bail  (* whole-vec read *)
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
                 | BShl -> Z.shift_left a (Z.to_int b)
                 | BShr | BAshr -> Z.shift_right a (Z.to_int b)
                 | BEq -> bool (Z.equal a b) | BNe -> bool (not (Z.equal a b))
                 | BLt -> bool (Z.lt a b) | BLe -> bool (Z.leq a b)
                 | BGt -> bool (Z.gt a b) | BGe -> bool (Z.geq a b))
           | _ -> None)
      | BUnOp { op = BNot; operand; _ } ->
          Option.map Z.lognot (const_of operand)
      | BSlice { signal; msb; lsb } ->
          let lo = min msb lsb and hi = max msb lsb in
          Option.map (fun v ->
            Z.logand (Z.shift_right v lo)
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
           | None -> raise Bail)
      | BCallStmt { func; _ } when String.length func > 0 && func.[0] = '$' -> ()
      | BAssign _ -> raise Bail   (* mixed writes: leave the process alone *)
      | BIf { condition; then_stmts; else_stmts } ->
          (match const_of (subst condition) with
           | Some c ->
               if Z.equal c Z.zero then List.iter go else_stmts
               else List.iter go then_stmts
           | None -> raise Bail)
      | BBlock ss -> List.iter go ss
      | _ -> raise Bail in
    List.iter go body;
    if not !saw_memwrite || Hashtbl.length store = 0 then raise Bail;
    Hashtbl.fold (fun v a acc ->
      let bits = Array.to_list a in
      if List.exists (fun b -> b = None) bits then raise Bail;
      let msb_first = List.rev_map (function Some e -> e | None -> assert false) bits in
      BAssign { lhs = v; rhs = BConcat msb_first } :: acc) store []
  in
  let processes = List.map (fun p ->
    match p with
    | BCombinational r ->
        (match try_lower r.body with
         | stmts -> BCombinational { r with body = stmts }
         | exception _ -> p)
    | _ -> p) m.processes in
  { m with processes }

let eval_program (p : bprogram) : bprogram =
  if Sys.getenv_opt "SVS_INITEVAL" = Some "0" then p
  else
    { p with modules =
        List.map (fun m -> lower_const_comb_memwrites (eval_module m))
          p.modules }
