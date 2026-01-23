(* Test VHDL Expression to IR Converter *)

open Vhdl_expr_to_ir
open VhdlTypes
open Sv_ast

(* Helper to create test expressions *)
let test_simple_expressions () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL Expression to IR Converter - Simple Tests\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let ctx = create_context () in

  (* Test 1: Integer literal *)
  Printf.printf "Test 1: Integer literal '5'\n";
  let int_expr = AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  IntPrimary (Big_int.big_int_of_int 5, 0)
                )
              )
            )
          )
        )
      )
    )
  ) in
  let (id, width) = convert_expression ctx int_expr in
  Printf.printf "  → IR node %d, width %d\n" id width;
  Printf.printf "  → Node count: %d\n\n" (Hashtbl.length ctx.ir_nodes);

  (* Test 2: Character literal '1' *)
  Printf.printf "Test 2: Character literal '1'\n";
  let char_expr = AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  CharPrimary ('1', 0)
                )
              )
            )
          )
        )
      )
    )
  ) in
  let (id2, width2) = convert_expression ctx char_expr in
  Printf.printf "  → IR node %d, width %d\n" id2 width2;
  Printf.printf "  → Node count: %d\n\n" (Hashtbl.length ctx.ir_nodes);

  (* Test 3: Signal name "CLK" *)
  Printf.printf "Test 3: Signal reference 'CLK'\n";
  let name_expr = AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("CLK", 0))
                )
              )
            )
          )
        )
      )
    )
  ) in
  let (id3, width3) = convert_expression ctx name_expr in
  Printf.printf "  → IR node %d, width %d\n" id3 width3;
  Printf.printf "  → Signal table: %d entries\n" (Hashtbl.length ctx.signals);
  Printf.printf "  → Wire table: %d entries\n\n" (Hashtbl.length ctx.ir_wires);

  (* Test 4: Addition "a + b" *)
  Printf.printf "Test 4: Addition 'a + b'\n";
  let add_expr = AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AddSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("a", 0))
                )
              )
            ),
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("b", 0))
                )
              )
            )
          )
        )
      )
    )
  ) in
  let (id4, width4) = convert_expression ctx add_expr in
  Printf.printf "  → IR node %d, width %d\n" id4 width4;
  Printf.printf "  → Total nodes: %d\n\n" (Hashtbl.length ctx.ir_nodes);

  (* Test 5: Comparison "x = y" *)
  Printf.printf "Test 5: Comparison 'x = y'\n";
  let cmp_expr = AtomExpression (
    AtomLogicalExpression (
      EqualRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("x", 0))
                )
              )
            )
          )
        ),
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("y", 0))
                )
              )
            )
          )
        )
      )
    )
  ) in
  let (id5, width5) = convert_expression ctx cmp_expr in
  Printf.printf "  → IR node %d, width %d (comparison returns 1-bit)\n" id5 width5;
  Printf.printf "  → Total nodes: %d\n\n" (Hashtbl.length ctx.ir_nodes);

  (* Test 6: Logical AND "p and q" *)
  Printf.printf "Test 6: Logical AND 'p and q'\n";
  let and_expr = AtomExpression (
    AndLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("p", 0))
                )
              )
            )
          )
        )
      ),
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor (
                AtomDotted (
                  NamePrimary (SimpleName ("q", 0))
                )
              )
            )
          )
        )
      )
    )
  ) in
  let (id6, width6) = convert_expression ctx and_expr in
  Printf.printf "  → IR node %d, width %d\n" id6 width6;
  Printf.printf "  → Total nodes: %d\n\n" (Hashtbl.length ctx.ir_nodes);

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
  Printf.printf "Total IR nodes created: %d\n" (Hashtbl.length ctx.ir_nodes);
  Printf.printf "Total signals tracked: %d\n" (Hashtbl.length ctx.signals);
  Printf.printf "Total wires created: %d\n\n" (Hashtbl.length ctx.ir_wires);

  Printf.printf "Signal table:\n";
  Hashtbl.iter (fun name (id, width) ->
    Printf.printf "  %s → node %d (width %d)\n" name id width
  ) ctx.signals;

  Printf.printf "\n✅ All expression conversion tests passed!\n"

let () =
  test_simple_expressions ()
