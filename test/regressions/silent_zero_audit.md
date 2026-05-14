# Silent-zero substitution audit (task #139)

Systematic scan of every tree-rewriting site that could silently
substitute `BConst 0` (or equivalent) on a failure path, and the
disposition for each.  Originated from the rope ORFS layout
incident: a single silent-zero in `Eval.tokenize`'s `'<not-0/1>`
fallback caused an array-typed localparam to be evaluated as
integer 0, which cascaded into 10 387 gates of "compute zero from
inputs and tie cells" at routed layout.

## Resolution legend

  **Converted** — now routes through `silent_zero` helper /
  `Silent_zero_substitution` exception.  Default = raise;
  `SV_DECOMP_LENIENT=1` opts back into the historical fallback.

  **Re-raise**  — exception-swallow site that was hiding our new
  Silent_zero from default-strict.  Now re-raises
  Silent_zero_substitution; other failures keep their old
  fallback.

  **Keep**      — fallback is structurally correct (Option contract,
  SV-standard semantics, recoverable file-system error, …).

## verible_to_behavioral.ml

| Line | Pattern | Disposition | Note |
|---:|---|---|---|
| 49  | `eval_int parse_dec _ -> None` | **Keep** | Option contract |
| 67  | `int_of_string … with _ -> None` | **Keep** | Option contract |
| 76  | `…0b…` parse | **Keep** | Option contract |
| 82  | `…0x…` parse | **Keep** | Option contract |
| 88  | `…dec…` parse | **Keep** | Option contract |
| 94  | `…0o…` parse | **Keep** | Option contract |
| 504 | `param_value_to_bexpr parse_dec _ -> None` | **Keep** | Caller falls back to BVar |
| 505 | `parse_dec helper` | **Keep** | Helper for above |
| 529 | `'1`/`'0` parse, fallback `BVar id` | **Keep** | Valid identifier |
| 658 | `parse_sized int_of_string _ -> 0` | **Converted** | Was: silent 0 on hex/dec parse failure |
| 680 | `TK_DecNumber int_of_string _ -> BConst 0` | **Converted** | Was: silent 0 on unbased-literal parse failure |
| 1186 (verible_elaborate) | `List.combine fn.fn_args args _ -> []` | **Keep** | Empty bindings → SVUnknown (correct) |
| 1840 | scope `int_of_string with _ -> None` | **Keep** | Option contract |
| 1874 | port-conn `expr_to_bexpr _ -> BVar fallback` | **Re-raise** | Was: swallowed Silent_zero |
| 1977 | `extract_body_params expr_to_bexpr _ -> None` | **Keep** | Option contract, caller handles |
| 2506 | net_decl_assign `expr_to_bexpr _ -> None` | **Re-raise** | Was: swallowed Silent_zero |
| 2585 | `$readmemh` file-read `with _ -> None` | **Keep** | File-not-found is recoverable |

## verible_elaborate.ml

| Line | Pattern | Disposition | Note |
|---:|---|---|---|
| 910 | `int_of_pvalue int_of_string _ -> None` | **Keep** | Option contract |
| 1186 | `List.combine fn.fn_args args _ -> []` | **Keep** | Empty bindings → SVUnknown |
| 1453 | `Eval.tokenize '<other> _ -> push (TNum 0)` | **Converted** | **Root cause of task #139.** `'{` was tokenising as TNum 0 |
| 1479 | `Eval.tokenize sized int_of_string _ -> 0` | **Converted** | Silent 0 on hex/dec parse failure |
| 1483 | `Eval.tokenize unsized int_of_string _ -> 0` | **Converted** | Silent 0 on decimal parse failure |
| 1491 | `Eval.tokenize _ -> incr i` | **Converted** | Was: silently skip unknown punctuation (`{`/`}`/`;`) |
| 1715 | `Eval.eval_string top-level _ -> None` | **Keep** | API contract: catches `bail`s, returns None |

## verilator_to_behavioral.ml

| Line | Pattern | Disposition | Note |
|---:|---|---|---|
| 60  | `name `s parse `_ -> 1` | **Keep** | Default port size |
| 121 | `parse_const_value _ -> 0` | **Converted** | Now raises `Verible_to_behavioral.Silent_zero_substitution`; LENIENT=1 restores 0 |
| 335 | `dynamic part-select no lsb -> BConst 0` | **Keep** | SV semantics: `[+:w]` starts at bit 0 |
| 442 | `dynamic part-write no lsb -> BConst 0` | **Keep** | Same |
| 965 | `instance overrides _ -> Some (name, None)` | **Keep** | None for non-int override is correct |

## vhdl_to_behavioral.ml — DEFERRED

The VHDL frontend has the largest concentration of silent-zero
substitutions, but it's also the least-exercised in the current
calibration set.  Listed for a future audit pass once the VHDL
regression suite expands:

| Line | Pattern |
|---:|---|
| 52  | `int_of_string _ -> None` (probably Option contract) |
| 141 | `with _ -> BConst { value = 0; width = 32 }` |
| 148 | `with _ -> BConst { value = 0; width = 1 }` |
| 297 | `_ -> BConst 0 width 1` (unmatched VHDL token) |
| 307 | `Double VhdPhysicalPrimary -> BConst 0 width 32` |
| 310 | `Double VhdFloatPrimary -> BConst 0 width 32` |
| 312 | `Double VhdNewFactor -> BConst 0 width 1` |
| 318 | `with _ -> BConst 0 width 32` |
| 319 | `Char '0' -> BConst 0 width 1` (correct: VHDL '0' literal) |
| 380 | empty pair-list `_ -> BConst 0` |
| 529, 535, 555 | various unmatched VHDL tokens |
| 559, 576 | `asctoken … with _ -> "?"` (stringify fallback) |

Most of these are "unmatched VHDL token shape produces BConst 0
width 1" — the same anti-pattern as the verible-side fallbacks
before this audit.  Each one should be converted to a
`silent_zero` helper call once VHDL regression coverage justifies
the effort.

## State after audit

8 sites converted (6 verible-side + 1 elaborator tokenizer + 1
verilator-side).  `SV_DECOMP_LENIENT=1` restores historical
behaviour for any consumer that depended on the silent fallback.
12 representative regression files parse cleanly under
default-strict.  The two task #139 regressions
(`svh_array_localparam{,_unroll}.sv`) emit BVar `LUT_Q31`
references instead of `32'0` — visible progress but still need
ROM-promotion of array-typed localparams to fully resolve.
