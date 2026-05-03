# Signed-vs-unsigned multiplier annotation in Vivado RTL elaboration

## Test cases

| Test | Source SV | Vivado cell | Miter verdict |
|---|---|---|---|
| `mul_unsigned` | `logic [3:0] a, b; logic [7:0] y; y = a*b;` | `RTL_MULT` (8-bit O, 4-bit I0/I1) | ✅ FORMALLY EQUIVALENT |
| `mul_signed` | `logic signed [3:0] a, b; logic signed [7:0] y; y = a*b;` | `RTL_MULT` (8-bit O, 4-bit I0/I1) | ❌ NOT EQUIVALENT |
| `mul_mixed` | one operand `signed`, other unsigned | `RTL_MULT` (8-bit O, 4-bit I0/I1) | ✅ FORMALLY EQUIVALENT |

## What the cell ports look like

For all three SV variants, Vivado's elaborated VHDL has *identical*
component declarations:

```vhdl
component RTL_MULT is
port (
  O  : out STD_LOGIC_VECTOR ( 7 downto 0 );
  I0 : in  STD_LOGIC_VECTOR ( 3 downto 0 );
  I1 : in  STD_LOGIC_VECTOR ( 3 downto 0 )
);
end component RTL_MULT;
```

There is no `RTL_MULT_SIGNED` family, no `IS_SIGNED` parameter, no
attribute on the cell instance that indicates signedness. The
operand and result types are `STD_LOGIC_VECTOR` (which by VHDL
convention is unsigned).

## What the miter shows

Encoding `RTL_MULT` as unsigned `BMul`:

* `mul_unsigned`: matches the unsigned source — UNSAT.
* `mul_signed`: a signed source produces a different result (signed
  multiplication of `a=1, b=4'b1111 = -1` gives `-1 = 8'b1111_1111`,
  while unsigned gives `15 = 8'b0000_1111`). The miter finds this
  counterexample.
* `mul_mixed`: per SystemVerilog LRM, mixing unsigned and signed
  promotes to *unsigned*. So the source semantics align with Vivado's
  unsigned encoding — UNSAT.

## What this means

The user's hypothesis is confirmed at the elaboration level: Vivado's
`RTL_MULT` cell does not preserve the signedness of the source
multiplication. The cell looks identical for signed and unsigned
operands.

The actual *synthesised* hardware Vivado eventually produces handles
signedness correctly (because synthesis sees the source signedness
declarations and uses the appropriate DSP slice / signed multiplier).
But the RTL-elaboration output is annotated as if it were always
unsigned.

For formal equivalence checking against the elaborated form, this
means a signed-multiplication design will *appear* incorrect through
this path, even though the eventual hardware is right. To get a
clean verdict, the converter would need to either:

1. Encode `RTL_MULT` with operand-signedness inferred from the source
   (which we don't have access to from the elaborated VHDL alone), or
2. Compare against the post-synthesis netlist, where the multiplier is
   tech-mapped and the signedness is implicit in the structure.

This test case is now part of the regression suite — `mul_signed`'s
deliberate failure documents the gap.
