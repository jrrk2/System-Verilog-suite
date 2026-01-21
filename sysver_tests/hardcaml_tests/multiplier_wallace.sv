// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: wallace-operator
:description: Applying wallace to an operator
:tags: 5.12
*/

module multiplier_wallace(
	   output logic [5:0] y,
	   input logic [2:0] a,b);

always_comb
begin
y = a * (* mode = "Wallace" *) b;
end

endmodule
