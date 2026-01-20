// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See Vhardcaml_alu.h for the primary calling header

#ifndef VERILATED_VHARDCAML_ALU___024ROOT_H_
#define VERILATED_VHARDCAML_ALU___024ROOT_H_  // guard

#include "verilated.h"


class Vhardcaml_alu__Syms;

class alignas(VL_CACHE_LINE_BYTES) Vhardcaml_alu___024root final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(a,7,0);
    VL_IN8(b,7,0);
    VL_IN8(op,3,0);
    VL_OUT8(y,7,0);
    CData/*0:0*/ __VstlFirstIteration;
    CData/*0:0*/ __VicoFirstIteration;
    CData/*0:0*/ __VactContinue;
    IData/*31:0*/ __VactIterCount;
    VlTriggerVec<1> __VstlTriggered;
    VlTriggerVec<1> __VicoTriggered;
    VlTriggerVec<0> __VactTriggered;
    VlTriggerVec<0> __VnbaTriggered;

    // INTERNAL VARIABLES
    Vhardcaml_alu__Syms* const vlSymsp;

    // CONSTRUCTORS
    Vhardcaml_alu___024root(Vhardcaml_alu__Syms* symsp, const char* v__name);
    ~Vhardcaml_alu___024root();
    VL_UNCOPYABLE(Vhardcaml_alu___024root);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
