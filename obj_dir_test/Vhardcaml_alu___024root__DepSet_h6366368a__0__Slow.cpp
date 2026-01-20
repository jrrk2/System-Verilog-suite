// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vhardcaml_alu.h for the primary calling header

#include "Vhardcaml_alu__pch.h"
#include "Vhardcaml_alu__Syms.h"
#include "Vhardcaml_alu___024root.h"

#ifdef VL_DEBUG
VL_ATTR_COLD void Vhardcaml_alu___024root___dump_triggers__stl(Vhardcaml_alu___024root* vlSelf);
#endif  // VL_DEBUG

VL_ATTR_COLD void Vhardcaml_alu___024root___eval_triggers__stl(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_triggers__stl\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__VstlTriggered.setBit(0U, (IData)(vlSelfRef.__VstlFirstIteration));
#ifdef VL_DEBUG
    if (VL_UNLIKELY(vlSymsp->_vm_contextp__->debug())) {
        Vhardcaml_alu___024root___dump_triggers__stl(vlSelf);
    }
#endif
}
