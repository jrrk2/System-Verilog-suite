// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vhardcaml_alu.h for the primary calling header

#include "Vhardcaml_alu__pch.h"
#include "Vhardcaml_alu___024root.h"

void Vhardcaml_alu___024root___ico_sequent__TOP__0(Vhardcaml_alu___024root* vlSelf);

void Vhardcaml_alu___024root___eval_ico(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_ico\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1ULL & vlSelfRef.__VicoTriggered.word(0U))) {
        Vhardcaml_alu___024root___ico_sequent__TOP__0(vlSelf);
    }
}

VL_INLINE_OPT void Vhardcaml_alu___024root___ico_sequent__TOP__0(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___ico_sequent__TOP__0\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*7:0*/ alu__DOT___135;
    alu__DOT___135 = 0;
    CData/*7:0*/ alu__DOT___139;
    alu__DOT___139 = 0;
    CData/*7:0*/ alu__DOT___153;
    alu__DOT___153 = 0;
    CData/*7:0*/ alu__DOT___104;
    alu__DOT___104 = 0;
    CData/*7:0*/ alu__DOT___108;
    alu__DOT___108 = 0;
    CData/*7:0*/ alu__DOT___122;
    alu__DOT___122 = 0;
    // Body
    if ((1U & (IData)(vlSelfRef.b))) {
        alu__DOT___135 = (0xffU & VL_SHIFTL_III(8,8,32, (IData)(vlSelfRef.a), 1U));
        alu__DOT___104 = (0xffU & VL_SHIFTR_III(8,8,32, (IData)(vlSelfRef.a), 1U));
    } else {
        alu__DOT___135 = (0xffU & (IData)(vlSelfRef.a));
        alu__DOT___104 = (0xffU & (IData)(vlSelfRef.a));
    }
    if ((2U & (IData)(vlSelfRef.b))) {
        alu__DOT___139 = (0xffU & VL_SHIFTL_III(8,8,32, (IData)(alu__DOT___135), 2U));
        alu__DOT___108 = (0xffU & VL_SHIFTR_III(8,8,32, (IData)(alu__DOT___104), 2U));
    } else {
        alu__DOT___139 = (0xffU & (IData)(alu__DOT___135));
        alu__DOT___108 = (0xffU & (IData)(alu__DOT___104));
    }
    if ((0x80U & (IData)(vlSelfRef.b))) {
        alu__DOT___153 = 0U;
        alu__DOT___122 = 0U;
    } else if ((0x40U & (IData)(vlSelfRef.b))) {
        alu__DOT___153 = 0U;
        alu__DOT___122 = 0U;
    } else if ((0x20U & (IData)(vlSelfRef.b))) {
        alu__DOT___153 = 0U;
        alu__DOT___122 = 0U;
    } else if ((0x10U & (IData)(vlSelfRef.b))) {
        alu__DOT___153 = 0U;
        alu__DOT___122 = 0U;
    } else if ((8U & (IData)(vlSelfRef.b))) {
        alu__DOT___153 = 0U;
        alu__DOT___122 = 0U;
    } else if ((4U & (IData)(vlSelfRef.b))) {
        alu__DOT___153 = (0xffU & VL_SHIFTL_III(8,8,32, (IData)(alu__DOT___139), 4U));
        alu__DOT___122 = (0xffU & VL_SHIFTR_III(8,8,32, (IData)(alu__DOT___108), 4U));
    } else {
        alu__DOT___153 = (0xffU & (IData)(alu__DOT___139));
        alu__DOT___122 = (0xffU & (IData)(alu__DOT___108));
    }
    vlSelfRef.y = (0xffU & ((0U == (IData)(vlSelfRef.op))
                             ? ((IData)(vlSelfRef.a) 
                                + (IData)(vlSelfRef.b))
                             : ((1U == (IData)(vlSelfRef.op))
                                 ? ((IData)(vlSelfRef.a) 
                                    - (IData)(vlSelfRef.b))
                                 : ((2U == (IData)(vlSelfRef.op))
                                     ? (0xffffU & ((IData)(vlSelfRef.a) 
                                                   * (IData)(vlSelfRef.b)))
                                     : ((3U == (IData)(vlSelfRef.op))
                                         ? 0xffU : 
                                        ((4U == (IData)(vlSelfRef.op))
                                          ? ((IData)(vlSelfRef.a) 
                                             & (IData)(vlSelfRef.b))
                                          : ((5U == (IData)(vlSelfRef.op))
                                              ? ((IData)(vlSelfRef.a) 
                                                 | (IData)(vlSelfRef.b))
                                              : ((6U 
                                                  == (IData)(vlSelfRef.op))
                                                  ? (IData)(alu__DOT___153)
                                                  : 
                                                 ((7U 
                                                   == (IData)(vlSelfRef.op))
                                                   ? (IData)(alu__DOT___122)
                                                   : 
                                                  ((8U 
                                                    == (IData)(vlSelfRef.op))
                                                    ? (IData)(alu__DOT___122)
                                                    : 
                                                   ((9U 
                                                     == (IData)(vlSelfRef.op))
                                                     ? (IData)(alu__DOT___153)
                                                     : 0U)))))))))));
}

void Vhardcaml_alu___024root___eval_triggers__ico(Vhardcaml_alu___024root* vlSelf);

bool Vhardcaml_alu___024root___eval_phase__ico(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_phase__ico\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __VicoExecute;
    // Body
    Vhardcaml_alu___024root___eval_triggers__ico(vlSelf);
    __VicoExecute = vlSelfRef.__VicoTriggered.any();
    if (__VicoExecute) {
        Vhardcaml_alu___024root___eval_ico(vlSelf);
    }
    return (__VicoExecute);
}

void Vhardcaml_alu___024root___eval_act(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_act\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
}

void Vhardcaml_alu___024root___eval_nba(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_nba\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
}

void Vhardcaml_alu___024root___eval_triggers__act(Vhardcaml_alu___024root* vlSelf);

bool Vhardcaml_alu___024root___eval_phase__act(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_phase__act\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlTriggerVec<0> __VpreTriggered;
    CData/*0:0*/ __VactExecute;
    // Body
    Vhardcaml_alu___024root___eval_triggers__act(vlSelf);
    __VactExecute = vlSelfRef.__VactTriggered.any();
    if (__VactExecute) {
        __VpreTriggered.andNot(vlSelfRef.__VactTriggered, vlSelfRef.__VnbaTriggered);
        vlSelfRef.__VnbaTriggered.thisOr(vlSelfRef.__VactTriggered);
        Vhardcaml_alu___024root___eval_act(vlSelf);
    }
    return (__VactExecute);
}

bool Vhardcaml_alu___024root___eval_phase__nba(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_phase__nba\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __VnbaExecute;
    // Body
    __VnbaExecute = vlSelfRef.__VnbaTriggered.any();
    if (__VnbaExecute) {
        Vhardcaml_alu___024root___eval_nba(vlSelf);
        vlSelfRef.__VnbaTriggered.clear();
    }
    return (__VnbaExecute);
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vhardcaml_alu___024root___dump_triggers__ico(Vhardcaml_alu___024root* vlSelf);
#endif  // VL_DEBUG
#ifdef VL_DEBUG
VL_ATTR_COLD void Vhardcaml_alu___024root___dump_triggers__nba(Vhardcaml_alu___024root* vlSelf);
#endif  // VL_DEBUG
#ifdef VL_DEBUG
VL_ATTR_COLD void Vhardcaml_alu___024root___dump_triggers__act(Vhardcaml_alu___024root* vlSelf);
#endif  // VL_DEBUG

void Vhardcaml_alu___024root___eval(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __VicoIterCount;
    CData/*0:0*/ __VicoContinue;
    IData/*31:0*/ __VnbaIterCount;
    CData/*0:0*/ __VnbaContinue;
    // Body
    __VicoIterCount = 0U;
    vlSelfRef.__VicoFirstIteration = 1U;
    __VicoContinue = 1U;
    while (__VicoContinue) {
        if (VL_UNLIKELY(((0x64U < __VicoIterCount)))) {
#ifdef VL_DEBUG
            Vhardcaml_alu___024root___dump_triggers__ico(vlSelf);
#endif
            VL_FATAL_MT("/tmp/hardcaml_alu.sv", 4, "", "Input combinational region did not converge.");
        }
        __VicoIterCount = ((IData)(1U) + __VicoIterCount);
        __VicoContinue = 0U;
        if (Vhardcaml_alu___024root___eval_phase__ico(vlSelf)) {
            __VicoContinue = 1U;
        }
        vlSelfRef.__VicoFirstIteration = 0U;
    }
    __VnbaIterCount = 0U;
    __VnbaContinue = 1U;
    while (__VnbaContinue) {
        if (VL_UNLIKELY(((0x64U < __VnbaIterCount)))) {
#ifdef VL_DEBUG
            Vhardcaml_alu___024root___dump_triggers__nba(vlSelf);
#endif
            VL_FATAL_MT("/tmp/hardcaml_alu.sv", 4, "", "NBA region did not converge.");
        }
        __VnbaIterCount = ((IData)(1U) + __VnbaIterCount);
        __VnbaContinue = 0U;
        vlSelfRef.__VactIterCount = 0U;
        vlSelfRef.__VactContinue = 1U;
        while (vlSelfRef.__VactContinue) {
            if (VL_UNLIKELY(((0x64U < vlSelfRef.__VactIterCount)))) {
#ifdef VL_DEBUG
                Vhardcaml_alu___024root___dump_triggers__act(vlSelf);
#endif
                VL_FATAL_MT("/tmp/hardcaml_alu.sv", 4, "", "Active region did not converge.");
            }
            vlSelfRef.__VactIterCount = ((IData)(1U) 
                                         + vlSelfRef.__VactIterCount);
            vlSelfRef.__VactContinue = 0U;
            if (Vhardcaml_alu___024root___eval_phase__act(vlSelf)) {
                vlSelfRef.__VactContinue = 1U;
            }
        }
        if (Vhardcaml_alu___024root___eval_phase__nba(vlSelf)) {
            __VnbaContinue = 1U;
        }
    }
}

#ifdef VL_DEBUG
void Vhardcaml_alu___024root___eval_debug_assertions(Vhardcaml_alu___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vhardcaml_alu___024root___eval_debug_assertions\n"); );
    Vhardcaml_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (VL_UNLIKELY(((vlSelfRef.op & 0xf0U)))) {
        Verilated::overWidthError("op");}
}
#endif  // VL_DEBUG
