// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Model implementation (design independent parts)

#include "Vhardcaml_alu__pch.h"

//============================================================
// Constructors

Vhardcaml_alu::Vhardcaml_alu(VerilatedContext* _vcontextp__, const char* _vcname__)
    : VerilatedModel{*_vcontextp__}
    , vlSymsp{new Vhardcaml_alu__Syms(contextp(), _vcname__, this)}
    , a{vlSymsp->TOP.a}
    , b{vlSymsp->TOP.b}
    , op{vlSymsp->TOP.op}
    , y{vlSymsp->TOP.y}
    , rootp{&(vlSymsp->TOP)}
{
    // Register model with the context
    contextp()->addModel(this);
}

Vhardcaml_alu::Vhardcaml_alu(const char* _vcname__)
    : Vhardcaml_alu(Verilated::threadContextp(), _vcname__)
{
}

//============================================================
// Destructor

Vhardcaml_alu::~Vhardcaml_alu() {
    delete vlSymsp;
}

//============================================================
// Evaluation function

#ifdef VL_DEBUG
void Vhardcaml_alu___024root___eval_debug_assertions(Vhardcaml_alu___024root* vlSelf);
#endif  // VL_DEBUG
void Vhardcaml_alu___024root___eval_static(Vhardcaml_alu___024root* vlSelf);
void Vhardcaml_alu___024root___eval_initial(Vhardcaml_alu___024root* vlSelf);
void Vhardcaml_alu___024root___eval_settle(Vhardcaml_alu___024root* vlSelf);
void Vhardcaml_alu___024root___eval(Vhardcaml_alu___024root* vlSelf);

void Vhardcaml_alu::eval_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+++++TOP Evaluate Vhardcaml_alu::eval_step\n"); );
#ifdef VL_DEBUG
    // Debug assertions
    Vhardcaml_alu___024root___eval_debug_assertions(&(vlSymsp->TOP));
#endif  // VL_DEBUG
    vlSymsp->__Vm_deleter.deleteAll();
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) {
        vlSymsp->__Vm_didInit = true;
        VL_DEBUG_IF(VL_DBG_MSGF("+ Initial\n"););
        Vhardcaml_alu___024root___eval_static(&(vlSymsp->TOP));
        Vhardcaml_alu___024root___eval_initial(&(vlSymsp->TOP));
        Vhardcaml_alu___024root___eval_settle(&(vlSymsp->TOP));
    }
    VL_DEBUG_IF(VL_DBG_MSGF("+ Eval\n"););
    Vhardcaml_alu___024root___eval(&(vlSymsp->TOP));
    // Evaluate cleanup
    Verilated::endOfEval(vlSymsp->__Vm_evalMsgQp);
}

//============================================================
// Events and timing
bool Vhardcaml_alu::eventsPending() { return false; }

uint64_t Vhardcaml_alu::nextTimeSlot() {
    VL_FATAL_MT(__FILE__, __LINE__, "", "No delays in the design");
    return 0;
}

//============================================================
// Utilities

const char* Vhardcaml_alu::name() const {
    return vlSymsp->name();
}

//============================================================
// Invoke final blocks

void Vhardcaml_alu___024root___eval_final(Vhardcaml_alu___024root* vlSelf);

VL_ATTR_COLD void Vhardcaml_alu::final() {
    Vhardcaml_alu___024root___eval_final(&(vlSymsp->TOP));
}

//============================================================
// Implementations of abstract methods from VerilatedModel

const char* Vhardcaml_alu::hierName() const { return vlSymsp->name(); }
const char* Vhardcaml_alu::modelName() const { return "Vhardcaml_alu"; }
unsigned Vhardcaml_alu::threads() const { return 1; }
void Vhardcaml_alu::prepareClone() const { contextp()->prepareClone(); }
void Vhardcaml_alu::atClone() const {
    contextp()->threadPoolpOnClone();
}
