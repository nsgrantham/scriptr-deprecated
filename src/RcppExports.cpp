// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// callgetopt
List callgetopt(CharacterVector args, ListOf<List> opts);
RcppExport SEXP _scriptr_callgetopt(SEXP argsSEXP, SEXP optsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type args(argsSEXP);
    Rcpp::traits::input_parameter< ListOf<List> >::type opts(optsSEXP);
    rcpp_result_gen = Rcpp::wrap(callgetopt(args, opts));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_scriptr_callgetopt", (DL_FUNC) &_scriptr_callgetopt, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_scriptr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
