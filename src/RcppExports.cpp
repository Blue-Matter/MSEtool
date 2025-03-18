// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// vecminInd
int vecminInd(NumericVector x);
RcppExport SEXP _MSEtool_vecminInd(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(vecminInd(x));
    return rcpp_result_gen;
END_RCPP
}
// LinInterp_cpp
double LinInterp_cpp(NumericVector x, NumericVector y, double xlev);
RcppExport SEXP _MSEtool_LinInterp_cpp(SEXP xSEXP, SEXP ySEXP, SEXP xlevSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type xlev(xlevSEXP);
    rcpp_result_gen = Rcpp::wrap(LinInterp_cpp(x, y, xlev));
    return rcpp_result_gen;
END_RCPP
}
// MSYCalcs
NumericVector MSYCalcs(double logF, NumericVector M_at_Age, NumericVector Wt_at_Age, NumericVector Mat_at_Age, NumericVector Fec_at_Age, NumericVector V_at_Age, NumericVector Wt_at_Age_C, int maxage, Function relRfun, List SRRpars, double R0x, int SRrelx, double hx, double SSBpR, int opt, int plusgroup, double spawn_time_frac);
RcppExport SEXP _MSEtool_MSYCalcs(SEXP logFSEXP, SEXP M_at_AgeSEXP, SEXP Wt_at_AgeSEXP, SEXP Mat_at_AgeSEXP, SEXP Fec_at_AgeSEXP, SEXP V_at_AgeSEXP, SEXP Wt_at_Age_CSEXP, SEXP maxageSEXP, SEXP relRfunSEXP, SEXP SRRparsSEXP, SEXP R0xSEXP, SEXP SRrelxSEXP, SEXP hxSEXP, SEXP SSBpRSEXP, SEXP optSEXP, SEXP plusgroupSEXP, SEXP spawn_time_fracSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type logF(logFSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type M_at_Age(M_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Wt_at_Age(Wt_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Mat_at_Age(Mat_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Fec_at_Age(Fec_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type V_at_Age(V_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Wt_at_Age_C(Wt_at_Age_CSEXP);
    Rcpp::traits::input_parameter< int >::type maxage(maxageSEXP);
    Rcpp::traits::input_parameter< Function >::type relRfun(relRfunSEXP);
    Rcpp::traits::input_parameter< List >::type SRRpars(SRRparsSEXP);
    Rcpp::traits::input_parameter< double >::type R0x(R0xSEXP);
    Rcpp::traits::input_parameter< int >::type SRrelx(SRrelxSEXP);
    Rcpp::traits::input_parameter< double >::type hx(hxSEXP);
    Rcpp::traits::input_parameter< double >::type SSBpR(SSBpRSEXP);
    Rcpp::traits::input_parameter< int >::type opt(optSEXP);
    Rcpp::traits::input_parameter< int >::type plusgroup(plusgroupSEXP);
    Rcpp::traits::input_parameter< double >::type spawn_time_frac(spawn_time_fracSEXP);
    rcpp_result_gen = Rcpp::wrap(MSYCalcs(logF, M_at_Age, Wt_at_Age, Mat_at_Age, Fec_at_Age, V_at_Age, Wt_at_Age_C, maxage, relRfun, SRRpars, R0x, SRrelx, hx, SSBpR, opt, plusgroup, spawn_time_frac));
    return rcpp_result_gen;
END_RCPP
}
// Ref_int_cpp
NumericMatrix Ref_int_cpp(NumericVector F_search, NumericVector M_at_Age, NumericVector Wt_at_Age, NumericVector Mat_at_Age, NumericVector Fec_at_Age, NumericVector V_at_Age, NumericVector Wt_at_Age_C, Function relRfun, List SRRpars, int maxage, int plusgroup, double spawn_time_frac);
RcppExport SEXP _MSEtool_Ref_int_cpp(SEXP F_searchSEXP, SEXP M_at_AgeSEXP, SEXP Wt_at_AgeSEXP, SEXP Mat_at_AgeSEXP, SEXP Fec_at_AgeSEXP, SEXP V_at_AgeSEXP, SEXP Wt_at_Age_CSEXP, SEXP relRfunSEXP, SEXP SRRparsSEXP, SEXP maxageSEXP, SEXP plusgroupSEXP, SEXP spawn_time_fracSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type F_search(F_searchSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type M_at_Age(M_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Wt_at_Age(Wt_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Mat_at_Age(Mat_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Fec_at_Age(Fec_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type V_at_Age(V_at_AgeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Wt_at_Age_C(Wt_at_Age_CSEXP);
    Rcpp::traits::input_parameter< Function >::type relRfun(relRfunSEXP);
    Rcpp::traits::input_parameter< List >::type SRRpars(SRRparsSEXP);
    Rcpp::traits::input_parameter< int >::type maxage(maxageSEXP);
    Rcpp::traits::input_parameter< int >::type plusgroup(plusgroupSEXP);
    Rcpp::traits::input_parameter< double >::type spawn_time_frac(spawn_time_fracSEXP);
    rcpp_result_gen = Rcpp::wrap(Ref_int_cpp(F_search, M_at_Age, Wt_at_Age, Mat_at_Age, Fec_at_Age, V_at_Age, Wt_at_Age_C, relRfun, SRRpars, maxage, plusgroup, spawn_time_frac));
    return rcpp_result_gen;
END_RCPP
}
// CalcBiomass_
List CalcBiomass_(List BiomassList, List NumberAtAgeAreaList, List WeightList, int TSindex);
RcppExport SEXP _MSEtool_CalcBiomass_(SEXP BiomassListSEXP, SEXP NumberAtAgeAreaListSEXP, SEXP WeightListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type BiomassList(BiomassListSEXP);
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type WeightList(WeightListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcBiomass_(BiomassList, NumberAtAgeAreaList, WeightList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcVBiomass_
List CalcVBiomass_(List VBiomassAreaList, List NumberAtAgeAreaList, List FleetWeightAtAgeList, List SelectivityAtAgeList, List ClosureAreaList, int TSindex);
RcppExport SEXP _MSEtool_CalcVBiomass_(SEXP VBiomassAreaListSEXP, SEXP NumberAtAgeAreaListSEXP, SEXP FleetWeightAtAgeListSEXP, SEXP SelectivityAtAgeListSEXP, SEXP ClosureAreaListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type VBiomassAreaList(VBiomassAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type FleetWeightAtAgeList(FleetWeightAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type SelectivityAtAgeList(SelectivityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type ClosureAreaList(ClosureAreaListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcVBiomass_(VBiomassAreaList, NumberAtAgeAreaList, FleetWeightAtAgeList, SelectivityAtAgeList, ClosureAreaList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcDensity_
List CalcDensity_(List DensityAreaList, List VBiomassAreaList, List RelativeSizeList, int TSindex);
RcppExport SEXP _MSEtool_CalcDensity_(SEXP DensityAreaListSEXP, SEXP VBiomassAreaListSEXP, SEXP RelativeSizeListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type DensityAreaList(DensityAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type VBiomassAreaList(VBiomassAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type RelativeSizeList(RelativeSizeListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcDensity_(DensityAreaList, VBiomassAreaList, RelativeSizeList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// DistEffort_
List DistEffort_(List EffortAreaList, List VBiomassAreaList, List EffortList, int TSindex);
RcppExport SEXP _MSEtool_DistEffort_(SEXP EffortAreaListSEXP, SEXP VBiomassAreaListSEXP, SEXP EffortListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type EffortAreaList(EffortAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type VBiomassAreaList(VBiomassAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type EffortList(EffortListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(DistEffort_(EffortAreaList, VBiomassAreaList, EffortList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcFArea_
List CalcFArea_(List FDeadAtAgeAreaList, List FRetainAtAgeAreaList, List EffortAreaList, List RelativeSizeList, List CatchabilityList, List SelectivityAtAgeList, List RetentionAtAgeList, List DiscardMortalityAtAgeList, int TSindex);
RcppExport SEXP _MSEtool_CalcFArea_(SEXP FDeadAtAgeAreaListSEXP, SEXP FRetainAtAgeAreaListSEXP, SEXP EffortAreaListSEXP, SEXP RelativeSizeListSEXP, SEXP CatchabilityListSEXP, SEXP SelectivityAtAgeListSEXP, SEXP RetentionAtAgeListSEXP, SEXP DiscardMortalityAtAgeListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type FDeadAtAgeAreaList(FDeadAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type FRetainAtAgeAreaList(FRetainAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type EffortAreaList(EffortAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type RelativeSizeList(RelativeSizeListSEXP);
    Rcpp::traits::input_parameter< List >::type CatchabilityList(CatchabilityListSEXP);
    Rcpp::traits::input_parameter< List >::type SelectivityAtAgeList(SelectivityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type RetentionAtAgeList(RetentionAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type DiscardMortalityAtAgeList(DiscardMortalityAtAgeListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcFArea_(FDeadAtAgeAreaList, FRetainAtAgeAreaList, EffortAreaList, RelativeSizeList, CatchabilityList, SelectivityAtAgeList, RetentionAtAgeList, DiscardMortalityAtAgeList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcCatch_
List CalcCatch_(List RemovalAtAgeAreaList, List RetainAtAgeAreaList, List RemovalNumberAtAgeList, List RetainNumberAtAgeList, List RemovalBiomassAtAgeList, List RetainBiomassAtAgeList, List NaturalMortalityAtAgeList, List FleetWeightAtAgeList, List NumberAtAgeAreaList, List FDeadAtAgeAreaList, List FRetainAtAgeAreaList, int TSindex);
RcppExport SEXP _MSEtool_CalcCatch_(SEXP RemovalAtAgeAreaListSEXP, SEXP RetainAtAgeAreaListSEXP, SEXP RemovalNumberAtAgeListSEXP, SEXP RetainNumberAtAgeListSEXP, SEXP RemovalBiomassAtAgeListSEXP, SEXP RetainBiomassAtAgeListSEXP, SEXP NaturalMortalityAtAgeListSEXP, SEXP FleetWeightAtAgeListSEXP, SEXP NumberAtAgeAreaListSEXP, SEXP FDeadAtAgeAreaListSEXP, SEXP FRetainAtAgeAreaListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type RemovalAtAgeAreaList(RemovalAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type RetainAtAgeAreaList(RetainAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type RemovalNumberAtAgeList(RemovalNumberAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type RetainNumberAtAgeList(RetainNumberAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type RemovalBiomassAtAgeList(RemovalBiomassAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type RetainBiomassAtAgeList(RetainBiomassAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type NaturalMortalityAtAgeList(NaturalMortalityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type FleetWeightAtAgeList(FleetWeightAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type FDeadAtAgeAreaList(FDeadAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type FRetainAtAgeAreaList(FRetainAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcCatch_(RemovalAtAgeAreaList, RetainAtAgeAreaList, RemovalNumberAtAgeList, RetainNumberAtAgeList, RemovalBiomassAtAgeList, RetainBiomassAtAgeList, NaturalMortalityAtAgeList, FleetWeightAtAgeList, NumberAtAgeAreaList, FDeadAtAgeAreaList, FRetainAtAgeAreaList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcFfromCatch_
List CalcFfromCatch_(List FDeadAtAgeList, List FRetainAtAgeList, List NumberAtAgeAreaList, List RemovalNumberAtAgeList, List NaturalMortalityAtAgeList, List SelectivityAtAgeList, List RetentionAtAgeList, List DiscardMortalityAtAgeList, List FDeadAtAgeAreaList, List FRetainAtAgeAreaList, int TSindex);
RcppExport SEXP _MSEtool_CalcFfromCatch_(SEXP FDeadAtAgeListSEXP, SEXP FRetainAtAgeListSEXP, SEXP NumberAtAgeAreaListSEXP, SEXP RemovalNumberAtAgeListSEXP, SEXP NaturalMortalityAtAgeListSEXP, SEXP SelectivityAtAgeListSEXP, SEXP RetentionAtAgeListSEXP, SEXP DiscardMortalityAtAgeListSEXP, SEXP FDeadAtAgeAreaListSEXP, SEXP FRetainAtAgeAreaListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type FDeadAtAgeList(FDeadAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type FRetainAtAgeList(FRetainAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type RemovalNumberAtAgeList(RemovalNumberAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type NaturalMortalityAtAgeList(NaturalMortalityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type SelectivityAtAgeList(SelectivityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type RetentionAtAgeList(RetentionAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type DiscardMortalityAtAgeList(DiscardMortalityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type FDeadAtAgeAreaList(FDeadAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type FRetainAtAgeAreaList(FRetainAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcFfromCatch_(FDeadAtAgeList, FRetainAtAgeList, NumberAtAgeAreaList, RemovalNumberAtAgeList, NaturalMortalityAtAgeList, SelectivityAtAgeList, RetentionAtAgeList, DiscardMortalityAtAgeList, FDeadAtAgeAreaList, FRetainAtAgeAreaList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcSpawnProduction_
List CalcSpawnProduction_(List SProductionList, List SBiomassList, List NumberAtAgeAreaList, List NaturalMortalityAtAgeList, List FecundityAtAgeList, List WeightAtAgeList, List MaturityAtAgeList, arma::vec SpawnTimeFrac, List SPFromList, List FDeadAtAgeList, int TSindex);
RcppExport SEXP _MSEtool_CalcSpawnProduction_(SEXP SProductionListSEXP, SEXP SBiomassListSEXP, SEXP NumberAtAgeAreaListSEXP, SEXP NaturalMortalityAtAgeListSEXP, SEXP FecundityAtAgeListSEXP, SEXP WeightAtAgeListSEXP, SEXP MaturityAtAgeListSEXP, SEXP SpawnTimeFracSEXP, SEXP SPFromListSEXP, SEXP FDeadAtAgeListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type SProductionList(SProductionListSEXP);
    Rcpp::traits::input_parameter< List >::type SBiomassList(SBiomassListSEXP);
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type NaturalMortalityAtAgeList(NaturalMortalityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type FecundityAtAgeList(FecundityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type WeightAtAgeList(WeightAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type MaturityAtAgeList(MaturityAtAgeListSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type SpawnTimeFrac(SpawnTimeFracSEXP);
    Rcpp::traits::input_parameter< List >::type SPFromList(SPFromListSEXP);
    Rcpp::traits::input_parameter< List >::type FDeadAtAgeList(FDeadAtAgeListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcSpawnProduction_(SProductionList, SBiomassList, NumberAtAgeAreaList, NaturalMortalityAtAgeList, FecundityAtAgeList, WeightAtAgeList, MaturityAtAgeList, SpawnTimeFrac, SPFromList, FDeadAtAgeList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcRecruitment_
NumericVector CalcRecruitment_(List SProductionList, List SP0List, List R0List, List RecDevsList, List SRRModelList, List SRRParsList, int TSindex);
RcppExport SEXP _MSEtool_CalcRecruitment_(SEXP SProductionListSEXP, SEXP SP0ListSEXP, SEXP R0ListSEXP, SEXP RecDevsListSEXP, SEXP SRRModelListSEXP, SEXP SRRParsListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type SProductionList(SProductionListSEXP);
    Rcpp::traits::input_parameter< List >::type SP0List(SP0ListSEXP);
    Rcpp::traits::input_parameter< List >::type R0List(R0ListSEXP);
    Rcpp::traits::input_parameter< List >::type RecDevsList(RecDevsListSEXP);
    Rcpp::traits::input_parameter< List >::type SRRModelList(SRRModelListSEXP);
    Rcpp::traits::input_parameter< List >::type SRRParsList(SRRParsListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcRecruitment_(SProductionList, SP0List, R0List, RecDevsList, SRRModelList, SRRParsList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// AddRecruits_
List AddRecruits_(List NumberAtAgeAreaList, arma::vec Recruits, List UnfishedDistList, int TSindex);
RcppExport SEXP _MSEtool_AddRecruits_(SEXP NumberAtAgeAreaListSEXP, SEXP RecruitsSEXP, SEXP UnfishedDistListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Recruits(RecruitsSEXP);
    Rcpp::traits::input_parameter< List >::type UnfishedDistList(UnfishedDistListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(AddRecruits_(NumberAtAgeAreaList, Recruits, UnfishedDistList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcNumberNext_
List CalcNumberNext_(List NumberAtAgeAreaList, List NaturalMortalityAtAgeList, List FDeadAtAgeAreaList, List SemelparousList, List AgesList, int TSindex);
RcppExport SEXP _MSEtool_CalcNumberNext_(SEXP NumberAtAgeAreaListSEXP, SEXP NaturalMortalityAtAgeListSEXP, SEXP FDeadAtAgeAreaListSEXP, SEXP SemelparousListSEXP, SEXP AgesListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type NaturalMortalityAtAgeList(NaturalMortalityAtAgeListSEXP);
    Rcpp::traits::input_parameter< List >::type FDeadAtAgeAreaList(FDeadAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type SemelparousList(SemelparousListSEXP);
    Rcpp::traits::input_parameter< List >::type AgesList(AgesListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcNumberNext_(NumberAtAgeAreaList, NaturalMortalityAtAgeList, FDeadAtAgeAreaList, SemelparousList, AgesList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// MoveStock_
List MoveStock_(List NumberAtAgeAreaList, List MovementList, int TSindex);
RcppExport SEXP _MSEtool_MoveStock_(SEXP NumberAtAgeAreaListSEXP, SEXP MovementListSEXP, SEXP TSindexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type NumberAtAgeAreaList(NumberAtAgeAreaListSEXP);
    Rcpp::traits::input_parameter< List >::type MovementList(MovementListSEXP);
    Rcpp::traits::input_parameter< int >::type TSindex(TSindexSEXP);
    rcpp_result_gen = Rcpp::wrap(MoveStock_(NumberAtAgeAreaList, MovementList, TSindex));
    return rcpp_result_gen;
END_RCPP
}
// CalcPopDynamics_
List CalcPopDynamics_(List OMListSim, NumericVector TimeSteps);
RcppExport SEXP _MSEtool_CalcPopDynamics_(SEXP OMListSimSEXP, SEXP TimeStepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type OMListSim(OMListSimSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type TimeSteps(TimeStepsSEXP);
    rcpp_result_gen = Rcpp::wrap(CalcPopDynamics_(OMListSim, TimeSteps));
    return rcpp_result_gen;
END_RCPP
}
// calcVatAge
NumericMatrix calcVatAge(NumericMatrix len_at_age, NumericMatrix len_aa_sd, NumericMatrix sel_at_length, int n_age, int nyears, int proyears, NumericVector CAL_binsmid);
RcppExport SEXP _MSEtool_calcVatAge(SEXP len_at_ageSEXP, SEXP len_aa_sdSEXP, SEXP sel_at_lengthSEXP, SEXP n_ageSEXP, SEXP nyearsSEXP, SEXP proyearsSEXP, SEXP CAL_binsmidSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type len_at_age(len_at_ageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type len_aa_sd(len_aa_sdSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type sel_at_length(sel_at_lengthSEXP);
    Rcpp::traits::input_parameter< int >::type n_age(n_ageSEXP);
    Rcpp::traits::input_parameter< int >::type nyears(nyearsSEXP);
    Rcpp::traits::input_parameter< int >::type proyears(proyearsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CAL_binsmid(CAL_binsmidSEXP);
    rcpp_result_gen = Rcpp::wrap(calcVatAge(len_at_age, len_aa_sd, sel_at_length, n_age, nyears, proyears, CAL_binsmid));
    return rcpp_result_gen;
END_RCPP
}
// combine
NumericVector combine(const List& list);
RcppExport SEXP _MSEtool_combine(SEXP listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type list(listSEXP);
    rcpp_result_gen = Rcpp::wrap(combine(list));
    return rcpp_result_gen;
END_RCPP
}
// get_freq
NumericVector get_freq(NumericVector x, double width, double origin, int outlen);
RcppExport SEXP _MSEtool_get_freq(SEXP xSEXP, SEXP widthSEXP, SEXP originSEXP, SEXP outlenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type origin(originSEXP);
    Rcpp::traits::input_parameter< int >::type outlen(outlenSEXP);
    rcpp_result_gen = Rcpp::wrap(get_freq(x, width, origin, outlen));
    return rcpp_result_gen;
END_RCPP
}
// get_freq2
NumericVector get_freq2(NumericVector x, NumericVector CAL_bins, int outlen);
RcppExport SEXP _MSEtool_get_freq2(SEXP xSEXP, SEXP CAL_binsSEXP, SEXP outlenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CAL_bins(CAL_binsSEXP);
    Rcpp::traits::input_parameter< int >::type outlen(outlenSEXP);
    rcpp_result_gen = Rcpp::wrap(get_freq2(x, CAL_bins, outlen));
    return rcpp_result_gen;
END_RCPP
}
// rnormSelect2
NumericVector rnormSelect2(int N, int mi, int ma);
RcppExport SEXP _MSEtool_rnormSelect2(SEXP NSEXP, SEXP miSEXP, SEXP maSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type mi(miSEXP);
    Rcpp::traits::input_parameter< int >::type ma(maSEXP);
    rcpp_result_gen = Rcpp::wrap(rnormSelect2(N, mi, ma));
    return rcpp_result_gen;
END_RCPP
}
// tdnorm
NumericVector tdnorm(NumericVector x, double mi, double ma);
RcppExport SEXP _MSEtool_tdnorm(SEXP xSEXP, SEXP miSEXP, SEXP maSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mi(miSEXP);
    Rcpp::traits::input_parameter< double >::type ma(maSEXP);
    rcpp_result_gen = Rcpp::wrap(tdnorm(x, mi, ma));
    return rcpp_result_gen;
END_RCPP
}
// genSizeComp
NumericMatrix genSizeComp(NumericMatrix VulnN, NumericVector CAL_binsmid, NumericVector CAL_bins, NumericMatrix selCurve, double CAL_ESS, double CAL_nsamp, NumericVector Linfs, NumericVector Ks, NumericVector t0s, double LenCV, double truncSD);
RcppExport SEXP _MSEtool_genSizeComp(SEXP VulnNSEXP, SEXP CAL_binsmidSEXP, SEXP CAL_binsSEXP, SEXP selCurveSEXP, SEXP CAL_ESSSEXP, SEXP CAL_nsampSEXP, SEXP LinfsSEXP, SEXP KsSEXP, SEXP t0sSEXP, SEXP LenCVSEXP, SEXP truncSDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type VulnN(VulnNSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CAL_binsmid(CAL_binsmidSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CAL_bins(CAL_binsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type selCurve(selCurveSEXP);
    Rcpp::traits::input_parameter< double >::type CAL_ESS(CAL_ESSSEXP);
    Rcpp::traits::input_parameter< double >::type CAL_nsamp(CAL_nsampSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Linfs(LinfsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Ks(KsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type t0s(t0sSEXP);
    Rcpp::traits::input_parameter< double >::type LenCV(LenCVSEXP);
    Rcpp::traits::input_parameter< double >::type truncSD(truncSDSEXP);
    rcpp_result_gen = Rcpp::wrap(genSizeComp(VulnN, CAL_binsmid, CAL_bins, selCurve, CAL_ESS, CAL_nsamp, Linfs, Ks, t0s, LenCV, truncSD));
    return rcpp_result_gen;
END_RCPP
}
// genSizeComp2
NumericMatrix genSizeComp2(NumericMatrix VulnN, NumericVector CAL_binsmid, NumericVector CAL_bins, NumericMatrix selCurve, double CAL_ESS, double CAL_nsamp, NumericVector Linfs, NumericVector Ks, NumericVector t0s, double LenCV, double truncSD);
RcppExport SEXP _MSEtool_genSizeComp2(SEXP VulnNSEXP, SEXP CAL_binsmidSEXP, SEXP CAL_binsSEXP, SEXP selCurveSEXP, SEXP CAL_ESSSEXP, SEXP CAL_nsampSEXP, SEXP LinfsSEXP, SEXP KsSEXP, SEXP t0sSEXP, SEXP LenCVSEXP, SEXP truncSDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type VulnN(VulnNSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CAL_binsmid(CAL_binsmidSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CAL_bins(CAL_binsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type selCurve(selCurveSEXP);
    Rcpp::traits::input_parameter< double >::type CAL_ESS(CAL_ESSSEXP);
    Rcpp::traits::input_parameter< double >::type CAL_nsamp(CAL_nsampSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Linfs(LinfsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Ks(KsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type t0s(t0sSEXP);
    Rcpp::traits::input_parameter< double >::type LenCV(LenCVSEXP);
    Rcpp::traits::input_parameter< double >::type truncSD(truncSDSEXP);
    rcpp_result_gen = Rcpp::wrap(genSizeComp2(VulnN, CAL_binsmid, CAL_bins, selCurve, CAL_ESS, CAL_nsamp, Linfs, Ks, t0s, LenCV, truncSD));
    return rcpp_result_gen;
END_RCPP
}
// grav
List grav(arma::vec log_visc, arma::vec log_grav, arma::vec fracs, int nareas);
RcppExport SEXP _MSEtool_grav(SEXP log_viscSEXP, SEXP log_gravSEXP, SEXP fracsSEXP, SEXP nareasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type log_visc(log_viscSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type log_grav(log_gravSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type fracs(fracsSEXP);
    Rcpp::traits::input_parameter< int >::type nareas(nareasSEXP);
    rcpp_result_gen = Rcpp::wrap(grav(log_visc, log_grav, fracs, nareas));
    return rcpp_result_gen;
END_RCPP
}
// movfit_Rcpp
double movfit_Rcpp(NumericVector par, double prb, double frac);
RcppExport SEXP _MSEtool_movfit_Rcpp(SEXP parSEXP, SEXP prbSEXP, SEXP fracSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type par(parSEXP);
    Rcpp::traits::input_parameter< double >::type prb(prbSEXP);
    Rcpp::traits::input_parameter< double >::type frac(fracSEXP);
    rcpp_result_gen = Rcpp::wrap(movfit_Rcpp(par, prb, frac));
    return rcpp_result_gen;
END_RCPP
}
// popdynOneTScpp
arma::mat popdynOneTScpp(double nareas, double maxage, NumericMatrix Ncurr, Rcpp::NumericMatrix Zcurr, int plusgroup);
RcppExport SEXP _MSEtool_popdynOneTScpp(SEXP nareasSEXP, SEXP maxageSEXP, SEXP NcurrSEXP, SEXP ZcurrSEXP, SEXP plusgroupSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nareas(nareasSEXP);
    Rcpp::traits::input_parameter< double >::type maxage(maxageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Ncurr(NcurrSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Zcurr(ZcurrSEXP);
    Rcpp::traits::input_parameter< int >::type plusgroup(plusgroupSEXP);
    rcpp_result_gen = Rcpp::wrap(popdynOneTScpp(nareas, maxage, Ncurr, Zcurr, plusgroup));
    return rcpp_result_gen;
END_RCPP
}
// movestockCPP
arma::mat movestockCPP(double nareas, double maxage, arma::cube mov, NumericMatrix Number);
RcppExport SEXP _MSEtool_movestockCPP(SEXP nareasSEXP, SEXP maxageSEXP, SEXP movSEXP, SEXP NumberSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nareas(nareasSEXP);
    Rcpp::traits::input_parameter< double >::type maxage(maxageSEXP);
    Rcpp::traits::input_parameter< arma::cube >::type mov(movSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Number(NumberSEXP);
    rcpp_result_gen = Rcpp::wrap(movestockCPP(nareas, maxage, mov, Number));
    return rcpp_result_gen;
END_RCPP
}
// popdynCPP
List popdynCPP(double nareas, double maxage, arma::mat Ncurr, double pyears, arma::mat M_age, arma::vec Asize_c, arma::mat MatAge, arma::mat WtAge, arma::mat FecAge, arma::mat Vuln, arma::mat Retc, arma::vec Prec, List movc, double SRrelc, arma::vec Effind, double Spat_targc, double hc, NumericVector R0c, NumericVector SSBpRc, NumericVector aRc, NumericVector bRc, double Qc, double Fapic, double maxF, arma::mat MPA, int control, double SSB0c, Function SRRfun, List SRRpars, int plusgroup, double spawn_time_frac);
RcppExport SEXP _MSEtool_popdynCPP(SEXP nareasSEXP, SEXP maxageSEXP, SEXP NcurrSEXP, SEXP pyearsSEXP, SEXP M_ageSEXP, SEXP Asize_cSEXP, SEXP MatAgeSEXP, SEXP WtAgeSEXP, SEXP FecAgeSEXP, SEXP VulnSEXP, SEXP RetcSEXP, SEXP PrecSEXP, SEXP movcSEXP, SEXP SRrelcSEXP, SEXP EffindSEXP, SEXP Spat_targcSEXP, SEXP hcSEXP, SEXP R0cSEXP, SEXP SSBpRcSEXP, SEXP aRcSEXP, SEXP bRcSEXP, SEXP QcSEXP, SEXP FapicSEXP, SEXP maxFSEXP, SEXP MPASEXP, SEXP controlSEXP, SEXP SSB0cSEXP, SEXP SRRfunSEXP, SEXP SRRparsSEXP, SEXP plusgroupSEXP, SEXP spawn_time_fracSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nareas(nareasSEXP);
    Rcpp::traits::input_parameter< double >::type maxage(maxageSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Ncurr(NcurrSEXP);
    Rcpp::traits::input_parameter< double >::type pyears(pyearsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type M_age(M_ageSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Asize_c(Asize_cSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type MatAge(MatAgeSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type WtAge(WtAgeSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type FecAge(FecAgeSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Vuln(VulnSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Retc(RetcSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Prec(PrecSEXP);
    Rcpp::traits::input_parameter< List >::type movc(movcSEXP);
    Rcpp::traits::input_parameter< double >::type SRrelc(SRrelcSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Effind(EffindSEXP);
    Rcpp::traits::input_parameter< double >::type Spat_targc(Spat_targcSEXP);
    Rcpp::traits::input_parameter< double >::type hc(hcSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type R0c(R0cSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type SSBpRc(SSBpRcSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type aRc(aRcSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bRc(bRcSEXP);
    Rcpp::traits::input_parameter< double >::type Qc(QcSEXP);
    Rcpp::traits::input_parameter< double >::type Fapic(FapicSEXP);
    Rcpp::traits::input_parameter< double >::type maxF(maxFSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type MPA(MPASEXP);
    Rcpp::traits::input_parameter< int >::type control(controlSEXP);
    Rcpp::traits::input_parameter< double >::type SSB0c(SSB0cSEXP);
    Rcpp::traits::input_parameter< Function >::type SRRfun(SRRfunSEXP);
    Rcpp::traits::input_parameter< List >::type SRRpars(SRRparsSEXP);
    Rcpp::traits::input_parameter< int >::type plusgroup(plusgroupSEXP);
    Rcpp::traits::input_parameter< double >::type spawn_time_frac(spawn_time_fracSEXP);
    rcpp_result_gen = Rcpp::wrap(popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize_c, MatAge, WtAge, FecAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc, R0c, SSBpRc, aRc, bRc, Qc, Fapic, maxF, MPA, control, SSB0c, SRRfun, SRRpars, plusgroup, spawn_time_frac));
    return rcpp_result_gen;
END_RCPP
}
