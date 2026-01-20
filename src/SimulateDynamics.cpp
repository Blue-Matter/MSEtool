
  //   // Calculate Recruitment and Numbers at beginning of next time step
  //   for (int st=0; st<nStock; st++) {
  //
      // Determine Age at Recruitment
  //     S4 Stock = StockList[st];
  //     S4 Ages = Stock.slot("Ages");
  //     double Seasons = Stock.slot("Seasons");
  //     int AgeRec = CalcRecruitment_TimeStep_(Ages, 1/Seasons);
  //     int TSRec = TSindex + AgeRec; // TSindex + 1 for age-1 recruitment
  // 
  //     arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
  //     int nAge = NumberAtAgeArea.n_rows;
  //     int nTSnumber = NumberAtAgeArea.n_cols;
  //     int nArea = NumberAtAgeArea.n_slices;
  // 
  //     S4 Spatial = Stock.slot("Spatial");
  // 
  // //     // Calc recruitment if there is enough space in NumberAtAgeArea
  //     if (TSRec<nTSnumber) {
  //       if (debug) {
  //         Rcout << "\n\nCalculate Recruitment and Numbers for Stock " << st << std::endl;
  //         Rcout << "TSindex " << TSindex << std::endl;
  //         Rcout << "Seasons f" << Seasons << std::endl;
  //         Rcout << "AgeRec " << AgeRec << std::endl;
  //         Rcout << "TSRec " << TSRec << std::endl;
  // 
  //       }
  // 
  //       S4 SRR = Stock.slot("SRR");
  //       arma::vec R0 = SRR.slot("R0");
  //       arma::vec RecDevHist = SRR.slot("RecDevHist");
  //       arma::vec RecDevProj = SRR.slot("RecDevProj");
  //       arma::vec RecDevs = join_cols(RecDevHist, RecDevProj);
  // 
  //       Function SRRModel = SRR.slot("Model");
  //       List SRRPars = SRR.slot("Pars");
  // 
  //       // Calculate Recruitment
  //       // Uses aggregate SProduction - ie summed over areas
  //       // TODO option to use time-varying alpha, beta
  // 
  //       // Equilibrium unfished spawning production
  // 
  //       int sp0_nts = SP0.n_cols;
  //       double sp0 = arma::as_scalar(SP0.row(st).col(0));
  //       if (sp0_nts >1 ) {
  //         sp0 = arma::as_scalar(SP0.row(st).col(TSindex));
  //       }
  // 
  //       double SProductionThisTimeStep = arma::as_scalar(SProduction.row(st).col(TSindex));
  //       double R0_recruit_TimeStep = arma::as_scalar(R0(TSRec));
  //       double RecDev_recruit_TimeStep = arma::as_scalar(RecDevs(TSRec));
  //       if (debug) {
  //         Rcout << "sp0 = " << sp0 << std::endl;
  //         Rcout << "SProductionThisTimeStep = " << SProductionThisTimeStep << std::endl;
  //         Rcout << "R0_recruit_TimeStep = " << R0_recruit_TimeStep << std::endl;
  //         Rcout << "RecDev_recruit_TimeStep = " << RecDev_recruit_TimeStep << std::endl;
  //       }
  // 
  //       double Recruits = CalcRecruitment_(SProductionThisTimeStep,
  //                                          R0_recruit_TimeStep,
  //                                          sp0,
  //                                          RecDev_recruit_TimeStep,
  //                                          SRRModel,
  //                                          SRRPars,
  //                                          TSindex);
  // //       if (debug) {
  //         Rcout << "Recruits =  " << Recruits << std::endl;
  //       }
  //
  //       // Distribute Recruits
  //       if (debug)
  //         Rcout << "Distribute Recruits " << std::endl;
  //
  //
  //       arma::cube UnfishedDist = Spatial.slot("UnfishedDist"); // nArea, nAge, nTS;
  //       arma::vec recruitArea(nArea);
  //
  //       for (int area=0; area<nArea; area++) {
  //         double rec = Recruits * arma::as_scalar(UnfishedDist(arma::span(area), arma::span(0), arma::span(TSRec)));
  //         if (rec < 1E-6)
  //           rec = 1E-6;
  //         recruitArea(area) = rec;
  //
  //         if (debug)
  //           Rcout << "Recruits in Area " << area << ": " << rec << std::endl;
  //
  //       }
  //       NumberAtAgeArea.subcube(0, TSRec, 0, 0, TSRec, nArea-1) = recruitArea;
  //     }
  //
  //     if (TSindex <(nTSnumber-1)) {
  //
  //       // Rcout << "timestep = " << timestep << std::endl;
  //       // Rcout << "TSindex = " << TSindex << std::endl;
  //       // Rcout << "nTSnumber = " << nTSnumber << std::endl;
  //       bool plusgroup = Ages.slot("PlusGroup");
  //
  //       List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
  //
  //       S4 NaturalMortality = Stock.slot("NaturalMortality");
  //       arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge");
  //
  //       S4 Maturity = Stock.slot("Maturity");
  //       arma::mat Semelparous = Maturity.slot("Semelparous");
  //
  //       if (debug)
  //         Rcout << "NumberAtAgeArea Next"  << std::endl;
  //
  //       NumberAtAgeArea.col(TSindex+1) = CalcNumberNext_(
  //         NumberAtAgeArea.col(TSindex),
  //         NumberAtAgeArea.col(TSindex+1),
  //         Semelparous.col(TSindex),
  //         FDeadAtAgeAreaStock[TSindex],
  //         NaturalMortalityAtAge.col(TSindex),
  //         plusgroup,
  //         nAge,
  //         nArea);
  //
  //       // Move Population at beginning of next Time Step
  //       if (debug)
  //         Rcout << "Movement"  << std::endl;
  //
  //       List MovementList = Spatial.slot("Movement");
  //       NumberAtAgeArea = CalcStockMovement_(NumberAtAgeArea,
  //                                            MovementList[TSindex+1],
  //                                                        nAge,
  //                                                        nArea,
  //                                                        TSindex+1);
  //     }
  //     NumberAtAgeAreaList[st] = NumberAtAgeArea;
  //
  //
  //     // Calculate Total Biomass
  //     if (debug)
  //       Rcout << "Calculating Total Biomass" << std::endl;
  //
  //     S4 Weight = Stock.slot("Weight");
  //     arma::mat WeightAtAge = Weight.slot("MeanAtAge");
  //
  //     arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.subcube(arma::span(0, nAge-1), arma::span(TSindex), arma::span(0, nArea-1));
  //     Biomass.row(st).col(TSindex) = CalcBiomass_(NumberAtAgeAreaThisTS, WeightAtAge.col(TSindex));
  //
  //     if (debug) {
  //       double BIOMASS = arma::as_scalar(Biomass.row(st).col(TSindex));
  //       Rcout << "Total Biomass = " << BIOMASS << std::endl;
  //     }
  //
  //   } // end of Stock loop
  //
  // } // end of Time Step loop
  //
  // HistSim.slot("Number") = NumberAtAgeAreaList;
  // HistSim.slot("Biomass") = Biomass;
  // HistSim.slot("SBiomass") = SBiomass;
  // HistSim.slot("SProduction") = SProduction;
  // HistSim.slot("Distribution") = DistributionList;
  // HistSim.slot("Effort") = EffortCube;
  // HistSim.slot("FDeadArea") = FDeadAtAgeAreaList;
  // HistSim.slot("FRetainArea") = FRetainAtAgeAreaList;
  //
  // // CalcCatch and overall F
  // if (CalcCatch>0) {
  //   if (debug) {
  //     Rcout << "*********************"  << std::endl;
  //     Rcout << "CalcCatch_ " << std::endl;
  //     Rcout << "*********************"  << std::endl;
  //   }
  //
  //   HistSim = CalcCatch_(HistSim, Years, debug);
  //
  //   if (debug) {
  //     Rcout << "*********************"  << std::endl;
  //     Rcout << "Done CalcCatch_ " << std::endl;
  //     Rcout << "*********************"  << std::endl;
  //   }
  //
  //   if (debug) {
  //     Rcout << "*********************"  << std::endl;
  //     Rcout << "CalcAggregateF_ " << std::endl;
  //     Rcout << "*********************"  << std::endl;
  //   }
  //   HistSim = CalcAggregateF_(HistSim, Years, debug);
  //   if (debug) {
  //     Rcout << "*********************"  << std::endl;
  //     Rcout << "Done CalcAggregateF_ " << std::endl;
  //     Rcout << "*********************"  << std::endl;
  //   }
  // }
//   return(HistSim);
// }