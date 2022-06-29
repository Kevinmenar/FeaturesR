library(Rcatch22)

catch22_function <- function(serie, list_size, i) {
  catch22_df <- data.frame(DN_HistogramMode_5=double(), DN_HistogramMode_10=double(), CO_f1ecac=double(),
                         CO_FirstMin_ac=double(), CO_HistogramAMI_even_2_5=double(), CO_trev_1_num=double(),
                         MD_hrv_classic_pnn40=double(), SB_BinaryStats_mean_longstretch1=double(), SB_TransitionMatrix_3ac_sumdiagcov=double(),
                         PD_PeriodicityWang_th0_01=double(), CO_Embed2_Dist_tau_d_expfit_meandiff=double(), IN_AutoMutualInfoStats_40_gaussian_fmmi=double(),
                         FC_LocalSimple_mean1_tauresrat=double(), DN_OutlierInclude_p_001_mdrmd=double(), DN_OutlierInclude_n_001_mdrmd=double(),
                         SP_Summaries_welch_rect_area_5_1=double(), SB_BinaryStats_diff_longstretch0=double(), SB_MotifThree_quantile_hh=double(),
                         SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1=double(), SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1=double(), SP_Summaries_welch_rect_centroid=double(),
                         FC_LocalSimple_mean3_stderr=double())
  

  serie_out <- catch22_all(serie)
  serie_out <- serie_out[,2]

  serie_out <- data.frame(DN_HistogramMode_5=c(serie[1]), DN_HistogramMode_10=c(serie[2]), CO_f1ecac=c(serie[3]),
                      CO_FirstMin_ac=c(serie[4]), CO_HistogramAMI_even_2_5=c(serie[5]), CO_trev_1_num=c(serie[6]),
                      MD_hrv_classic_pnn40=c(serie[7]), SB_BinaryStats_mean_longstretch1=c(serie[8]), SB_TransitionMatrix_3ac_sumdiagcov=c(serie[9]),
                      PD_PeriodicityWang_th0_01=c(serie[10]), CO_Embed2_Dist_tau_d_expfit_meandiff=c(serie[11]), IN_AutoMutualInfoStats_40_gaussian_fmmi=c(serie[12]),
                      FC_LocalSimple_mean1_tauresrat=c(serie[13]), DN_OutlierInclude_p_001_mdrmd=c(serie[14]), DN_OutlierInclude_n_001_mdrmd=c(serie[15]),
                      SP_Summaries_welch_rect_area_5_1=c(serie[16]), SB_BinaryStats_diff_longstretch0=c(serie[17]), SB_MotifThree_quantile_hh=c(serie[18]),
                      SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1=c(serie[19]), SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1=c(serie[20]), SP_Summaries_welch_rect_centroid=c(serie[21]),
                      FC_LocalSimple_mean3_stderr=c(serie[22]))

  return (serie_out)
}
