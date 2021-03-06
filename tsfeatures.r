tsFeatures_generator <- function (serie) {

      tsfeatures_df <- data.frame(hurts_tsf=double(), max_level_shift_tsf=double(),
                            max_var_shift_tsf=double(), max_kl_shift_tsf=double(),
                            alpha_tsf=double(), beta_tsf=double(), gamma_tsf=double(), 
                            firstmin_ac_tsf=double(), firstzero_ac_tsf=double(),
                            std1st_der_tsf=double(), spreadrandomlocal_meantaul_tsf=double(),
                            fluctanal_prop_r1_tsf=double(), embed2_incircle_1_tsf=double(), embed2_incircle_2_tsf=double(),
                            ac_9_tsf=double(), firstmin_ac_tsf=double(), trev_num_tsf=double(), motiftwo_entro3_tsf=double(),
                            walker_propcross_tsf=double(), std1st_der_tsf=double(), histogram_mode_tsf=double(),
                            outlierinclude_mdrmd_tsf=double(), firstzero_ac_tsf=double(),
                            heterogeneity_arch_acf_tsf=double(), heterogeneity_garch_acf_tsf=double(), 
                            heterogeneity_arch_r2_tsf=double(), heterogeneity_garch_r2_tsf=double(), 
                            stability_tsf=double(), lumpiness_tsf=double(), crossing_points_tsf=double(),
                            flat_spots_tsf=double(), nonlinearity_tsf=double(), unitroot_kpss_tsf=double(),
                            unitroot_pp_tsf=double(), arch_stat_tsf=double(), acf_features_x_acf1=double(),
                            acf_features_x_acf10=double(),acf_features_seas_acf1=double(),
                            acf_features_diff2x_pacf5=double(),
                            acf_features_x_pacf5=double(), stl_features_nperiods=double(),
                            stl_features_seasonal_period=double(), stl_features_trend=double(),
                            stl_features_spike=double(), stl_features_linearity=double(),
                            stl_features_curvature=double(), stl_features_e_acf1=double(),
                            stl_features_e_acf10=double(), stl_features_seasonal_strength=double(),
                            stl_features_peak=double(), stl_features_trough=double(),
                            stl_features_trough=double())

      hurts_tsf <- hurst(serie)
      max_level_shift_tsf <- max_level_shift(serie)
      max_var_shift_tsf <- max_var_shift(serie)
      max_kl_shift_tsf <- max_kl_shift(serie)
      firstmin_ac_tsf <- firstmin_ac(serie)
      firstzero_ac_tsf <- firstzero_ac(serie)
      std1st_der_tsf <- std1st_der(serie)
      spreadrandomlocal_meantaul_tsf <- spreadrandomlocal_meantaul(serie, l=12)
      fluctanal_prop_r1_tsf <- fluctanal_prop_r1(serie)
      hw_parameters_tsf <- hw_parameters(serie)
      embed2_incircle_1_tsf <- embed2_incircle(serie, boundary = 1)
      embed2_incircle_2_tsf <- embed2_incircle(serie, boundary = 2)
      ac_9_tsf <- ac_9(serie)
      firstmin_ac_tsf <- firstmin_ac(serie)
      trev_num_tsf <- trev_num(serie)
      motiftwo_entro3_tsf <- motiftwo_entro3(serie)
      walker_propcross_tsf <- walker_propcross(serie)
      std1st_der_tsf <- std1st_der(serie)
      histogram_mode_tsf <- histogram_mode(serie)
      outlierinclude_mdrmd_tsf <- outlierinclude_mdrmd(serie)
      firstzero_ac_tsf <- firstzero_ac(serie)
      stability_tsf <- stability(serie)
      lumpiness_tsf <- lumpiness(serie)
      crossing_points_tsf <- crossing_points(serie)
      flat_spots_tsf <- flat_spots(serie)
      nonlinearity_tsf <- nonlinearity(serie)
      unitroot_kpss_tsf <- unitroot_kpss(serie)
      unitroot_pp_tsf <- unitroot_pp(serie)
      arch_stat_tsf <- arch_stat(serie)
      acf_features_x_acf1 <- acf_features(serie)["x_acf1"]
      acf_features_x_acf10 <- acf_features(serie)["x_acf10"]
      acf_features_seas_acf1 <- acf_features(serie)["seas_acf1"]
      acf_features_diff2x_pacf5 <- pacf_features(serie)["diff2x_pacf5"]
      acf_features_x_pacf5 <- pacf_features(serie)["x_pacf5"]
      stl_features_temp <- stl_features(serie)
      stl_features_nperiods <- stl_features_temp["nperiods"]
      stl_features_seasonal_period <- stl_features_temp["seasonal_period"]
      stl_features_trend <- stl_features_temp["trend"]
      stl_features_spike <- stl_features_temp["spike"]
      stl_features_linearity <- stl_features_temp["linearity"]
      stl_features_curvature <- stl_features_temp["curvature"]
      stl_features_e_acf1 <- stl_features_temp["e_acf1"]
      stl_features_e_acf10 <- stl_features_temp["e_acf10"]
      stl_features_seasonal_strength <- stl_features_temp["seasonal_strength"]
      stl_features_peak <- stl_features_temp["peak"]
      stl_features_trough <- stl_features_temp["trough"]
      
      
      serie <- data.frame(hurts_tsf=hurts_tsf, max_level_shift_tsf=getElement(max_level_shift_tsf, "max_level_shift"),
                          max_var_shift_tsf=getElement(max_var_shift_tsf, "max_var_shift"),
                          max_kl_shift_tsf=getElement(max_kl_shift_tsf, "max_kl_shift"),
                          alpha_tsf=getElement(hw_parameters_tsf, "alpha"), beta_tsf=getElement(hw_parameters_tsf, "beta"),
                          gamma_tsf=getElement(hw_parameters_tsf, "gamma"), 
                          firstmin_ac_tsf=firstmin_ac_tsf, firstzero_ac_tsf=firstzero_ac_tsf,
                          std1st_der_tsf=std1st_der_tsf, spreadrandomlocal_meantaul_tsf=spreadrandomlocal_meantaul_tsf,
                          fluctanal_prop_r1_tsf=fluctanal_prop_r1_tsf, embed2_incircle_1_tsf=embed2_incircle_1_tsf,
                          embed2_incircle_2_tsf=embed2_incircle_2_tsf, ac_9_tsf=ac_9_tsf, firstmin_ac_tsf=firstmin_ac_tsf,
                          trev_num_tsf=trev_num_tsf, motiftwo_entro3_tsf=motiftwo_entro3_tsf, walker_propcross_tsf=walker_propcross_tsf,
                          std1st_der_tsf=std1st_der_tsf, histogram_mode_tsf=histogram_mode_tsf, outlierinclude_mdrmd_tsf=outlierinclude_mdrmd_tsf,
                          outlierinclude_mdrmd_tsf=outlierinclude_mdrmd_tsf, firstzero_ac_tsf=firstzero_ac_tsf, 
                          stability_tsf=stability_tsf, lumpiness_tsf=lumpiness_tsf, crossing_points_tsf=crossing_points_tsf,
                          flat_spots_tsf=flat_spots_tsf, nonlinearity_tsf=nonlinearity_tsf, unitroot_kpss_tsf=unitroot_kpss_tsf,
                          unitroot_pp_tsf=unitroot_pp_tsf, arch_stat_tsf=arch_stat_tsf, acf_features_x_acf1=acf_features_x_acf1,
                          acf_features_x_acf10=acf_features_x_acf10, acf_features_seas_acf1=acf_features_seas_acf1,
                          acf_features_diff2x_pacf5=acf_features_diff2x_pacf5, acf_features_x_pacf5=acf_features_x_pacf5,
                          stl_features_nperiods=stl_features_nperiods, stl_features_seasonal_period=stl_features_seasonal_period,
                          stl_features_trend=stl_features_trend, stl_features_spike=stl_features_spike, 
                          stl_features_linearity=stl_features_linearity, stl_features_curvature=stl_features_curvature,
                          stl_features_e_acf1=stl_features_e_acf1, stl_features_e_acf10=stl_features_e_acf10,
                          stl_features_seasonal_strength=stl_features_seasonal_strength,
                          stl_features_peak=stl_features_peak, stl_features_trough=stl_features_trough)
      
      tsfeatures_df <- rbind(tsfeatures_df, serie)

      return(tsfeatures_df)
    }


#tsFeatures_generator(series_file_list)