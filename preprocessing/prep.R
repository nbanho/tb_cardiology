#### Libraries ####

library(tidyverse)
library(haven)


#### Data ####

prepDF <- read_dta("data-raw/reporting_file_all_2_echo_29122023 1.dta") 

prepDF <- prepDF %>%
  dplyr::select(record_id, redcap_event_name, redcap_data_access_group,
                age_cat25, sex_at_birth, clindiag, drugresistant, highbact, cavity, hiv_test_result, tbd_pat_cat,
                constriction_sign, constriction, pericardial_calc, pericard_effusion, pericard_effusion_importance, pericard_thickening,
                age, bmi, smwt_distance_nr, fa_sittest_yn, fa_sit_to_stand_nr, ce_sys_blpress_nr, ce_dia_blpress_nr, hiv_cd4_mm3_nr, hiv_viral_load, hiv_viral_load_nr, hiv_art_yn, tbd_diagnosis, sm_smoking_yn,
                mh_hypertension_yn, mh_coronheart_yn, mh_heart_failure_yn, mh_diabetes_yn, mh_lung_disease_yn, mh_pulm_hyper_yn, mh_copd_yn, mh_asthma_yn, mh_pulfribrosis_yn,
                lab_creactive_prot, lab_creactive_prot_nr, lab_neutrophils, lab_neutrophils_nr, lab_lymphocytes, lab_lymphocytes_nr, lab_eosinophils, lab_eosinophils_nr, lab_hemoglobin_nr,
                lv_dilatation, abnormal_lv_geometry, type_lv_geometry_anomaly, aortic_dilation, asc_aorta_dilation, 
                av_disease, as, ar, mv_disease, ms, mr, tv_disease, ts, tr,
                lvef_visual, lvef_simpson, lv_systolic_dysfunction,
                la_dilation, rv_dilation, rv_function, tv_dti, fac, rv_long_dysf, rv_rad_dysf, ra_dilation, cvp
                ) %>%
  mutate_all(as.character) %>%
  rename(age_cont = age, 
         age = age_cat25,
         sex = sex_at_birth,
         hiv = hiv_test_result,
         site = redcap_data_access_group,
         event_time = redcap_event_name) %>%
  mutate(record_id = ifelse(record_id == "302072", "352072", record_id),
         record_id = ifelse(record_id == "35255", "352055", record_id),
         site = ifelse(record_id %in% c("352072", "352055"), "global_southafrica", site),
         constriction_sign = ifelse(record_id == "351140" & event_time == "baseline_arm_1", "1", constriction_sign),
         constriction_sign = ifelse(record_id == "352053" & event_time == "baseline_arm_1", "0", constriction_sign),
         constriction = ifelse(record_id == "351140" & event_time == "baseline_arm_1", "1", constriction),
         constriction = ifelse(record_id == "352053" & event_time == "baseline_arm_1", "0", constriction),
         event_time = ifelse(event_time == "baseline_arm_1", "Start treatment", ifelse(event_time == "end_of_treatment_arm_1", "End treatment", "Post treatment (6mo)")),
         site = factor(ifelse(site == "",  NA, ifelse(site == "global_zambia", "Zambia", "South Africa")), levels = c("Zambia", "South Africa")),
         comment = ifelse(is.na(hiv), "HIV value was missing, imputed as negative", ""),
         hiv = ifelse(is.na(hiv), "0", hiv),
         cavity = ifelse(is.na(cavity), "0", cavity),
         across(c(constriction, constriction_sign, pericard_effusion, pericardial_calc, pericard_thickening), ~ ifelse(.x == "0", "No", "Yes")), 
         across(c(constriction, constriction_sign, pericard_effusion, pericard_thickening, pericardial_calc), ~ factor(.x, levels = c("No", "Yes"))),
         across(c(age, clindiag, drugresistant, highbact, cavity), ~ factor(ifelse(.x == "1", "Yes", "No"), levels = c("No", "Yes"))),
         sex = factor(ifelse(sex == "1", "Male", "Female"), levels = c("Male", "Female")),
         hiv = factor(ifelse(hiv == "1", "Positive", "Negative"), levels = c("Negative", "Positive")),
         tbd_pat_cat = ifelse(is.na(tbd_pat_cat), "1", tbd_pat_cat),
         tbd_pat_cat = factor(ifelse(tbd_pat_cat == "1", "New case", "Relapse"), levels = c("New case", "Relapse")),
         pericard_effusion_importance = factor(ifelse(pericard_effusion_importance == "1", "Mild", 
                                                      ifelse(pericard_effusion_importance == "2", "Moderate", "Large")),
                                               levels = c("Mild", "Moderate", "Large")),
         across(c(age_cont, bmi, smwt_distance_nr, fa_sit_to_stand_nr, hiv_cd4_mm3_nr, hiv_viral_load_nr, ce_sys_blpress_nr, ce_dia_blpress_nr,
                  lvef_visual, lvef_simpson, 
                  rv_function, rv_function, tv_dti, fac, cvp), 
                as.numeric),
         obesity = ifelse(bmi > 30, "Yes", "No"),
         fa_sittest_nr = ifelse(fa_sittest_yn == "0", 0, ifelse(fa_sittest_yn == "1", fa_sit_to_stand_nr, NA)),
         hiv_viral_load_nr = ifelse(hiv_viral_load == "1", hiv_viral_load_nr, NA),
         hiv_viral_load = factor(ifelse(hiv_viral_load == "0", "Undetectable", ifelse(hiv_viral_load == "1", "Detectable", NA)), levels = c("Undetectable", "Detectable")),
         hiv_art_yn = factor(ifelse(hiv == "Positive", ifelse(is.na(hiv_art_yn), "Unknown", ifelse(hiv_art_yn == "1", "Yes", "No")), NA), levels = c("Yes", "No", "Unknown")),
         tbd_diagnosis = factor(ifelse(is.na(tbd_diagnosis), "Unknown", ifelse(tbd_diagnosis == "1", "Pulmonary", "Pulmonary and extrapulmonary")), levels = c("Pulmonary", "Pulmonary and extrapulmonary", "Unknown")),
         across(c(obesity, sm_smoking_yn), ~ factor(ifelse(is.na(.x), "Unknown", ifelse(.x == "1", "Yes", "No")), levels = c("Yes", "No", "Unknown"))),
         mh_coronheart_yn = ifelse(mh_coronheart_yn == "2", "0", ifelse(mh_coronheart_yn == "1", "1", mh_coronheart_yn)),
         across(c(mh_hypertension_yn, mh_coronheart_yn, mh_heart_failure_yn, mh_diabetes_yn,
                  mh_lung_disease_yn, mh_pulm_hyper_yn, mh_copd_yn, mh_asthma_yn, mh_pulfribrosis_yn), ~ factor(ifelse(.x == "1", "Yes", 
                                                                                                                       ifelse(.x == "0", "No", 
                                                                                                                              ifelse(.x == "99", "Unknown", .x))),
                                                                                                                levels = c("Yes", "No", "Unknown"))),
         lab_creactive_prot_nr = ifelse(lab_creactive_prot == "1", lab_creactive_prot_nr, NA),
         lab_neutrophils_nr = ifelse(lab_neutrophils == "3", lab_neutrophils_nr, NA),
         lab_lymphocytes_nr = ifelse(lab_lymphocytes == "3", lab_lymphocytes_nr, NA),
         lab_eosinophils_nr = ifelse(lab_eosinophils == "3", lab_eosinophils_nr, NA),
         across(c(lab_creactive_prot_nr, lab_neutrophils_nr, lab_lymphocytes_nr, lab_eosinophils_nr, lab_hemoglobin_nr), as.numeric),
         across(c(lv_dilatation, abnormal_lv_geometry, aortic_dilation, asc_aorta_dilation, 
                  av_disease, as, ar, mv_disease, ms, mr, tv_disease, ts, tr,
                  lv_systolic_dysfunction,
                  la_dilation, rv_dilation, rv_long_dysf, rv_rad_dysf, ra_dilation), 
                ~ factor(ifelse(.x == "1", "Yes", "No"), levels = c("Yes", "No"))),
         type_lv_geometry_anomaly = factor(ifelse(type_lv_geometry_anomaly == "1", "concentric remodelling", ifelse(type_lv_geometry_anomaly == "2", "concentric hypertrophy", "eccentric hypertrophy")),
                                           levels = c("concentric remodelling", "concentric hypertrophy", "eccentric hypertrophy"))
  ) %>%
  dplyr::select(-lab_creactive_prot, -lab_neutrophils, -lab_lymphocytes, -lab_eosinophils)

# 419 out of 426 have at least one outcome

prepDF_filt <- prepDF %>%
  rowwise() %>%
  filter(!is.na(constriction) | !is.na(constriction_sign) | !is.na(pericard_effusion) 
         | !is.na(pericard_effusion_importance) | !is.na(pericard_thickening))

saveRDS(prepDF_filt, file = "data-clean/echo.rds")
