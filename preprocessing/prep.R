# libraries
library(tidyverse)

# load echo data
echo <- read.csv("data-raw/Z519TBSRNEchoStudy_DATA_2024-02-19_1650.csv") %>%
       dplyr::select(-record_id) %>%
       rename(record_id = record_id_excel_table) %>%
       dplyr::select(
              record_id,
              redcap_event_name,
              bmi,
              constriction_sign,
              constriction,
              pericardial_calc,
              pericard_effusion,
              pericard_effusion_importance,
              pericard_thickening,
              lv_dilatation,
              abnormal_lv_geometry,
              type_lv_geometry_anomaly,
              aortic_dilation,
              asc_aorta_dilation,
              av_disease,
              as,
              ar,
              mv_disease,
              ms,
              mr,
              tv_disease,
              ts,
              tr,
              lvef_visual,
              lvef_simpson,
              lv_systolic_dysfunction,
              diastolic_dysfunction_new,
              typ_diastolic_dysfunction_new,
              lv_dilatation,
              la_dilation,
              rv_dilation,
              rv_function,
              tv_dti,
              fac,
              rv_long_dysf,
              rv_rad_dysf,
              tr_pgmax,
              ra_dilation,
              cvp,
              note,
              mv_e_a,
              mv_dec_time,
              dilated_ivc,
              ivc_low_respiratory_change,
              abnormal_mitral_inflow,
              ivs_shift,
              e_septal,
              e_lateral,
              septal_bounce,
              note
       ) %>%
       rename(event_time = redcap_event_name) %>%
       mutate(
              record_id = ifelse(record_id == 3511, 351116, record_id),
              constriction_sign = ifelse(record_id == 351140 & event_time == "baseline_arm_1", 1, constriction_sign),
              constriction_sign = ifelse(record_id == 352053 & event_time == "baseline_arm_1", 0, constriction_sign),
              constriction = ifelse(record_id == 351140 & event_time == "baseline_arm_1", 1, constriction),
              constriction = ifelse(record_id == 352053 & event_time == "baseline_arm_1", 0, constriction),
              pericard_effusion_importance = factor(
                     ifelse(pericard_effusion_importance == 1, "Mild",
                            ifelse(pericard_effusion_importance == 2, "Moderate", "Large")
                     ),
                     levels = c("Mild", "Moderate", "Large")
              ),
              across(c(constriction, constriction_sign, pericard_effusion, pericardial_calc, pericard_thickening), ~ ifelse(.x == "0", "No", "Yes")),
              across(c(constriction, constriction_sign, pericard_effusion, pericard_thickening, pericardial_calc), ~ factor(.x, levels = c("No", "Yes"))),
              event_time = ifelse(event_time == "baseline_arm_1", "Start", ifelse(event_time == "end_of_treatment_arm_1", "End", "Post")),
              obesity = ifelse(bmi > 30, "Yes", "No"),
              underweight = ifelse(bmi < 18, "Yes", "No"),
              across(c(obesity, underweight), ~ factor(ifelse(is.na(.x), "Unknown", .x), levels = c("Yes", "No", "Unknown"))),
              across(
                     c(
                            lv_dilatation, abnormal_lv_geometry, aortic_dilation, asc_aorta_dilation,
                            av_disease, as, ar, mv_disease, ms, mr, tv_disease, ts, tr,
                            lv_systolic_dysfunction, diastolic_dysfunction_new,
                            lv_dilatation, la_dilation, rv_dilation, rv_long_dysf, rv_rad_dysf, ra_dilation
                     ),
                     ~ factor(ifelse(.x == 1, "Yes", "No"), levels = c("Yes", "No"))
              ),
              type_lv_geometry_anomaly = factor(ifelse(type_lv_geometry_anomaly == 1, "concentric remodelling", ifelse(type_lv_geometry_anomaly == 2, "concentric hypertrophy", "eccentric hypertrophy")),
                     levels = c("concentric remodelling", "concentric hypertrophy", "eccentric hypertrophy")
              ),
              typ_diastolic_dysfunction_new = factor(ifelse(
                     typ_diastolic_dysfunction_new == 1, "Grade I",
                     ifelse(typ_diastolic_dysfunction_new == 2, "Grade II", "Undefined dysfunction")
              ))
       )

# filter missing
echo_filt <- echo %>%
       rowwise() %>%
       filter(!is.na(constriction) | !is.na(constriction_sign) |
              !is.na(pericard_effusion) | !is.na(pericard_thickening))

sprintf("Number of missing records: %s", nrow(echo) - nrow(echo_filt))

# load patient data
pat <- read.csv("data-raw/1734TuberculosisPati_DATA_2024-02-15_1012.csv") %>%
       dplyr::select(
              record_id,
              redcap_data_access_group,
              redcap_event_name,
              age,
              dem_sex,
              xr_cavitation_yn,
              mb_xpert_t1_rifresist,
              mb_drug_rif,
              mb_smear1_result_who,
              ic4, ic5, ic6, ic7,
              hiv_test_result,
              hiv_viral_load,
              hiv_viral_load_nr,
              hiv_cd4_mm3_nr,
              hiv_art_yn,
              tbd_pat_cat,
              tbd_diagnosis,
              ce_sys_blpress_nr,
              ce_dia_blpress_nr,
              fa_sittest_yn,
              fa_sit_to_stand_nr,
              smwt_distance_nr,
              sm_smoking_yn,
              mh_hypertension_yn,
              mh_coronheart_yn,
              mh_heart_failure_yn,
              mh_diabetes_yn,
              mh_lung_disease_yn,
              mh_pulm_hyper_yn,
              mh_copd_yn,
              mh_asthma_yn,
              mh_pulfribrosis_yn,
              lab_creactive_prot,
              lab_creactive_prot_nr,
              lab_neutrophils,
              lab_neutrophils_nr,
              lab_lymphocytes,
              lab_lymphocytes_nr,
              lab_eosinophils,
              lab_eosinophils_nr,
              lab_hemoglobin_nr
       ) %>%
       rename(
              hiv = hiv_test_result,
              sex = dem_sex
       ) %>%
       group_by(record_id) %>%
       filter(grepl("baseline", redcap_event_name)) %>%
       slice(1) %>%
       ungroup() %>%
       mutate(
              drugresistant = ifelse(mb_xpert_t1_rifresist == 1, 1, ifelse(mb_drug_rif == 2, 1, 0)),
              drugresistant = ifelse(is.na(drugresistant), 0, drugresistant),
              highbact = ifelse(mb_smear1_result_who %in% c(3, 4), 1, 0),
              highbact = ifelse(is.na(highbact), 0, highbact),
              cavity = ifelse(xr_cavitation_yn == 1, 1, 0),
              cavity = ifelse(is.na(cavity), 0, cavity),
              clindiag = ifelse(ic4 == 1 | ic5 == 5 | ic6 == 1 | ic7 == 1, 0, 1),
              clindiag = ifelse(is.na(clindiag), 1, clindiag),
              hiv = ifelse(is.na(hiv), 0, hiv),
              hiv = factor(ifelse(hiv == "1", "Positive", "Negative"), levels = c("Negative", "Positive")),
              age_cat25 = ifelse(age < 25, 1, 0),
              across(c(age_cat25, clindiag, drugresistant, highbact, cavity), ~ factor(ifelse(.x == 1, "Yes", "No"), levels = c("No", "Yes"))),
              sex = factor(ifelse(sex == 1, "Male", "Female"), levels = c("Male", "Female")),
              tbd_pat_cat = ifelse(is.na(tbd_pat_cat), 1, tbd_pat_cat),
              tbd_pat_cat = factor(ifelse(tbd_pat_cat == 1, "New case", "Relapse"), levels = c("New case", "Relapse")),
              fa_sittest_nr = ifelse(fa_sittest_yn == 0, 0, ifelse(fa_sittest_yn == 1, fa_sit_to_stand_nr, NA)),
              hiv_viral_load_nr = ifelse(hiv_viral_load == 1, hiv_viral_load_nr, NA),
              hiv_viral_load = factor(ifelse(hiv_viral_load == 0, "Undetectable", ifelse(hiv_viral_load == 1, "Detectable", NA)), levels = c("Undetectable", "Detectable")),
              hiv_art_yn = factor(ifelse(hiv == "Positive", ifelse(is.na(hiv_art_yn), "Unknown", ifelse(hiv_art_yn == 1, "Yes", "No")), NA), levels = c("Yes", "No", "Unknown")),
              tbd_diagnosis = factor(ifelse(is.na(tbd_diagnosis), "Unknown", ifelse(tbd_diagnosis == 1, "Pulmonary", "Pulmonary and extrapulmonary")), levels = c("Pulmonary", "Pulmonary and extrapulmonary", "Unknown")),
              across(c(sm_smoking_yn), ~ factor(ifelse(is.na(.x), "Unknown", ifelse(.x == 1, "Yes", "No")), levels = c("Yes", "No", "Unknown"))),
              mh_coronheart_yn = ifelse(mh_coronheart_yn == 2, 0, ifelse(mh_coronheart_yn == 1, 1, mh_coronheart_yn)),
              across(c(
                     mh_hypertension_yn, mh_coronheart_yn, mh_heart_failure_yn, mh_diabetes_yn,
                     mh_lung_disease_yn, mh_pulm_hyper_yn, mh_copd_yn, mh_asthma_yn, mh_pulfribrosis_yn
              ), ~ factor(
                     ifelse(.x == 1, "Yes",
                            ifelse(.x == 0, "No",
                                   ifelse(.x == 99, "Unknown", .x)
                            )
                     ),
                     levels = c("Yes", "No", "Unknown")
              )),
              lab_creactive_prot_nr = ifelse(lab_creactive_prot == 1, lab_creactive_prot_nr, NA),
              lab_neutrophils_nr = ifelse(lab_neutrophils == 3, lab_neutrophils_nr, NA),
              lab_lymphocytes_nr = ifelse(lab_lymphocytes == 3, lab_lymphocytes_nr, NA),
              lab_eosinophils_nr = ifelse(lab_eosinophils == 3, lab_eosinophils_nr, NA),
              across(c(lab_creactive_prot_nr, lab_neutrophils_nr, lab_lymphocytes_nr, lab_eosinophils_nr, lab_hemoglobin_nr), as.numeric),
       ) %>%
       dplyr::select(
              -ic4, -ic5, -ic6, -ic7,
              -xr_cavitation_yn,
              -mb_xpert_t1_rifresist,
              -mb_drug_rif,
              -mb_smear1_result_who,
              -redcap_event_name
       ) %>%
       mutate(record_id = as.integer(gsub("-", "", record_id)))

# merge
prep_df <- left_join(echo_filt, pat, by = "record_id") %>%
       mutate(
              site = ifelse(grepl("africa", redcap_data_access_group), "South Africa", "Zambia"),
              site = factor(site, levels = c("South Africa", "Zambia"))
       )
sprintf("Missing gender: %i", nrow(filter(prep_df, is.na(sex))))

# add deaths

pat_death <- read.csv("data-raw/1734TuberculosisPati_DATA_2024-02-15_1012.csv") %>%
       dplyr::select(record_id, death_yn) %>%
       mutate(death_yn = ifelse(is.na(death_yn), 0, death_yn)) %>%
       group_by(record_id) %>%
       summarize(death_yn = max(death_yn, na.rm = TRUE)) %>%
       ungroup() %>%
       mutate(record_id = as.integer(gsub("-", "", record_id)))

prep_df <- left_join(prep_df, pat_death, by = "record_id")

sprintf("Number of patients dying records: %s", nrow(filter(prep_df, death_yn == 1)))

# missing baseline
missing_base <- prep_df %>%
       group_by(record_id) %>%
       filter(!any(event_time == "Start")) %>%
       ungroup() %>%
       dplyr::select(
              record_id,
              site,
              event_time,
              constriction,
              constriction_sign,
              pericard_effusion,
              pericard_thickening,
              pericardial_calc
       )

n_distinct(missing_base$record_id)

write.csv(missing_base, "results/missing_baseline.csv", row.names = FALSE)

missing_base_guy <- read.csv("data-raw/base_echo_Guy-Mar-03.csv")

rbind(missing_base, missing_base_guy) %>%
       group_by(record_id) %>%
       filter(n() > 1)

missing_base_guy %>%
       dplyr::select(record_id, site) %>%
       left_join(
              prep_df %>% dplyr::select(record_id, site) %>% mutate(matched = TRUE),
              by = c("record_id", "site")
       ) %>%
       filter(is.na(matched))

# save
saveRDS(prep_df, file = "data-clean/echo.rds")
