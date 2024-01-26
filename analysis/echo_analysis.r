##### Libraries ####

library(tidyverse)
library(haven)
library(reshape2)
library(openxlsx)
library(graphics)

echo_data<-read_dta("echo_data.dta")


# frequencies overall  ----------------------------------------------------


table(echo_data$lv_dilatation, useNA = "always")
table(echo_data$abnormal_lv_geometry, useNA = "always")
table(echo_data$aortic_dilation, useNA = "always")
table(echo_data$asc_aorta_dilation, useNA = "always")
table(echo_data$mv_disease, useNA = "always")
table(echo_data$av_disease, useNA = "always")
table(echo_data$tv_disease, useNA = "always")
table(echo_data$pv_disease, useNA = "always")
table(echo_data$lv_systolic_dysfunction, useNA = "always")
table(echo_data$la_dilation, useNA = "always")
table(echo_data$rv_dilation, useNA = "always")
table(echo_data$rv_long_dysf, useNA = "always")
table(echo_data$rv_rad_dysf, useNA = "always")
table(echo_data$ra_dilation, useNA = "always")
table(echo_data$pericard_effusion, useNA = "always")
table(echo_data$pericard_effusion_importance, useNA = "always")
table(echo_data$pericard_thickening, useNA = "always")
table(echo_data$pericardial_calc, useNA = "always")
table(echo_data$cvp, useNA = "always")
table(echo_data$hiv_test_result, useNA = "always")
table(echo_data$lab_creactive_prot, useNA = "always")
table(echo_data$lab_creactive_prot_nr, useNA = "always")


# lab results overall  ----------------------------------------------------


###crp 
lab_crp<-c(echo_data$lab_creactive_prot_nr)
lab_crp<-lab_crp[!is.na(lab_crp)]
barplot(lab_crp, ylim = c(0,300),
        main = "C reactive protein values in mg/l (all patients)")

###neutr
echo_data$lab_neutrophils_nr
lab_neutr<-c(echo_data$lab_neutrophils_nr)
lab_neutr<-lab_neutr[!is.na(lab_neutr)]
barplot(lab_neutr,ylim = c(0,100),
        main = "Neutrophils (all patients)",
        ylab = "Neutrophils in G/l")

###lymph
echo_data$lab_lymphocytes_nr
lab_lymph<-c(echo_data$lab_lymphocytes_nr)
lab_lymph<-lab_lymph[!is.na(lab_lymph)]
barplot(lab_lymph, ylim = c(0,80),
        main = "Lymphocytes (all patients)",
        ylab = "Lymphocytes in G/l")

###CD4
echo_data$hiv_cd4_mm3_nr
lab_cd4<-c(echo_data$hiv_cd4_mm3_nr)
lab_cd4<-lab_cd4[!is.na(lab_cd4)]
barplot(lab_cd4, ylim = c(0,1000),
        main = "Absolute CD4 T-cell count (all patients)",
        ylab = "CD4 T-cell count in cells/mm^3")


echo_data$hiv_cd4_percentage_nr
lab_cd4_perc<-c(echo_data$hiv_cd4_percentage_nr)
lab_cd4_perc<-lab_cd4_perc[!is.na(lab_cd4_perc)]
barplot(lab_cd4_perc,ylim = c(0,50),
        main = "Relative CD4 T-cell count (all patients)",
        ylab = "CD4 T-cell count in %")

###eosinophiles 
echo_data$lab_eosinophils_nr
lab_eos<-c(echo_data$lab_eosinophils_nr)
lab_eos<-lab_eos[!is.na(lab_eos)]
barplot(lab_eos,
        main = "Eosinophils (all patients)",
        ylab = "Eosinophils in G/l")

###hb
echo_data$lab_hemoglobin_nr
lab_hb<-c(echo_data$lab_hemoglobin_nr)
lab_hb<-lab_hb[!is.na(lab_hb)]
barplot(lab_hb, ylim = c(0,20),
        main = "Haemoglobin values (all patients)", 
        ylab="Haemoglobin in g/dl")

###platelets
echo_data$lab_platelets_nr
lab_platelets<-c(echo_data$lab_platelets_nr)
lab_platelets<-lab_platelets[!is.na(lab_platelets)]
barplot(lab_platelets,ylim = c(0,1000), 
        main = "Blood platelet values (all patients)", 
        ylab = "Platelets in G/l")


# test results overall  ---------------------------------------------------


###bp 
echo_data$sbp
syst_bp<-c(echo_data$sbp)
syst_bp<-syst_bp[!is.na(syst_bp)]
barplot(syst_bp, ylim = c(0, 200),
        main = "Systolic blood pressure in mmHg (all patients)")

echo_data$dbp
diast_bp<-c(echo_data$dbp)
diast_bp<-diast_bp[!is.na(diast_bp)]
barplot(diast_bp, ylim = c(0,140),
        main = "Diastolic blood pressure in mmHg (all patients)")

###walking test 
echo_data$smwt_distance_nr
wt<-c(echo_data$smwt_distance_nr)
wt<-wt[!is.na(wt)]
barplot(wt, ylim = c(0,700),
        main = "6 minute walk test distance (all patients)",
        ylab = "Distance in m")



# frequencies hiv + -------------------------------------------------------


table(echo_data$lv_dilatation[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$abnormal_lv_geometry[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$aortic_dilation[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$asc_aorta_dilation[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$mv_disease[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$av_disease[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$tv_disease[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$pv_disease[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$lv_systolic_dysfunction[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$la_dilation[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$rv_dilation[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$rv_long_dysf[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$rv_rad_dysf[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$ra_dilation[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$pericard_effusion[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$pericard_effusion_importance[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$pericard_thickening[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$pericardial_calc[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$cvp[echo_data$hiv_test_result==1], useNA = "always")
table(echo_data$hiv_test_result[echo_data$hiv_test_result==1], useNA = "always")


# frequencies hiv - -------------------------------------------------------


table(echo_data$lv_dilatation[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$abnormal_lv_geometry[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$aortic_dilation[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$asc_aorta_dilation[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$av_disease[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$mv_disease[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$tv_disease[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$pv_disease[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$lv_systolic_dysfunction[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$la_dilation[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$rv_dilation[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$rv_long_dysf[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$rv_rad_dysf[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$ra_dilation[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$pericard_effusion[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$pericard_effusion_importance[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$pericard_thickening[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$pericardial_calc[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$cvp[echo_data$hiv_test_result==0], useNA = "always")
table(echo_data$hiv_test_result[echo_data$hiv_test_result==0], useNA = "always")


# lab values hiv + --------------------------------------------------------


###crp 
lab_crp_hivp<-c(echo_data$lab_creactive_prot_nr[echo_data$hiv_test_result==1])
lab_crp_hivp<-lab_crp_hivp[!is.na(lab_crp_hivp)]
barplot(lab_crp_hivp,ylim = c(0,300),
        main = "C reactive protein values in mg/l (HIV +)")

lab_crp_hivn<-c(echo_data$lab_creactive_prot_nr[echo_data$hiv_test_result==0])
lab_crp_hivn<-lab_crp_hivn[!is.na(lab_crp_hivn)]
barplot(lab_crp_hivn, ylim = c(0,300),
        main = "C reactive protein values in mg/l (HIV -)")

###neutr
echo_data$lab_neutrophils_nr
lab_neutr_hivp<-c(echo_data$lab_neutrophils_nr[echo_data$hiv_test_result==1])
lab_neutr_hivp<-lab_neutr_hivp[!is.na(lab_neutr_hivp)]
barplot(lab_neutr_hivp,ylim = c(0,100),
        main = "Neutrophils (HIV +)",
        ylab = "Neutrophils in G/l")

###lymph
echo_data$lab_lymphocytes_nr
lab_lymph_hivp<-c(echo_data$lab_lymphocytes_nr[echo_data$hiv_test_result==1])
lab_lymph_hivp<-lab_lymph_hivp[!is.na(lab_lymph_hivp)]
barplot(lab_lymph_hivp, ylim = c(0,80),
        main = "Lymphocytes (HIV +)",
        ylab = "Lymphocytes in G/l")

###CD4
echo_data$hiv_cd4_mm3_nr
lab_cd4_hivp<-c(echo_data$hiv_cd4_mm3_nr[echo_data$hiv_test_result==1])
lab_cd4_hivp<-lab_cd4_hivp[!is.na(lab_cd4_hivp)]
barplot(lab_cd4_hivp, ylim = c(0,1000),
        main = "Absolute CD4 T-cell count (HIV +)",
        ylab = "CD4 T-cell count in cells/mm^3")


echo_data$hiv_cd4_percentage_nr
lab_cd4_perc_hivp<-c(echo_data$hiv_cd4_percentage_nr[echo_data$hiv_test_result==1])
lab_cd4_perc_hivp<-lab_cd4_perc_hivp[!is.na(lab_cd4_perc_hivp)]
barplot(lab_cd4_perc_hivp,ylim = c(0,50),
        main = "Relative CD4 T-cell count (HIV +)",
        ylab = "CD4 T-cell count in %")

###eosinophiles 
echo_data$lab_eosinophils_nr
lab_eos_hivp<-c(echo_data$lab_eosinophils_nr[echo_data$hiv_test_result==1])
lab_eos_hivp<-lab_eos_hivp[!is.na(lab_eos_hivp)]
barplot(lab_eos_hivp,
        main = "Eosinophils (HIV +)",
        ylab = "Eosinophils in G/l")

###hb
echo_data$lab_hemoglobin_nr
lab_hb_hivp<-c(echo_data$lab_hemoglobin_nr[echo_data$hiv_test_result==1])
lab_hb_hivp<-lab_hb_hivp[!is.na(lab_hb_hivp)]
barplot(lab_hb_hivp, ylim = c(0,20),
        main = "Haemoglobin values (HIV +)", 
        ylab="Haemoglobin in g/dl")

###platelets
echo_data$lab_platelets_nr
lab_platelets_hivp<-c(echo_data$lab_platelets_nr[echo_data$hiv_test_result==1])
lab_platelets_hivp<-lab_platelets_hivp[!is.na(lab_platelets_hivp)]
barplot(lab_platelets_hivp,ylim = c(0,1000), 
        main = "Blood platelet values (HIV +)", 
        ylab = "Platelets in G/l")


# test results hiv + ------------------------------------------------------


###bp 
echo_data$sbp
syst_bp_hivp<-c(echo_data$sbp[echo_data$hiv_test_result==1])
syst_bp_hivp<-syst_bp_hivp[!is.na(syst_bp_hivp)]
barplot(syst_bp_hivp, ylim = c(0, 200),
        main = "Systolic blood pressure in mmHg (HIV +)")

echo_data$dbp
diast_bp_hivp<-c(echo_data$dbp[echo_data$hiv_test_result==1])
diast_bp_hivp<-diast_bp_hivp[!is.na(diast_bp_hivp)]
barplot(diast_bp_hivp, ylim = c(0,140),
        main = "Diastolic blood pressure in mmHg (HIV +)")

###walking test 
echo_data$smwt_distance_nr
wt_hivp<-c(echo_data$smwt_distance_nr[echo_data$hiv_test_result==1])
wt_hivp<-wt_hivp[!is.na(wt_hivp)]
barplot(wt_hivp, ylim = c(0,700),
        main = "6 minute walk test distance (HIV +)",
        ylab = "Distance in m")


# lab values hiv +/- (in descending order) ------------------------------------------------------


###crp
data_lab_crp<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$lab_creactive_prot_nr))
lab_crp_new<-data_lab_crp%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_crp_new$V2==0, "#4DAF4A", "#E41A1C")

lab_crp<-c(lab_crp_new$V3)
lab_crp<-lab_crp[!is.na(lab_crp)]
barplot(lab_crp, ylim = c(0,300), col = col, border = col,space = 0.3,
        main = "C reactive protein values in mg/l")
legend("topright", legend=c("HIV +", "HIV-"),  fill = c("#E41A1C", "#4DAF4A"))

###neutr
data_lab_neutr<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$lab_neutrophils_nr))
lab_neutr_new<-data_lab_neutr%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_neutr_new$V2==0, "#4DAF4A", "#E41A1C")

lab_neutr<-c(lab_neutr_new$V3)
lab_neutr<-lab_neutr[!is.na(lab_neutr)]
barplot(lab_neutr, ylim = c(0,80), col=col, border=col, space = 0.4,
        main = "Neutrophils in G/l")

###lymph
data_lab_lymph<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$lab_lymphocytes_nr))
lab_lymph_new<-data_lab_lymph%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_lymph_new$V2==0, "#4DAF4A", "#E41A1C")

lab_lymph<-c(lab_lymph_new$V3)
lab_lymph<-lab_lymph[!is.na(lab_lymph)]
barplot(lab_lymph, ylim = c(0,80), col=col, border=col, space=0.5,
        main = "Lymphocytes in G/l")
       

###eosinophiles 
data_lab_eos<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$lab_eosinophils_nr))
lab_eos_new<-data_lab_eos%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_eos_new$V2==0, "#4DAF4A", "#E41A1C")

lab_eos<-c(lab_eos_new$V3)
lab_eos<-lab_eos[!is.na(lab_eos)]
barplot(lab_eos, ylim = c(0,6), col=col, border = col, space = 0.5,
        main = "Eosinophils in G/l")

###CD4
data_lab_cd4<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$hiv_cd4_mm3_nr))
lab_cd4_new<-data_lab_cd4%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_cd4_new$V2==0,"#4DAF4A", "#E41A1C")

lab_cd4<-c(lab_cd4_new$V3)
lab_cd4<-lab_cd4[!is.na(lab_cd4)]
barplot(lab_cd4, ylim = c(0,1000), col=col, border=col, space=0.5,
        main = "CD4 T-cell count in cells/mm^3")

data_lab_cd4_perc<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$hiv_cd4_percentage_nr))
lab_cd4_perc_new<-data_lab_cd4_perc%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

lab_cd4_perc<-c(lab_cd4_perc_new$V3)
lab_cd4_perc<-lab_cd4_perc[!is.na(lab_cd4_perc)]
barplot(lab_cd4_perc,ylim = c(0,50),col=col, border=col, space = 0.5,
        main = "Relative CD4 T-cell count in %")


###hb
data_lab_hb<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$lab_hemoglobin_nr))
lab_hb_new<-data_lab_hb%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_hb_new$V2==0,"#4DAF4A", "#E41A1C")

lab_hb<-c(lab_hb_new$V3)
lab_hb<-lab_hb[!is.na(lab_hb)]
barplot(lab_hb, ylim = c(0,20),col=col, border=col, space = 0.5,
        main = "Haemoglobin in g/dl")

###platelets
data_lab_platelets<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$lab_platelets_nr))
lab_platelets_new<-data_lab_platelets%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(lab_platelets_new$V2==0,"#4DAF4A", "#E41A1C")

lab_platelets<-c(lab_platelets_new$V3)
lab_platelets<-lab_platelets[!is.na(lab_platelets)]
barplot(lab_platelets,ylim = c(0,1000), col=col, border=col, space = 0.5,
        main = "Blood platelets in G/l") 


# test results hiv +/- (in descending order) ----------------------------------------------------

###bp 
data_sbp<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$sbp))
sbp_new<-data_sbp%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(sbp_new$V2==0,"#4DAF4A", "#E41A1C")

sbp<-c(sbp_new$V3)
sbp<-sbp[!is.na(sbp)]
barplot(sbp, ylim = c(0, 200),col=col, border=col, space = 0.5,
        main = "Systolic blood pressure in mmHg")



data_dbp<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$dbp))
dbp_new<-data_dbp%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(dbp_new$V2==0,"#4DAF4A", "#E41A1C")

dbp<-c(dbp_new$V3)
dbp<-dbp[!is.na(dbp)]
barplot(dbp, ylim = c(0,140),col=col, border=col, space = 0.5,
        main = "Diastolic blood pressure in mmHg")

###walking test 
data_wt<-as.data.frame(cbind(echo_data$record_id, echo_data$hiv_test_result, echo_data$smwt_distance_nr))
wt_new<-data_wt%>%
  arrange(desc(V3))%>%
  select(V1,V2,V3)

col <- ifelse(wt_new$V2==0,"#4DAF4A", "#E41A1C")

wt<-c(wt_new$V3)
wt<-wt[!is.na(wt)]
barplot(wt, ylim = c(0,700),col=col, border=col, space = 0.5,
        main = "6 minute walking test",
        ylab = "Walking distance in m")
   

# mean values  ------------------------------------------------------------

mean(echo_data$sbp, na.rm = TRUE)
mean(echo_data$sbp[echo_data$hiv_test_result==1], na.rm = TRUE)
mean(echo_data$sbp[echo_data$hiv_test_result==0], na.rm = TRUE)


# binary values  ----------------------------------------------------------

hiv<- ifelse(echo_data$hiv_test_result==1, "hivp", "hivn")
hivp<-ifelse(echo_data$hiv_test_result==1 & echo_data$hiv_cd4_mm3_nr <= 350, "hivp2", "hivp1")
hivp
table(hivp)
