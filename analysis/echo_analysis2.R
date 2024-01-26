
# libraries ---------------------------------------------------------------

library(tidyverse)
library(haven)
library(openxlsx)



# import data -------------------------------------------------------------
echo_data<-read_dta("echo_data.dta")

# analysis  ---------------------------------------------------------------

###define subgroups 
#hiv status 
hiv_pn<-ifelse(echo_data$hiv_test_result==1, "hivp", "hivn")
table(hiv_pn)
#hiv subgroups (according to cell count)
hivcd4<-ifelse(echo_data$hiv_cd4_mm3_nr <= 350, "hivp2", ifelse(echo_data$hiv_cd4_mm3_nr > 350, "hivp1", "na"))
table(hivcd4)



# baseline characteristics ------------------------------------------------

##epidemiological data
#sex
table(echo_data$sex_at_birth)
#age
summary(echo_data$age)
#hiv status
table(echo_data$hiv_test_result)
#previous tb treatment 
table(echo_data$tbh_tb_treated_yn)
#drug resistance 
table(echo_data$drugresistant[echo_data$tbh_tb_treated_yn==1])
table(echo_data$drugresistant[echo_data$tbh_tb_treated_yn==0])
#manifestation 
tb_manifest<-ifelse(echo_data$tbd_extra_location___1==1|echo_data$tbd_extra_location___2==1|echo_data$tbd_extra_location___3==1|echo_data$tbd_extra_location___4==1|
                      echo_data$tbd_extra_location___5==1|echo_data$tbd_extra_location___6==1|echo_data$tbd_extra_location___7==1|echo_data$tbd_extra_location___88==1|
                      echo_data$tbd_extra_location___99==1, "extra", "pulm")
table(tb_manifest)


##clinical characteristics
#bp 
summary(echo_data$sbp)
summary(echo_data$dbp)
#bmi
summary(echo_data$bmi)
#wt
summary(echo_data$smwt_distance_nr)

##lab results 
#crp 
summary(echo_data$lab_creactive_prot_nr)
#neutr
summary(echo_data$lab_neutrophils_nr)
#lymph
summary(echo_data$lab_lymphocytes_nr)
#eos
summary(echo_data$lab_eosinophils_nr)
#cd4
summary(echo_data$hiv_cd4_mm3_nr)
summary(echo_data$hiv_cd4_percentage_nr)
#hb 
summary(echo_data$lab_hemoglobin_nr)



# echocardiography binary variables  --------------------------------------


##overall
#lvdil
table(echo_data$lv_dilatation)
#lvgeom
table(echo_data$abnormal_lv_geometry)
table(echo_data$type_lv_geometry_anomaly)
#aort_rd
table(echo_data$aortic_dilation)
#asc_aort_dil
table(echo_data$asc_aorta_dilation)
#valve dis
table(echo_data$av_disease)
table(echo_data$mv_disease)
table(echo_data$tv_disease)
table(echo_data$pv_disease)
#lv_dys
table(echo_data$lv_systolic_dysfunction)
table(echo_data$diast_dysf)
table(echo_data$diast_dysf_subtype)
#la_dil
table(echo_data$la_dilation)
#rv_dil 
table(echo_data$rv_dilation)
#rv_long_dys
table(echo_data$rv_long_dysf)
#rv_rad_dys
table(echo_data$rv_rad_dysf)
#ra_dil
table(echo_data$ra_dilation)
#per_eff
table(echo_data$pericard_effusion)
table(echo_data$pericard_effusion_importance)
#per_thick
table(echo_data$pericard_thickening)
#per_calc
table(echo_data$pericardial_calc)
#ra_coll
table(echo_data$ra_collapse)
#ab_mi
table(echo_data$abnormal_mitral_inflow)
#ab_ti
table(echo_data$abnormal_tricuspid_inflow)



##hiv positive
#lvdil
table(echo_data$lv_dilatation[echo_data$hiv_test_result==1])
#lvgeom
table(echo_data$abnormal_lv_geometry[echo_data$hiv_test_result==1])
table(echo_data$type_lv_geometry_anomaly[echo_data$hiv_test_result==1])
#aort_rd
table(echo_data$aortic_dilation[echo_data$hiv_test_result==1])
#asc_aort_dil
table(echo_data$asc_aorta_dilation[echo_data$hiv_test_result==1])
#valve dis
table(echo_data$av_disease[echo_data$hiv_test_result==1])
table(echo_data$mv_disease[echo_data$hiv_test_result==1])
table(echo_data$tv_disease[echo_data$hiv_test_result==1])
table(echo_data$pv_disease[echo_data$hiv_test_result==1])
#lv_dys
table(echo_data$lv_systolic_dysfunction[echo_data$hiv_test_result==1])
table(echo_data$diast_dysf[echo_data$hiv_test_result==1])
table(echo_data$diast_dysf_subtype[echo_data$hiv_test_result==1])
#la_dil
table(echo_data$la_dilation[echo_data$hiv_test_result==1])
#rv_dil 
table(echo_data$rv_dilation[echo_data$hiv_test_result==1])
#rv_long_dys
table(echo_data$rv_long_dysf[echo_data$hiv_test_result==1])
#rv_rad_dys
table(echo_data$rv_rad_dysf[echo_data$hiv_test_result==1])
#ra_dil
table(echo_data$ra_dilation[echo_data$hiv_test_result==1])
#per_eff
table(echo_data$pericard_effusion[echo_data$hiv_test_result==1])
table(echo_data$pericard_effusion_importance[echo_data$hiv_test_result==1])
#per_thick
table(echo_data$pericard_thickening[echo_data$hiv_test_result==1])
#per_calc
table(echo_data$pericardial_calc[echo_data$hiv_test_result==1])
#ra_coll
table(echo_data$ra_collapse[echo_data$hiv_test_result==1])
#ab_mi
table(echo_data$abnormal_mitral_inflow[echo_data$hiv_test_result==1])
#ab_ti
table(echo_data$abnormal_tricuspid_inflow[echo_data$hiv_test_result==1])


##hiv negative
#lvdil
table(echo_data$lv_dilatation[echo_data$hiv_test_result==0])
#lvgeom
table(echo_data$abnormal_lv_geometry[echo_data$hiv_test_result==0])
table(echo_data$type_lv_geometry_anomaly[echo_data$hiv_test_result==0])
#aort_rd
table(echo_data$aortic_dilation[echo_data$hiv_test_result==0])
#asc_aort_dil
table(echo_data$asc_aorta_dilation[echo_data$hiv_test_result==0])
#valve dis
table(echo_data$av_disease[echo_data$hiv_test_result==0])
table(echo_data$mv_disease[echo_data$hiv_test_result==0])
table(echo_data$tv_disease[echo_data$hiv_test_result==0])
table(echo_data$pv_disease[echo_data$hiv_test_result==0])
#lv_dys
table(echo_data$lv_systolic_dysfunction[echo_data$hiv_test_result==0])
table(echo_data$diast_dysf[echo_data$hiv_test_result==0])
table(echo_data$diast_dysf_subtype[echo_data$hiv_test_result==0])
#la_dil
table(echo_data$la_dilation[echo_data$hiv_test_result==0])
#rv_dil 
table(echo_data$rv_dilation[echo_data$hiv_test_result==0])
#rv_long_dys
table(echo_data$rv_long_dysf[echo_data$hiv_test_result==0])
#rv_rad_dys
table(echo_data$rv_rad_dysf[echo_data$hiv_test_result==0])
#ra_dil
table(echo_data$ra_dilation[echo_data$hiv_test_result==0])
#per_eff
table(echo_data$pericard_effusion[echo_data$hiv_test_result==0])
table(echo_data$pericard_effusion_importance[echo_data$hiv_test_result==0])
#per_thick
table(echo_data$pericard_thickening[echo_data$hiv_test_result==0])
#per_calc
table(echo_data$pericardial_calc[echo_data$hiv_test_result==0])
#ra_coll
table(echo_data$ra_collapse[echo_data$hiv_test_result==0])
#ab_mi
table(echo_data$abnormal_mitral_inflow[echo_data$hiv_test_result==0])
#ab_ti
table(echo_data$abnormal_tricuspid_inflow[echo_data$hiv_test_result==0])

##previous treatment yes
#lvdil
table(echo_data$lv_dilatation[echo_data$tbh_tb_treated_yn==1])
#lvgeom
table(echo_data$abnormal_lv_geometry[echo_data$tbh_tb_treated_yn==1])
table(echo_data$type_lv_geometry_anomaly[echo_data$tbh_tb_treated_yn==1])
#aort_rd
table(echo_data$aortic_dilation[echo_data$tbh_tb_treated_yn==1])
#asc_aort_dil
table(echo_data$asc_aorta_dilation[echo_data$tbh_tb_treated_yn==1])
#valve dis
table(echo_data$av_disease[echo_data$tbh_tb_treated_yn==1])
table(echo_data$mv_disease[echo_data$tbh_tb_treated_yn==1])
table(echo_data$tv_disease[echo_data$tbh_tb_treated_yn==1])
table(echo_data$pv_disease[echo_data$tbh_tb_treated_yn==1])
#lv_dys
table(echo_data$lv_systolic_dysfunction[echo_data$tbh_tb_treated_yn==1])
table(echo_data$diast_dysf[echo_data$tbh_tb_treated_yn==1])
table(echo_data$diast_dysf_subtype[echo_data$tbh_tb_treated_yn==1])
#la_dil
table(echo_data$la_dilation[echo_data$tbh_tb_treated_yn==1])
#rv_dil 
table(echo_data$rv_dilation[echo_data$tbh_tb_treated_yn==1])
#rv_long_dys
table(echo_data$rv_long_dysf[echo_data$tbh_tb_treated_yn==1])
#rv_rad_dys
table(echo_data$rv_rad_dysf[echo_data$tbh_tb_treated_yn==1])
#ra_dil
table(echo_data$ra_dilation[echo_data$tbh_tb_treated_yn==1])
#per_eff
table(echo_data$pericard_effusion[echo_data$tbh_tb_treated_yn==1])
table(echo_data$pericard_effusion_importance[echo_data$tbh_tb_treated_yn==1])
#per_thick
table(echo_data$pericard_thickening[echo_data$tbh_tb_treated_yn==1])
#per_calc
table(echo_data$pericardial_calc[echo_data$tbh_tb_treated_yn==1])
#ra_coll
table(echo_data$ra_collapse[echo_data$tbh_tb_treated_yn==1])
#ab_mi
table(echo_data$abnormal_mitral_inflow[echo_data$tbh_tb_treated_yn==1])
#ab_ti
table(echo_data$abnormal_tricuspid_inflow[echo_data$tbh_tb_treated_yn==1])

##previous treatment no 
#lvdil
table(echo_data$lv_dilatation[echo_data$tbh_tb_treated_yn==0])
#lvgeom
table(echo_data$abnormal_lv_geometry[echo_data$tbh_tb_treated_yn==0])
table(echo_data$type_lv_geometry_anomaly[echo_data$tbh_tb_treated_yn==0])
#aort_rd
table(echo_data$aortic_dilation[echo_data$tbh_tb_treated_yn==0])
#asc_aort_dil
table(echo_data$asc_aorta_dilation[echo_data$tbh_tb_treated_yn==0])
#valve dis
table(echo_data$av_disease[echo_data$tbh_tb_treated_yn==0])
table(echo_data$mv_disease[echo_data$tbh_tb_treated_yn==0])
table(echo_data$tv_disease[echo_data$tbh_tb_treated_yn==0])
table(echo_data$pv_disease[echo_data$tbh_tb_treated_yn==0])
#lv_dys
table(echo_data$lv_systolic_dysfunction[echo_data$tbh_tb_treated_yn==0])
table(echo_data$diast_dysf[echo_data$tbh_tb_treated_yn==0])
table(echo_data$diast_dysf_subtype[echo_data$tbh_tb_treated_yn==0])
#la_dil
table(echo_data$la_dilation[echo_data$tbh_tb_treated_yn==0])
#rv_dil 
table(echo_data$rv_dilation[echo_data$tbh_tb_treated_yn==0])
#rv_long_dys
table(echo_data$rv_long_dysf[echo_data$tbh_tb_treated_yn==0])
#rv_rad_dys
table(echo_data$rv_rad_dysf[echo_data$tbh_tb_treated_yn==0])
#ra_dil
table(echo_data$ra_dilation[echo_data$tbh_tb_treated_yn==0])
#per_eff
table(echo_data$pericard_effusion[echo_data$tbh_tb_treated_yn==0])
table(echo_data$pericard_effusion_importance[echo_data$tbh_tb_treated_yn==0])
#per_thick
table(echo_data$pericard_thickening[echo_data$tbh_tb_treated_yn==0])
#per_calc
table(echo_data$pericardial_calc[echo_data$tbh_tb_treated_yn==0])
#ra_coll
table(echo_data$ra_collapse[echo_data$tbh_tb_treated_yn==0])
#ab_mi
table(echo_data$abnormal_mitral_inflow[echo_data$tbh_tb_treated_yn==0])
#ab_ti
table(echo_data$abnormal_tricuspid_inflow[echo_data$tbh_tb_treated_yn==0])

##according to drug resistance status 
#lvdil
table(echo_data$lv_dilatation[echo_data$drugresistant==1])
#lvgeom
table(echo_data$abnormal_lv_geometry[echo_data$drugresistant==1])
table(echo_data$type_lv_geometry_anomaly[echo_data$drugresistant==1])
#aort_rd
table(echo_data$aortic_dilation[echo_data$drugresistant==1])
#asc_aort_dil
table(echo_data$asc_aorta_dilation[echo_data$drugresistant==1])
#valve dis
table(echo_data$av_disease[echo_data$drugresistant==1])
table(echo_data$mv_disease[echo_data$drugresistant==1])
table(echo_data$tv_disease[echo_data$drugresistant==1])
table(echo_data$pv_disease[echo_data$drugresistant==1])
#lv_dys
table(echo_data$lv_systolic_dysfunction[echo_data$drugresistant==1])
table(echo_data$diast_dysf[echo_data$drugresistant==1])
table(echo_data$diast_dysf_subtype[echo_data$drugresistant==1])
#la_dil
table(echo_data$la_dilation[echo_data$drugresistant==1])
#rv_dil 
table(echo_data$rv_dilation[echo_data$drugresistant==1])
#rv_long_dys
table(echo_data$rv_long_dysf[echo_data$drugresistant==1])
#rv_rad_dys
table(echo_data$rv_rad_dysf[echo_data$drugresistant==1])
#ra_dil
table(echo_data$ra_dilation[echo_data$drugresistant==1])
#per_eff
table(echo_data$pericard_effusion[echo_data$drugresistant==1])
table(echo_data$pericard_effusion_importance[echo_data$drugresistant==1])
#per_thick
table(echo_data$pericard_thickening[echo_data$drugresistant==1])
#per_calc
table(echo_data$pericardial_calc[echo_data$drugresistant==1])
#ra_coll
table(echo_data$ra_collapse[echo_data$drugresistant==1])
#ab_mi
table(echo_data$abnormal_mitral_inflow[echo_data$drugresistant==1])
#ab_ti
table(echo_data$abnormal_tricuspid_inflow[echo_data$drugresistant==1])

##according to drug resistance status 
#lvdil
table(echo_data$lv_dilatation[echo_data$drugresistant==0])
#lvgeom
table(echo_data$abnormal_lv_geometry[echo_data$drugresistant==0])
table(echo_data$type_lv_geometry_anomaly[echo_data$drugresistant==0])
#aort_rd
table(echo_data$aortic_dilation[echo_data$drugresistant==0])
#asc_aort_dil
table(echo_data$asc_aorta_dilation[echo_data$drugresistant==0])
#valve dis
table(echo_data$av_disease[echo_data$drugresistant==0])
table(echo_data$mv_disease[echo_data$drugresistant==0])
table(echo_data$tv_disease[echo_data$drugresistant==0])
table(echo_data$pv_disease[echo_data$drugresistant==0])
#lv_dys
table(echo_data$lv_systolic_dysfunction[echo_data$drugresistant==0])
table(echo_data$diast_dysf[echo_data$drugresistant==0])
table(echo_data$diast_dysf_subtype[echo_data$drugresistant==0])
#la_dil
table(echo_data$la_dilation[echo_data$drugresistant==0])
#rv_dil 
table(echo_data$rv_dilation[echo_data$drugresistant==0])
#rv_long_dys
table(echo_data$rv_long_dysf[echo_data$drugresistant==0])
#rv_rad_dys
table(echo_data$rv_rad_dysf[echo_data$drugresistant==0])
#ra_dil
table(echo_data$ra_dilation[echo_data$drugresistant==0])
#per_eff
table(echo_data$pericard_effusion[echo_data$drugresistant==0])
table(echo_data$pericard_effusion_importance[echo_data$drugresistant==0])
#per_thick
table(echo_data$pericard_thickening[echo_data$drugresistant==0])
#per_calc
table(echo_data$pericardial_calc[echo_data$drugresistant==0])
#ra_coll
table(echo_data$ra_collapse[echo_data$drugresistant==0])
#ab_mi
table(echo_data$abnormal_mitral_inflow[echo_data$drugresistant==0])
#ab_ti
table(echo_data$abnormal_tricuspid_inflow[echo_data$drugresistant==0])


# echocardiography continuous variables  ----------------------------------


##overall 
#ivsd
summary(echo_data$ivsd)
#lve
summary(echo_data$lvedd)
summary(echo_data$lvesd)
#lvpwd
summary(echo_data$lvpwd)
#lvrwt
summary(echo_data$lv_rwt)
#lv mass
summary(echo_data$lv_massi)
#lvot 
summary(echo_data$lvot)
#aortic_an
summary(echo_data$aortic_annulus)
#aortic_si
summary(echo_data$aortic_sinus)
#aortic_stj
summary(echo_data$sinotubular_junction)
#asc_aorta
summary(echo_data$ascending_aorta)
#aortic_valve_gr
summary(echo_data$av_pgmean)
#aortic_valve_vm
summary(echo_data$av_vmax)
#lvef
summary(echo_data$lvef_visual)
summary(echo_data$lvef_simpson)
#gls 
summary(echo_data$lv_strain)
#lvot
summary(echo_data$lvot_svi)
#edvi
summary(echo_data$lvedvi)
#plax
summary(echo_data$la_diameter)
#lavbpi
summary(echo_data$la_volume_i)
#rvedbd
summary(echo_data$rvedd_base)
#tapse
summary(echo_data$rv_function)
#tv_an
summary(echo_data$tv_dti)
#rv_fac
summary(echo_data$fac)
#rv/ra_gr
summary(echo_data$tr_pgmax)
#peak_tr
summary(echo_data$peak_tr_velocity)
#ra_area
summary(echo_data$ra_area)
#mvea
summary(echo_data$mv_e_a)
#mve_vel
summary(echo_data$mv_e_velocity)
#mv_dec
summary(echo_data$mv_dec_time)
#e 
summary(echo_data$e_septal)
summary(echo_data$e_lateral)
summary(echo_data$e_inferior)
summary(echo_data$e_anterior)
#vena_cava
summary(echo_data$ivc_expirium)
summary(echo_data$ivc_inspirium)
summary(echo_data$ivc_change)
#cvp
summary(echo_data$cvp)



##hiv positive

#ivsd
summary(echo_data$ivsd[echo_data$hiv_test_result==1])
#lve
summary(echo_data$lvedd[echo_data$hiv_test_result==1])
summary(echo_data$lvesd[echo_data$hiv_test_result==1])
#lvpwd
summary(echo_data$lvpwd[echo_data$hiv_test_result==1])
#lvrwt
summary(echo_data$lv_rwt[echo_data$hiv_test_result==1])
#lv mass
summary(echo_data$lv_massi[echo_data$hiv_test_result==1])
#lvot 
summary(echo_data$lvot[echo_data$hiv_test_result==1])
#aortic_an
summary(echo_data$aortic_annulus[echo_data$hiv_test_result==1])
#aortic_si
summary(echo_data$aortic_sinus[echo_data$hiv_test_result==1])
#aortic_stj
summary(echo_data$sinotubular_junction[echo_data$hiv_test_result==1])
#asc_aorta
summary(echo_data$ascending_aorta[echo_data$hiv_test_result==1])
#aortic_valve_gr
summary(echo_data$av_pgmean[echo_data$hiv_test_result==1])
#aortic_valve_vm
summary(echo_data$av_vmax[echo_data$hiv_test_result==1])
#lvef
summary(echo_data$lvef_visual[echo_data$hiv_test_result==1])
summary(echo_data$lvef_simpson[echo_data$hiv_test_result==1])
#gls 
summary(echo_data$lv_strain[echo_data$hiv_test_result==1])
#lvot
summary(echo_data$lvot_svi[echo_data$hiv_test_result==1])
#edvi
summary(echo_data$lvedvi[echo_data$hiv_test_result==1])
#plax
summary(echo_data$la_diameter[echo_data$hiv_test_result==1])
#lavbpi
summary(echo_data$la_volume_i[echo_data$hiv_test_result==1])
#rvedbd
summary(echo_data$rvedd_base[echo_data$hiv_test_result==1])
#tapse
summary(echo_data$rv_function[echo_data$hiv_test_result==1])
#tv_an
summary(echo_data$tv_dti[echo_data$hiv_test_result==1])
#rv_fac
summary(echo_data$fac[echo_data$hiv_test_result==1])
#rv/ra_gr
summary(echo_data$tr_pgmax[echo_data$hiv_test_result==1])
#peak_tr
summary(echo_data$peak_tr_velocity[echo_data$hiv_test_result==1])
#ra_area
summary(echo_data$ra_area[echo_data$hiv_test_result==1])
#mvea
summary(echo_data$mv_e_a[echo_data$hiv_test_result==1])
#mve_vel
summary(echo_data$mv_e_velocity[echo_data$hiv_test_result==1])
#mv_dec
summary(echo_data$mv_dec_time[echo_data$hiv_test_result==1])
#e 
summary(echo_data$e_septal[echo_data$hiv_test_result==1])
summary(echo_data$e_lateral[echo_data$hiv_test_result==1])
summary(echo_data$e_inferior[echo_data$hiv_test_result==1])
summary(echo_data$e_anterior[echo_data$hiv_test_result==1])
#vena_cava
summary(echo_data$ivc_expirium[echo_data$hiv_test_result==1])
summary(echo_data$ivc_inspirium[echo_data$hiv_test_result==1])
summary(echo_data$ivc_change[echo_data$hiv_test_result==1])
#cvp
summary(echo_data$cvp[echo_data$hiv_test_result==1])


##hiv negative

#ivsd
summary(echo_data$ivsd[echo_data$hiv_test_result==0])
#lve
summary(echo_data$lvedd[echo_data$hiv_test_result==0])
summary(echo_data$lvesd[echo_data$hiv_test_result==0])
#lvpwd
summary(echo_data$lvpwd[echo_data$hiv_test_result==0])
#lvrwt
summary(echo_data$lv_rwt[echo_data$hiv_test_result==0])
#lv mass
summary(echo_data$lv_massi[echo_data$hiv_test_result==0])
#lvot 
summary(echo_data$lvot[echo_data$hiv_test_result==0])
#aortic_an
summary(echo_data$aortic_annulus[echo_data$hiv_test_result==0])
#aortic_si
summary(echo_data$aortic_sinus[echo_data$hiv_test_result==0])
#aortic_stj
summary(echo_data$sinotubular_junction[echo_data$hiv_test_result==0])
#asc_aorta
summary(echo_data$ascending_aorta[echo_data$hiv_test_result==0])
#aortic_valve_gr
summary(echo_data$av_pgmean[echo_data$hiv_test_result==0])
#aortic_valve_vm
summary(echo_data$av_vmax[echo_data$hiv_test_result==0])
#lvef
summary(echo_data$lvef_visual[echo_data$hiv_test_result==0])
summary(echo_data$lvef_simpson[echo_data$hiv_test_result==0])
#gls 
summary(echo_data$lv_strain[echo_data$hiv_test_result==0])
#lvot
summary(echo_data$lvot_svi[echo_data$hiv_test_result==0])
#edvi
summary(echo_data$lvedvi[echo_data$hiv_test_result==0])
#plax
summary(echo_data$la_diameter[echo_data$hiv_test_result==0])
#lavbpi
summary(echo_data$la_volume_i[echo_data$hiv_test_result==0])
#rvedbd
summary(echo_data$rvedd_base[echo_data$hiv_test_result==0])
#tapse
summary(echo_data$rv_function[echo_data$hiv_test_result==0])
#tv_an
summary(echo_data$tv_dti[echo_data$hiv_test_result==0])
#rv_fac
summary(echo_data$fac[echo_data$hiv_test_result==0])
#rv/ra_gr
summary(echo_data$tr_pgmax[echo_data$hiv_test_result==0])
#peak_tr
summary(echo_data$peak_tr_velocity[echo_data$hiv_test_result==0])
#ra_area
summary(echo_data$ra_area[echo_data$hiv_test_result==0])
#mvea
summary(echo_data$mv_e_a[echo_data$hiv_test_result==0])
#mve_vel
summary(echo_data$mv_e_velocity[echo_data$hiv_test_result==0])
#mv_dec
summary(echo_data$mv_dec_time[echo_data$hiv_test_result==0])
#e 
summary(echo_data$e_septal[echo_data$hiv_test_result==0])
summary(echo_data$e_lateral[echo_data$hiv_test_result==0])
summary(echo_data$e_inferior[echo_data$hiv_test_result==0])
summary(echo_data$e_anterior[echo_data$hiv_test_result==0])
#vena_cava
summary(echo_data$ivc_expirium[echo_data$hiv_test_result==0])
summary(echo_data$ivc_inspirium[echo_data$hiv_test_result==0])
summary(echo_data$ivc_change[echo_data$hiv_test_result==0])
#cvp
summary(echo_data$cvp[echo_data$hiv_test_result==0])



##Previous treatment yes

#ivsd
summary(echo_data$ivsd[echo_data$tbh_tb_treated_yn==1])
#lve
summary(echo_data$lvedd[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$lvesd[echo_data$tbh_tb_treated_yn==1])
#lvpwd
summary(echo_data$lvpwd[echo_data$tbh_tb_treated_yn==1])
#lvrwt
summary(echo_data$lv_rwt[echo_data$tbh_tb_treated_yn==1])
#lv mass
summary(echo_data$lv_massi[echo_data$tbh_tb_treated_yn==1])
#lvot 
summary(echo_data$lvot[echo_data$tbh_tb_treated_yn==1])
#aortic_an
summary(echo_data$aortic_annulus[echo_data$tbh_tb_treated_yn==1])
#aortic_si
summary(echo_data$aortic_sinus[echo_data$tbh_tb_treated_yn==1])
#aortic_stj
summary(echo_data$sinotubular_junction[echo_data$tbh_tb_treated_yn==1])
#asc_aorta
summary(echo_data$ascending_aorta[echo_data$tbh_tb_treated_yn==1])
#aortic_valve_gr
summary(echo_data$av_pgmean[echo_data$tbh_tb_treated_yn==1])
#aortic_valve_vm
summary(echo_data$av_vmax[echo_data$tbh_tb_treated_yn==1])
#lvef
summary(echo_data$lvef_visual[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$lvef_simpson[echo_data$tbh_tb_treated_yn==1])
#gls 
summary(echo_data$lv_strain[echo_data$tbh_tb_treated_yn==1])
#lvot
summary(echo_data$lvot_svi[echo_data$tbh_tb_treated_yn==1])
#edvi
summary(echo_data$lvedvi[echo_data$tbh_tb_treated_yn==1])
#plax
summary(echo_data$la_diameter[echo_data$tbh_tb_treated_yn==1])
#lavbpi
summary(echo_data$la_volume_i[echo_data$tbh_tb_treated_yn==1])
#rvedbd
summary(echo_data$rvedd_base[echo_data$tbh_tb_treated_yn==1])
#tapse
summary(echo_data$rv_function[echo_data$tbh_tb_treated_yn==1])
#tv_an
summary(echo_data$tv_dti[echo_data$tbh_tb_treated_yn==1])
#rv_fac
summary(echo_data$fac[echo_data$tbh_tb_treated_yn==1])
#rv/ra_gr
summary(echo_data$tr_pgmax[echo_data$tbh_tb_treated_yn==1])
#peak_tr
summary(echo_data$peak_tr_velocity[echo_data$tbh_tb_treated_yn==1])
#ra_area
summary(echo_data$ra_area[echo_data$tbh_tb_treated_yn==1])
#mvea
summary(echo_data$mv_e_a[echo_data$tbh_tb_treated_yn==1])
#mve_vel
summary(echo_data$mv_e_velocity[echo_data$tbh_tb_treated_yn==1])
#mv_dec
summary(echo_data$mv_dec_time[echo_data$tbh_tb_treated_yn==1])
#e 
summary(echo_data$e_septal[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$e_lateral[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$e_inferior[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$e_anterior[echo_data$tbh_tb_treated_yn==1])
#vena_cava
summary(echo_data$ivc_expirium[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$ivc_inspirium[echo_data$tbh_tb_treated_yn==1])
summary(echo_data$ivc_change[echo_data$tbh_tb_treated_yn==1])
#cvp
summary(echo_data$cvp[echo_data$tbh_tb_treated_yn==1])



##previous treatment no

#ivsd
summary(echo_data$ivsd[echo_data$tbh_tb_treated_yn==0])
#lve
summary(echo_data$lvedd[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$lvesd[echo_data$tbh_tb_treated_yn==0])
#lvpwd
summary(echo_data$lvpwd[echo_data$tbh_tb_treated_yn==0])
#lvrwt
summary(echo_data$lv_rwt[echo_data$tbh_tb_treated_yn==0])
#lv mass
summary(echo_data$lv_massi[echo_data$tbh_tb_treated_yn==0])
#lvot 
summary(echo_data$lvot[echo_data$tbh_tb_treated_yn==0])
#aortic_an
summary(echo_data$aortic_annulus[echo_data$tbh_tb_treated_yn==0])
#aortic_si
summary(echo_data$aortic_sinus[echo_data$tbh_tb_treated_yn==0])
#aortic_stj
summary(echo_data$sinotubular_junction[echo_data$tbh_tb_treated_yn==0])
#asc_aorta
summary(echo_data$ascending_aorta[echo_data$tbh_tb_treated_yn==0])
#aortic_valve_gr
summary(echo_data$av_pgmean[echo_data$tbh_tb_treated_yn==0])
#aortic_valve_vm
summary(echo_data$av_vmax[echo_data$tbh_tb_treated_yn==0])
#lvef
summary(echo_data$lvef_visual[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$lvef_simpson[echo_data$tbh_tb_treated_yn==0])
#gls 
summary(echo_data$lv_strain[echo_data$tbh_tb_treated_yn==0])
#lvot
summary(echo_data$lvot_svi[echo_data$tbh_tb_treated_yn==0])
#edvi
summary(echo_data$lvedvi[echo_data$tbh_tb_treated_yn==0])
#plax
summary(echo_data$la_diameter[echo_data$tbh_tb_treated_yn==0])
#lavbpi
summary(echo_data$la_volume_i[echo_data$tbh_tb_treated_yn==0])
#rvedbd
summary(echo_data$rvedd_base[echo_data$tbh_tb_treated_yn==0])
#tapse
summary(echo_data$rv_function[echo_data$tbh_tb_treated_yn==0])
#tv_an
summary(echo_data$tv_dti[echo_data$tbh_tb_treated_yn==0])
#rv_fac
summary(echo_data$fac[echo_data$tbh_tb_treated_yn==0])
#rv/ra_gr
summary(echo_data$tr_pgmax[echo_data$tbh_tb_treated_yn==0])
#peak_tr
summary(echo_data$peak_tr_velocity[echo_data$tbh_tb_treated_yn==0])
#ra_area
summary(echo_data$ra_area[echo_data$tbh_tb_treated_yn==0])
#mvea
summary(echo_data$mv_e_a[echo_data$tbh_tb_treated_yn==0])
#mve_vel
summary(echo_data$mv_e_velocity[echo_data$tbh_tb_treated_yn==0])
#mv_dec
summary(echo_data$mv_dec_time[echo_data$tbh_tb_treated_yn==0])
#e 
summary(echo_data$e_septal[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$e_lateral[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$e_inferior[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$e_anterior[echo_data$tbh_tb_treated_yn==0])
#vena_cava
summary(echo_data$ivc_expirium[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$ivc_inspirium[echo_data$tbh_tb_treated_yn==0])
summary(echo_data$ivc_change[echo_data$tbh_tb_treated_yn==0])
#cvp
summary(echo_data$cvp[echo_data$tbh_tb_treated_yn==0])

##according to drug resistance status
#ivsd
summary(echo_data$ivsd[echo_data$drugresistant==1])
#lve
summary(echo_data$lvedd[echo_data$drugresistant==1])
summary(echo_data$lvesd[echo_data$drugresistant==1])
#lvpwd
summary(echo_data$lvpwd[echo_data$drugresistant==1])
#lvrwt
summary(echo_data$lv_rwt[echo_data$drugresistant==1])
#lv mass
summary(echo_data$lv_massi[echo_data$drugresistant==1])
#lvot 
summary(echo_data$lvot[echo_data$drugresistant==1])
#aortic_an
summary(echo_data$aortic_annulus[echo_data$drugresistant==1])
#aortic_si
summary(echo_data$aortic_sinus[echo_data$drugresistant==1])
#aortic_stj
summary(echo_data$sinotubular_junction[echo_data$drugresistant==1])
#asc_aorta
summary(echo_data$ascending_aorta[echo_data$drugresistant==1])
#aortic_valve_gr
summary(echo_data$av_pgmean[echo_data$drugresistant==1])
#aortic_valve_vm
summary(echo_data$av_vmax[echo_data$drugresistant==1])
#lvef
summary(echo_data$lvef_visual[echo_data$drugresistant==1])
summary(echo_data$lvef_simpson[echo_data$drugresistant==1])
#lvot
summary(echo_data$lvot_svi[echo_data$drugresistant==1])
#plax
summary(echo_data$la_diameter[echo_data$drugresistant==1])
#lavbpi
summary(echo_data$la_volume_i[echo_data$drugresistant==1])
#rvedbd
summary(echo_data$rvedd_base[echo_data$drugresistant==1])
#tapse
summary(echo_data$rv_function[echo_data$drugresistant==1])
#tv_an
summary(echo_data$tv_dti[echo_data$drugresistant==1])
#rv_fac
summary(echo_data$fac[echo_data$drugresistant==1])
#rv/ra_gr
summary(echo_data$tr_pgmax[echo_data$drugresistant==1])
#peak_tr
summary(echo_data$peak_tr_velocity[echo_data$drugresistant==1])
#ra_area
summary(echo_data$ra_area[echo_data$drugresistant==1])
#mvea
summary(echo_data$mv_e_a[echo_data$drugresistant==1])
#mve_vel
summary(echo_data$mv_e_velocity[echo_data$drugresistant==1])
#mv_dec
summary(echo_data$mv_dec_time[echo_data$drugresistant==1])
#e 
summary(echo_data$e_septal[echo_data$drugresistant==1])
summary(echo_data$e_lateral[echo_data$drugresistant==1])
summary(echo_data$e_inferior[echo_data$drugresistant==1])
summary(echo_data$e_anterior[echo_data$drugresistant==1])
#vena_cava
summary(echo_data$ivc_expirium[echo_data$drugresistant==1])
summary(echo_data$ivc_inspirium[echo_data$drugresistant==1])
summary(echo_data$ivc_change[echo_data$drugresistant==1])
#cvp
summary(echo_data$cvp[echo_data$drugresistant==1])


##according to drug resistance status
#ivsd
summary(echo_data$ivsd[echo_data$drugresistant==0])
#lve
summary(echo_data$lvedd[echo_data$drugresistant==0])
summary(echo_data$lvesd[echo_data$drugresistant==0])
#lvpwd
summary(echo_data$lvpwd[echo_data$drugresistant==0])
#lvrwt
summary(echo_data$lv_rwt[echo_data$drugresistant==0])
#lv mass
summary(echo_data$lv_massi[echo_data$drugresistant==0])
#lvot 
summary(echo_data$lvot[echo_data$drugresistant==0])
#aortic_an
summary(echo_data$aortic_annulus[echo_data$drugresistant==0])
#aortic_si
summary(echo_data$aortic_sinus[echo_data$drugresistant==0])
#aortic_stj
summary(echo_data$sinotubular_junction[echo_data$drugresistant==0])
#asc_aorta
summary(echo_data$ascending_aorta[echo_data$drugresistant==0])
#aortic_valve_gr
summary(echo_data$av_pgmean[echo_data$drugresistant==0])
#aortic_valve_vm
summary(echo_data$av_vmax[echo_data$drugresistant==0])
#lvef
summary(echo_data$lvef_visual[echo_data$drugresistant==0])
summary(echo_data$lvef_simpson[echo_data$drugresistant==0])
#lvot
summary(echo_data$lvot_svi[echo_data$drugresistant==0])
#plax
summary(echo_data$la_diameter[echo_data$drugresistant==0])
#lavbpi
summary(echo_data$la_volume_i[echo_data$drugresistant==0])
#rvedbd
summary(echo_data$rvedd_base[echo_data$drugresistant==0])
#tapse
summary(echo_data$rv_function[echo_data$drugresistant==0])
#tv_an
summary(echo_data$tv_dti[echo_data$drugresistant==0])
#rv_fac
summary(echo_data$fac[echo_data$drugresistant==0])
#rv/ra_gr
summary(echo_data$tr_pgmax[echo_data$drugresistant==0])
#peak_tr
summary(echo_data$peak_tr_velocity[echo_data$drugresistant==0])
#ra_area
summary(echo_data$ra_area[echo_data$drugresistant==0])
#mvea
summary(echo_data$mv_e_a[echo_data$drugresistant==0])
#mve_vel
summary(echo_data$mv_e_velocity[echo_data$drugresistant==0])
#mv_dec
summary(echo_data$mv_dec_time[echo_data$drugresistant==0])
#e 
summary(echo_data$e_septal[echo_data$drugresistant==0])
summary(echo_data$e_lateral[echo_data$drugresistant==0])
summary(echo_data$e_inferior[echo_data$drugresistant==0])
summary(echo_data$e_anterior[echo_data$drugresistant==0])
#vena_cava
summary(echo_data$ivc_expirium[echo_data$drugresistant==0])
summary(echo_data$ivc_inspirium[echo_data$drugresistant==0])
summary(echo_data$ivc_change[echo_data$drugresistant==0])
#cvp
summary(echo_data$cvp[echo_data$drugresistant==0])

# logistic regression model  ----------------------------------------------



model<- lm(hiv_test_result ~ lv_dilation, data = echo_data)
summary(model)
exp(-0.4535)

summary(echo_data$age)
table(echo_data$age)
