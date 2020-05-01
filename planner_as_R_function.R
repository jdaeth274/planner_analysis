###############################################################################
## Lets transform the excel sheet into an R function ##########################
###############################################################################

require(ggpubr)
require(ggplot2)

## Baseline inputs ##

total_cc_beds <- 4123
total_ga_beds <- 99935
cc_occupied_non_covid <- 3423
ga_occupied_non_covid <- 91971
non_covid_vent <- 0.43
num_vents <- 8175
cc_nurses_fte <- 3939
ga_nurses_fte <- 32354
cc_sen_doc_fte <- 965
cc_jun_doc_fte <- 677
ga_sen_doc_fte <- 12680
ga_jun_doc_fte <- 10293

## ratios ##
cc_bed_nurse <- 1
ga_bed_nurse <- 5
cc_bed_sen_doc <- 15
cc_bed_jun_doc <- 8
ga_bed_sen_doc <- 15
ga_bed_jun_doc <- 15

## covid_inputs ##

covid_cc_num <- 1423
covid_ga_num <- (1423/0.16) - 1423
covid_vent <- 0.63
nurse_covid_sickness <- 0.92
doc_covid_sickness <- 0.86
headcount_to_fte <- 0.88
pop_size <- 55977200

## intervention effects ##

cancel_elective_cc <- 0.3
cancel_elective_ga <- 0.41
nice_cc <- 0.6

field_cc <- 500
field_ga <- 8000

private_beds <- 8000
private_ventilators <- 1200
private_nurse_headcount <- 10000
private_doctor_headcount <- 700
private_theatres <- 7
private_beds_theatres <- 2

theatres_convert <- 2404
beds_per_theatre <- 2

cc_bed_gain_ga <- 2000

cc_nurse_gain <- 647
cc_jun_gain <- 206

return_nurse_gain <- 6147
return_doctor_gain <- 2660

student_nurse <- 18700
student_doc <- 5500

international_doc <- 264

new_vents <- 20000

## combine into lists ##

baseline_info <- c(total_cc_beds, total_ga_beds, cc_occupied_non_covid,#3
                   ga_occupied_non_covid, non_covid_vent, num_vents,#6
                   cc_nurses_fte, ga_nurses_fte, cc_sen_doc_fte,#9
                   cc_jun_doc_fte, ga_jun_doc_fte, ga_sen_doc_fte, #12
                   cc_bed_nurse, ga_bed_nurse, cc_bed_sen_doc,#15
                   cc_bed_jun_doc, ga_bed_sen_doc, ga_bed_jun_doc,#18
                   covid_cc_num, covid_ga_num, covid_vent,#21
                   nurse_covid_sickness, doc_covid_sickness,headcount_to_fte,#24
                   pop_size)#25
intervention_info <- c(cancel_elective_cc, cancel_elective_ga,#2
                       nice_cc,#3
                       field_cc, field_ga,#5
                       private_beds, private_ventilators, private_nurse_headcount,#8
                       private_doctor_headcount, private_theatres, private_beds_theatres,#11
                       theatres_convert, beds_per_theatre,#13
                       cc_bed_gain_ga,#14
                       cc_nurse_gain, cc_jun_gain,#16
                       return_nurse_gain, return_doctor_gain,#18
                       student_nurse, student_doc,#20
                       international_doc,#21
                       new_vents)#22)

hospital_capacity_electives <- function(baseline_vector, intervention_vector, combination_vector = NULL,
                                        covid_cc_patients = seq(0,5000,1)){
  
  
  spare_capacity <- NULL                                                                                                          
  for(k in 1:length(covid_cc_patients)){                                                                  
    print(k)

    covid_ga <- round((covid_cc_patients[k] / 0.16) - covid_cc_patients[k])
    ga_cc_bed_split <- baseline_vector[1]/(baseline_vector[1] + baseline_vector[2])
    cc_ga_nurse_split <- baseline_vector[7] / (baseline_vector[7] + baseline_vector[8])
    cc_jun_doc_prop <- baseline_vector[10] / sum(baseline_vector[9:12]) 
    ga_jun_doc_prop <- baseline_vector[11] / sum(baseline_vector[9:12]) 
    cc_sen_doc_prop <- baseline_vector[9] / sum(baseline_vector[9:12])
    ga_sen_doc_prop <- baseline_vector[12] / sum(baseline_vector[9:12]) 
    ## Current capacity load ##
    
    cc_patients <- baseline_vector[3] + covid_cc_patients[k]
    ga_patients <- baseline_vector[4] + covid_ga
    ventilator_need <- (baseline_vector[3] * baseline_vector[5]) + (covid_cc_patients[k] * baseline_vector[21])
    
    ## baseline capacity ## 
    
    cc_beds <- ((baseline_vector[1] - cc_patients) / baseline_vector[25]) * 10000
    ga_beds <- ((baseline_vector[2] - ga_patients) / baseline_vector[25]) * 10000
    vents <- ((baseline_vector[6] - ventilator_need) / baseline_vector[25]) * 10000
    cc_nurses <- (((baseline_vector[7] * baseline_vector[22]) - (cc_patients/baseline_vector[13])) / baseline_vector[25]) * 10000
    ga_nurses <- (((baseline_vector[8] * baseline_vector[22])  - (ga_patients/baseline_vector[14])) / baseline_vector[25]) * 10000
    cc_sen_doc <- (((baseline_vector[9] * baseline_vector[23]) - (cc_patients/baseline_vector[15])) / baseline_vector[25]) * 10000
    cc_jun_doc <- (((baseline_vector[10] * baseline_vector[23]) - (cc_patients/baseline_vector[16])) / baseline_vector[25]) * 10000
    ga_sen_doc <- (((baseline_vector[12] * baseline_vector[23]) - (ga_patients/baseline_vector[17])) / baseline_vector[25]) * 10000
    ga_jun_doc <- (((baseline_vector[11] * baseline_vector[23]) - (ga_patients/baseline_vector[18])) / baseline_vector[25]) * 10000
    
    baseline <- c(cc_beds, ga_beds, vents, cc_nurses, ga_nurses, cc_sen_doc, cc_jun_doc,
                  ga_sen_doc, ga_jun_doc)
    baseline_gains <- rep(0, length(baseline))
    
    ## Cancel electives ##
    
    cancel_electives <- baseline
    cc_patients_electives <- baseline_vector[3] * (1 - intervention_vector[1]) + covid_cc_patients[k]
    ga_patients_electives <- baseline_vector[4] * (1 - intervention_vector[2]) + covid_ga
    vent_need_electives <- (baseline_vector[3] * (1 - intervention_vector[1]) * baseline_vector[5]) + (covid_cc_patients[k] * baseline_vector[21])
    
    cancel_electives[1] <- ((baseline_vector[1] - cc_patients_electives) / baseline_vector[25]) * 10000
    cancel_electives[2] <- ((baseline_vector[2] - ga_patients_electives) / baseline_vector[25]) * 10000
    cancel_electives[3] <- ((baseline_vector[6] - vent_need_electives) / baseline_vector[25]) * 10000
    cancel_electives[4] <- (((baseline_vector[7] * baseline_vector[22]) - cc_patients_electives/baseline_vector[13]) / baseline_vector[25]) * 10000
    cancel_electives[5] <- (((baseline_vector[8] * baseline_vector[22]) - ga_patients_electives/baseline_vector[14]) / baseline_vector[25]) * 10000
    cancel_electives[6] <- (((baseline_vector[9] * baseline_vector[23]) - cc_patients_electives/baseline_vector[15]) / baseline_vector[25]) * 10000
    cancel_electives[7] <- (((baseline_vector[10] * baseline_vector[23]) - cc_patients_electives/baseline_vector[16]) / baseline_vector[25]) * 10000
    cancel_electives[8] <- (((baseline_vector[12] * baseline_vector[23]) - ga_patients_electives/baseline_vector[17]) / baseline_vector[25]) * 10000
    cancel_electives[9] <- (((baseline_vector[11] * baseline_vector[23]) - ga_patients_electives/baseline_vector[18]) / baseline_vector[25]) * 10000
    
    ## NICE guidlines  
    nice_guide <- baseline
    nice_guide_gains <- baseline_gains
    
    cc_patients_nice <- baseline_vector[3] * (1 - intervention_vector[3]) + covid_cc_patients[k] 
    vent_need_nice <- (baseline_vector[3] * (1 - intervention_vector[3]) * baseline_vector[5]) + (covid_cc_patients[k] * baseline_vector[21])
    
    nice_guide[1] <- ((baseline_vector[1] - cc_patients_nice) / baseline_vector[25]) * 10000
    nice_guide[3] <- ((baseline_vector[6] - vent_need_nice) / baseline_vector[25]) * 10000
    nice_guide[4] <- (((baseline_vector[7] * baseline_vector[22]) - cc_patients_nice/baseline_vector[13]) / baseline_vector[25]) * 10000
    nice_guide[6] <- (((baseline_vector[9] * baseline_vector[23]) - cc_patients_nice/baseline_vector[15]) / baseline_vector[25]) * 10000
    nice_guide[7] <- (((baseline_vector[10] * baseline_vector[23]) - cc_patients_nice/baseline_vector[16]) / baseline_vector[25]) * 10000
    
    
    ## field hospital capacity ##
    
    field_hosp <- baseline
    field_hosp_gains <- baseline_gains
    
    field_hosp_cc <- baseline_vector[1] + intervention_vector[4]
    field_hosp_ga <- baseline_vector[2] + intervention_vector[5]
    
    field_hosp[1] <- ((field_hosp_cc - cc_patients) / baseline_vector[25]) * 10000
    field_hosp[2] <- ((field_hosp_ga - ga_patients) / baseline_vector[25]) * 10000
    
    field_hosp_gains[1] <- intervention_vector[4]
    field_hosp_gains[2] <- intervention_vector[5]
    
    ## Private resources ##
    
    private_res <- baseline
    private_res_gains <- baseline_gains
    
    private_cc_beds <-  baseline_vector[1] + (ga_cc_bed_split * intervention_vector[6]) + (intervention_vector[10] * intervention_vector[11])
    private_ga_beds <- baseline_vector[2] + ((1 - ga_cc_bed_split) * intervention_vector[6])
    private_cc_nurses <- baseline_vector[7] + (cc_ga_nurse_split * baseline_vector[24] * intervention_vector[8])
    private_ga_nurses <- baseline_vector[8] + ((1- cc_ga_nurse_split) * baseline_vector[24] * intervention_vector[8])
    private_cc_sen_doc <- baseline_vector[9] + (cc_sen_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_cc_jun_doc <- baseline_vector[10] + (cc_jun_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_ga_sen_doc <- baseline_vector[12] + (ga_sen_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_ga_jun_doc <- baseline_vector[11] + (ga_jun_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_vents <- baseline_vector[6] + intervention_vector[7]
    
    private_res[1] <- ((private_cc_beds - cc_patients) / baseline_vector[25]) * 10000
    private_res[2] <- ((private_ga_beds - ga_patients) / baseline_vector[25]) * 10000
    private_res[3] <- ((private_vents - ventilator_need) / baseline_vector[25]) * 10000
    private_res[4] <- (((private_cc_nurses * baseline_vector[22]) - cc_patients/baseline_vector[13]) / baseline_vector[25]) * 10000
    private_res[5] <- (((private_ga_nurses * baseline_vector[22]) - ga_patients/baseline_vector[14]) / baseline_vector[25]) * 10000
    private_res[6] <- (((private_cc_sen_doc * baseline_vector[23]) - cc_patients/baseline_vector[15]) / baseline_vector[25]) * 10000
    private_res[7] <- (((private_cc_jun_doc * baseline_vector[23]) - cc_patients/baseline_vector[16]) / baseline_vector[25]) * 10000
    private_res[8] <- (((private_ga_sen_doc * baseline_vector[23]) - ga_patients/baseline_vector[17]) / baseline_vector[25]) * 10000
    private_res[9] <- (((private_ga_jun_doc * baseline_vector[23]) - ga_patients/baseline_vector[18]) / baseline_vector[25]) * 10000
    
    private_res_gains[1] <- (ga_cc_bed_split * intervention_vector[6]) + (intervention_vector[10] * intervention_vector[11])
    private_res_gains[2] <- ((1 - ga_cc_bed_split) * intervention_vector[6])
    private_res_gains[3] <- baseline_vector[6] + intervention_vector[7]
    private_res_gains[4] <- (cc_ga_nurse_split * baseline_vector[24] * intervention_vector[8])
    private_res_gains[5] <- ((1- cc_ga_nurse_split) * baseline_vector[24] * intervention_vector[8])
    private_res_gains[6] <- (cc_sen_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_res_gains[7] <- (cc_jun_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_res_gains[8] <- (ga_sen_doc_prop * baseline_vector[24] * intervention_vector[9])
    private_res_gains[9] <- (ga_jun_doc_prop * baseline_vector[24] * intervention_vector[9])
    
    ## Convert theatres to CC ##
    
    theatres <- baseline
    theatres_gains <- baseline_gains
    
    theatre_cc_bed <- baseline_vector[1] + (intervention_vector[12] * intervention_vector[13])
    
    theatres[1] <- ((theatre_cc_bed - cc_patients) / baseline_vector[25]) * 10000
    
    theatres_gains[1] <- (intervention_vector[12] * intervention_vector[13])
    
    ## Convert G&A beds ##
    
    convert_beds <- baseline
    convert_beds_gains <- baseline_gains
    
    convert_beds_cc <- baseline_vector[1] + intervention_vector[14]
    convert_beds_ga <- baseline_vector[2] - intervention_vector[14]
    
    convert_beds[1] <- ((convert_beds_cc - cc_patients) / baseline_vector[25]) * 10000
    convert_beds[2] <- ((convert_beds_ga - ga_patients) / baseline_vector[25]) * 10000
    
    convert_beds_gains[1] <- intervention_vector[14]
    convert_beds_gains[2] <- -intervention_vector[14]
    
    ## Upskill G&A staff ##
    
    upskill <- baseline
    upskill_gains <- baseline_gains
    
    upskill_cc_nurse <- (baseline_vector[7] + intervention_vector[15]) * baseline_vector[22]
    upskill_ga_nurse <- (baseline_vector[8] - intervention_vector[15]) * baseline_vector[22]
    upskill_cc_jun_doc <- (baseline_vector[10] + intervention_vector[16]) * baseline_vector[23]
    upskill_ga_jun_doc <- (baseline_vector[11] - intervention_vector[16]) * baseline_vector[23]
    
    upskill[4] <- (upskill_cc_nurse - cc_patients/baseline_vector[13])/baseline_vector[25] * 10000
    upskill[5] <- (upskill_ga_nurse - ga_patients/baseline_vector[14])/baseline_vector[25] * 10000
    upskill[7] <- (upskill_cc_jun_doc - cc_patients/baseline_vector[16])/baseline_vector[25] * 10000
    upskill[9] <- (upskill_ga_jun_doc - ga_patients/baseline_vector[18])/baseline_vector[25] * 10000
    
    upskill_gains[4] <-  intervention_vector[15] 
    upskill_gains[5] <- -intervention_vector[15] 
    upskill_gains[7] <- intervention_vector[16] 
    upskill_gains[9] <- - intervention_vector[16]
    
    ## Staff return ##
    
    return_staff <- baseline
    return_gains <- baseline_gains 
    
    return_cc_nurses <-  baseline_vector[7] + (cc_ga_nurse_split * baseline_vector[24] * intervention_vector[17])
    return_ga_nurses <-  baseline_vector[8] + ((1- cc_ga_nurse_split) * baseline_vector[24] * intervention_vector[17])
    return_cc_sen_doc <- baseline_vector[9] + (cc_sen_doc_prop * baseline_vector[24] * intervention_vector[18])
    return_cc_jun_doc <- baseline_vector[10] + (cc_jun_doc_prop * baseline_vector[24] * intervention_vector[18])
    return_ga_sen_doc <- baseline_vector[12] + (ga_sen_doc_prop * baseline_vector[24] * intervention_vector[18])
    return_ga_jun_doc <- baseline_vector[11] + (ga_jun_doc_prop * baseline_vector[24] * intervention_vector[18])
    
    return_staff[4] <- (return_cc_nurses * baseline_vector[22] - cc_patients/baseline_vector[13])/baseline_vector[25] * 10000 
    return_staff[5] <- (((return_ga_nurses * baseline_vector[22]) - ga_patients/baseline_vector[14]) / baseline_vector[25]) * 10000
    return_staff[6] <- (((return_cc_sen_doc * baseline_vector[23]) - cc_patients/baseline_vector[15]) / baseline_vector[25]) * 10000
    return_staff[7] <- (((return_cc_jun_doc * baseline_vector[23]) - cc_patients/baseline_vector[16]) / baseline_vector[25]) * 10000
    return_staff[8] <- (((return_ga_sen_doc * baseline_vector[23]) - ga_patients/baseline_vector[17]) / baseline_vector[25]) * 10000
    return_staff[9] <- (((return_ga_jun_doc * baseline_vector[23]) - ga_patients/baseline_vector[18]) / baseline_vector[25]) * 10000
    
    return_gains[4] <- (cc_ga_nurse_split * baseline_vector[24] * intervention_vector[17])
    return_gains[5] <- ((1- cc_ga_nurse_split) * baseline_vector[24] * intervention_vector[17])
    return_gains[6] <- (cc_sen_doc_prop * baseline_vector[24] * intervention_vector[18])
    return_gains[7] <- (cc_jun_doc_prop * baseline_vector[24] * intervention_vector[18])
    return_gains[8] <- (ga_sen_doc_prop * baseline_vector[24] * intervention_vector[18])
    return_gains[9] <- (ga_jun_doc_prop * baseline_vector[24] * intervention_vector[18])
    
    ## student deployment ##
    
    student_deploy <- baseline
    student_deploy_gains <- baseline_gains
    
    student_ga_nurse <- baseline_vector[8] + (intervention_vector[19] * baseline_vector[24])
    student_ga_jun_doc <- baseline_vector[11] + (intervention_vector[20] * baseline_vector[24])
    
    student_deploy[5] <- ((student_ga_nurse * baseline_vector[22]) - ga_patients/baseline_vector[14])/baseline_vector[25] * 10000
    student_deploy[9] <- ((student_ga_jun_doc * baseline_vector[23]) - ga_patients/baseline_vector[18])/baseline_vector[25] * 10000
    
    student_deploy_gains[5] <- (intervention_vector[19] * baseline_vector[24])
    student_deploy_gains[9] <- (intervention_vector[20] * baseline_vector[24])
    
    
    ## international doctors ##
    
    int_doctors <- baseline
    int_doctors_gains <- baseline_gains
    
    int_doctor_ga_jun <- baseline_vector[11] + (intervention_vector[21] * baseline_vector[24])
    
    int_doctors[9] <- ((int_doctor_ga_jun * baseline_vector[23]) - ga_patients/baseline_vector[18])/baseline_vector[25] * 10000
    
    int_doctors_gains[9] <- (intervention_vector[21] * baseline_vector[24])
    
    ## new vents ##
    
    new_vents <- baseline
    new_vents_gains <- baseline_gains
    
    new_vents_num <- baseline_vector[6] + intervention_vector[22]
    
    new_vents[3] <- (new_vents_num - ventilator_need)/baseline_vector[25] * 10000
    
    new_vents_gains[3] <- intervention_vector[22]
    
    ## merge the gains vectors ##
    
    gains_df <- rbind.data.frame(field_hosp_gains, private_res_gains, theatres_gains,
                                 convert_beds_gains, upskill_gains, return_gains,
                                 student_deploy_gains, int_doctors_gains, new_vents_gains)
    
    spare_capacity_df <- rbind.data.frame(baseline,cancel_electives, nice_guide,field_hosp,
                                          private_res, theatres, convert_beds,
                                          upskill, return_staff, student_deploy,
                                          int_doctors, new_vents)
    colnames(spare_capacity_df) <- c("CC_beds","GA_beds","Ventilators","CC_nurses",
                                     "GA_nurses","CC_senior_doctors","CC_junior_doctors",
                                     "GA_senior_doctors","GA_junior_doctors")
    spare_capacity_df$intervention <- c("Baseline","Cancel_electives","NICE_guide","Field_Hospitals",
                                        "Private_resources","Convert_theatres","Convert_beds",
                                        "Upskill_staff","Returning_staff","Student_deployment",
                                        "International_doctors","New_Ventilators")
    
    
    if(length(combination_vector) > 0){
      if(1 %in% combination_vector){
        
        combo_rows <- combination_vector[-(which(combination_vector == 1))] - 2
        merged_rows <- gains_df[combo_rows,]
        
        combo_effects <- colSums(merged_rows)
        combo_and_baseline <- combo_effects + c(baseline_vector[1], baseline_vector[2], baseline_vector[6],
                                                baseline_vector[7], baseline_vector[8], baseline_vector[9],
                                                baseline_vector[10], baseline_vector[12], baseline_vector[11])
        
        combo <- baseline
        
        combo[1] <- ((combo_and_baseline[1] - cc_patients_electives) / baseline_vector[25]) * 10000
        combo[2] <- ((combo_and_baseline[2] - ga_patients_electives) / baseline_vector[25]) * 10000
        combo[3] <- ((combo_and_baseline[3] - vent_need_electives) / baseline_vector[25]) * 10000
        combo[4] <- (((combo_and_baseline[4] * baseline_vector[22]) - cc_patients_electives/baseline_vector[13]) / baseline_vector[25]) * 10000
        combo[5] <- (((combo_and_baseline[5] * baseline_vector[22]) - ga_patients_electives/baseline_vector[14]) / baseline_vector[25]) * 10000
        combo[6] <- (((combo_and_baseline[6] * baseline_vector[23]) - cc_patients_electives/baseline_vector[15]) / baseline_vector[25]) * 10000
        combo[7] <- (((combo_and_baseline[7] * baseline_vector[23]) - cc_patients_electives/baseline_vector[16]) / baseline_vector[25]) * 10000
        combo[8] <- (((combo_and_baseline[8] * baseline_vector[23]) - ga_patients_electives/baseline_vector[17]) / baseline_vector[25]) * 10000
        combo[9] <- (((combo_and_baseline[9] * baseline_vector[23]) - ga_patients_electives/baseline_vector[18]) / baseline_vector[25]) * 10000
        
        combo <- as.data.frame(t(combo))
        colnames(combo) <- c("CC_beds","GA_beds","Ventilators","CC_nurses",
                             "GA_nurses","CC_senior_doctors","CC_junior_doctors",
                             "GA_senior_doctors","GA_junior_doctors")
        combo$intervention <-  "Combination"
        
        spare_capacity_df <- rbind.data.frame(spare_capacity_df, combo)
      }else if(2 %in% combination_vector){
        combo_rows <- combination_vector[-(which(combination_vector == 2))] - 2
        merged_rows <- gains_df[combo_rows,]
        
        combo_effects <- colSums(merged_rows)
        combo_and_baseline <- combo_effects + c(baseline_vector[1], baseline_vector[2], baseline_vector[6],
                                                baseline_vector[7], baseline_vector[8], baseline_vector[9],
                                                baseline_vector[10], baseline_vector[12], baseline_vector[11])
        
        combo <- baseline
        
        combo[1] <- ((combo_and_baseline[1] - cc_patients_nice) / baseline_vector[25]) * 10000
        combo[2] <- ((combo_and_baseline[2] - ga_patients) / baseline_vector[25]) * 10000
        combo[3] <- ((combo_and_baseline[3] - vent_need_nice) / baseline_vector[25]) * 10000
        combo[4] <- (((combo_and_baseline[4] * baseline_vector[22]) - cc_patients_nice/baseline_vector[13]) / baseline_vector[25]) * 10000
        combo[5] <- (((combo_and_baseline[5] * baseline_vector[22]) - ga_patients/baseline_vector[14]) / baseline_vector[25]) * 10000
        combo[6] <- (((combo_and_baseline[6] * baseline_vector[23]) - cc_patients_nice/baseline_vector[15]) / baseline_vector[25]) * 10000
        combo[7] <- (((combo_and_baseline[7] * baseline_vector[23]) - cc_patients_nice/baseline_vector[16]) / baseline_vector[25]) * 10000
        combo[8] <- (((combo_and_baseline[8] * baseline_vector[23]) - ga_patients/baseline_vector[17]) / baseline_vector[25]) * 10000
        combo[9] <- (((combo_and_baseline[9] * baseline_vector[23]) - ga_patients/baseline_vector[18]) / baseline_vector[25]) * 10000
        
        combo <- as.data.frame(t(combo))
        colnames(combo) <- c("CC_beds","GA_beds","Ventilators","CC_nurses",
                             "GA_nurses","CC_senior_doctors","CC_junior_doctors",
                             "GA_senior_doctors","GA_junior_doctors")
        combo$intervention <-  "Combination"
        
        spare_capacity_df <- rbind.data.frame(spare_capacity_df, combo)
        
        
        
      }else{
        
        combo_rows <- combination_vector - 2
        merged_rows <- gains_df[combo_rows,]
        
        combo_effects <- colSums(merged_rows)
        combo_and_baseline <- combo_effects + c(baseline_vector[1], baseline_vector[2], baseline_vector[6],
                                                baseline_vector[7], baseline_vector[8], baseline_vector[9],
                                                baseline_vector[10], baseline_vector[12], baseline_vector[11])
        
        combo <- baseline
        
        combo[1] <- ((combo_and_baseline[1] - cc_patients) / baseline_vector[25]) * 10000
        combo[2] <- ((combo_and_baseline[2] - ga_patients) / baseline_vector[25]) * 10000
        combo[3] <- ((combo_and_baseline[3] - ventilator_need) / baseline_vector[25]) * 10000
        combo[4] <- (((combo_and_baseline[4] * baseline_vector[22]) - cc_patients/baseline_vector[13]) / baseline_vector[25]) * 10000
        combo[5] <- (((combo_and_baseline[5] * baseline_vector[22]) - ga_patients/baseline_vector[14]) / baseline_vector[25]) * 10000
        combo[6] <- (((combo_and_baseline[6] * baseline_vector[23]) - cc_patients/baseline_vector[15]) / baseline_vector[25]) * 10000
        combo[7] <- (((combo_and_baseline[7] * baseline_vector[23]) - cc_patients/baseline_vector[16]) / baseline_vector[25]) * 10000
        combo[8] <- (((combo_and_baseline[8] * baseline_vector[23]) - ga_patients/baseline_vector[17]) / baseline_vector[25]) * 10000
        combo[9] <- (((combo_and_baseline[9] * baseline_vector[23]) - ga_patients/baseline_vector[18]) / baseline_vector[25]) * 10000
        
        combo <- as.data.frame(t(combo))
        colnames(combo) <- c("CC_beds","GA_beds","Ventilators","CC_nurses",
                             "GA_nurses","CC_senior_doctors","CC_junior_doctors",
                             "GA_senior_doctors","GA_junior_doctors")
        combo$intervention <- "Combination"
        
        spare_capacity_df <- rbind.data.frame(spare_capacity_df, combo)
        
      }
      
      spare_capacity_df$COVID_CC <- covid_cc_patients[k]
      spare_capacity_df$staffing_deficit <- rowSums(spare_capacity_df[,4:9] < 0)
      spare_capacity_df$equipment_deficit <- rowSums(spare_capacity_df[,1:3] < 0)
      
      
    }
    
    spare_capacity <- rbind.data.frame(spare_capacity, spare_capacity_df)
    
    
  }
  
  spare_capacity <- spare_capacity[,c(10,1:9,11:13)]
  return(spare_capacity)
  
}

spazza_capac <- hospital_capacity_electives(baseline_info, intervention_vector = intervention_info,
                                            covid_cc_patients = seq(0,10000,1), combination_vector = c(3,4,5,6,7,8,9))



###############################################################################
## Lets look into how we can use a heatmap to map varying capacity for ########
## electives ##################################################################
###############################################################################

melted_df <- spazza_capac[,c(1,11,12,13)]
melted_df <- melted_df[-c(which(melted_df$intervention %in% c("NICE_guide","Cancel_electives"))),]
melted_df$staffing_deficit <- as.character(melted_df$staffing_deficit)
melted_df$equipment_deficit <- as.character(melted_df$equipment_deficit)

tile_map_staffing <- ggplot(data = melted_df, aes(x = melted_df$COVID_CC, y = melted_df$intervention,
                                         z = melted_df$staffing_deficit)) + 
  geom_raster(aes(fill = melted_df$staffing_deficit), hjust = 1, vjust = 0.5) + 
  xlab("COVID19 CC patients") + ylab("Intervention") + labs(fill = "Staffing catergory deficits") + 
  scale_fill_manual(values = c("green","yellow","darkorange1","red","red3")) + geom_vline(xintercept = 3618 )
tile_map_staffing

tile_map_equip <- ggplot(data = melted_df, aes(x = melted_df$COVID_CC, y = melted_df$intervention,
                                                  z = melted_df$equipment_deficit)) + 
  geom_raster(aes(fill = melted_df$equipment_deficit), hjust = 1, vjust = 0.5) + 
  xlab("COVID19 CC patients") + ylab("Intervention") + labs(fill = "Equipment catergory deficits") + 
  scale_fill_manual(values = c("green","yellow","red")) + geom_vline(xintercept = 3618 )
tile_map_equip


staff_beds <- ggarrange(tile_map_staffing, tile_map_equip,
                        ncol = 2, nrow = 1)

staff_beds

## What goes missing in the equipment? ##


########################################################################################
## Lets look at how increasing the upskilling affects the different capacity scenarios #
## for staff ###########################################################################

upskill_alteration <- function(max_staff_shift, baseline_info, intervention_info,
                               covid_cc_patients){
  
  # Max staff shift the max percent (as integer) for upskilling
  
  
  mammoth_output <- NULL
  staff_percents <- seq(0,max_staff_shift,0.1)
  for(k in 1:length(staff_percents)){
    print((paste("On staff percent:",staff_percents[k])))
    ## CC nurse gain 
    intervention_info[15] <- baseline_info[8] * (staff_percents[k] / 100)
    ## CC Jun doc gain
    intervention_info[16] <- baseline_info[11] * (staff_percents[k] / 100)
    
    current_percent_data <- hospital_capacity_electives(baseline_info, intervention_vector = intervention_info,
                                                        covid_cc_patients = covid_cc_patients,
                                                        combination_vector = c(3,4,5,6,7,8,9))
    current_percent_data$upskill_percent <- rep(staff_percents[k], nrow(current_percent_data))
    mammoth_output <- rbind(mammoth_output, current_percent_data)
    
  }
  
  upskill_affect_only <- mammoth_output[mammoth_output$intervention == "Upskill_staff",]
  
  return(upskill_affect_only)
  
}


upskill_data <- upskill_alteration(20,baseline_info = baseline_info,
                                   intervention_info = intervention_info,
                                   covid_cc_patients = seq(0,5000,10))





upskill_heat_map_cc_nurses <- ggplot(data = upskill_data, aes(x = COVID_CC,
                                                              y = upskill_percent,
                                                              z = CC_nurses)) +
  geom_tile(aes(fill = CC_nurses)) +scale_fill_viridis() 
upskill_heat_map_cc_nurses

upskill_heat_map_ga_nurses <- ggplot(data = upskill_data, aes(x = COVID_CC,
                                                              y = upskill_percent,
                                                              z = GA_nurses)) +
  geom_tile(aes(fill = GA_nurses)) +scale_fill_viridis() 
upskill_heat_map_ga_nurses

upskill_heat_map_cc_doc <- ggplot(data = upskill_data, aes(x = COVID_CC,
                                                              y = upskill_percent,
                                                              z = CC_junior_doctors)) +
  geom_tile(aes(fill = CC_junior_doctors)) +scale_fill_viridis() 
upskill_heat_map_cc_doc

upskill_heat_map_ga_doc <- ggplot(data = upskill_data, aes(x = COVID_CC,
                                                              y = upskill_percent,
                                                              z = GA_junior_doctors)) +
  geom_tile(aes(fill = GA_junior_doctors)) +scale_fill_viridis() 
upskill_heat_map_ga_doc

upskill_effect <- ggarrange(upskill_heat_map_cc_nurses, upskill_heat_map_cc_doc,
                            upskill_heat_map_ga_nurses, upskill_heat_map_ga_doc,
                            ncol = 2, nrow = 2)

upskill_effect

## look just at 0-10 percent changes ##

upskill_data_0_10 <- upskill_data[upskill_data$upskill_percent <= 10.0,]

upskill_heat_map_cc_nurses <- ggplot(data = upskill_data_0_10, aes(x = COVID_CC,
                                                              y = upskill_percent,
                                                              z = CC_nurses)) +
  geom_tile(aes(fill = CC_nurses)) +scale_fill_viridis() 
upskill_heat_map_cc_nurses

upskill_heat_map_ga_nurses <- ggplot(data = upskill_data_0_10, aes(x = COVID_CC,
                                                              y = upskill_percent,
                                                              z = GA_nurses)) +
  geom_tile(aes(fill = GA_nurses)) +scale_fill_viridis() 
upskill_heat_map_ga_nurses

upskill_heat_map_cc_doc <- ggplot(data = upskill_data_0_10, aes(x = COVID_CC,
                                                           y = upskill_percent,
                                                           z = CC_junior_doctors)) +
  geom_tile(aes(fill = CC_junior_doctors)) +scale_fill_viridis() 
upskill_heat_map_cc_doc

upskill_heat_map_ga_doc <- ggplot(data = upskill_data_0_10, aes(x = COVID_CC,
                                                           y = upskill_percent,
                                                           z = GA_junior_doctors)) +
  geom_tile(aes(fill = GA_junior_doctors)) +scale_fill_viridis() 
upskill_heat_map_ga_doc

upskill_effect <- ggarrange(upskill_heat_map_cc_nurses, upskill_heat_map_cc_doc,
                            upskill_heat_map_ga_nurses, upskill_heat_map_ga_doc,
                            ncol = 2, nrow = 2)

















