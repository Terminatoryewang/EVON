# Re-organize census data

library(tidyverse)

setwd("~/Documents/School/CSCI6409/Project")

data = read.csv("Data/Census\ Profile/98-401-X2021013_English_CSV_data.csv")

data <- data %>%
  filter(substr(GEO_NAME, 1, 1) %in% list('K','L','M','N','P')) %>%
  select(-CENSUS_YEAR, -DGUID, -ALT_GEO_CODE, -GEO_LEVEL, -TNR_SF, -TNR_LF, -DATA_QUALITY_FLAG, -CHARACTERISTIC_NOTE, 
         -SYMBOL, -SYMBOL.1, -SYMBOL.2, -SYMBOL.3, -SYMBOL.4, -SYMBOL.5) %>%
  filter(CHARACTERISTIC_ID %in% list(1,4,6,7,8,16,17,18,19,20,21,22,23,24,39,40,
                                     42,43,44,45,46,47,48,49,57,59,66,128,130,143,144,
                                     156,158,159,160,161,162,163,164,165,166,167,168,
                                     243,244,1403,1410,1415,1416,1417,1419,1420,1433,
                                     1466,1467,1523,1526,1528,1529,1537,1975,1976,1984,1985,
                                     2015,2016,2018,2024,2095,2097,2100,2109,2117,2121,2127,2132,2140,2143,
                                     2149,2155,2228,2229,2230,2232,2233,2234,2235,2236,2240,2245,
                                     2249,2250,2251,2252,2253,2254,2255,2256,2257,2258,
                                     2262,2263,2264,2265,2266,2267,2268,2269,2270,
                                     2271,2272,2273,2274,2275,2276,2277,2278,2279,2280,2281,
                                     2594,2595,2596,2597,
                                     2599,2600,2601,2602,
                                     2605,2606,2607,2608,2609,2610,
                                     2612,2613,2614,2615,2616,
                                     2618,2619,2620,2621,2622,2623)) %>%
  mutate(value = ifelse(CHARACTERISTIC_ID==8, C2_COUNT_MEN., C1_COUNT_TOTAL)) %>%
  select(GEO_NAME, CHARACTERISTIC_ID, CHARACTERISTIC_NAME, value)

field_names = tibble(
  CHARACTERISTIC_ID = c(1,4,6,7,8,16,17,18,19,20,21,22,23,24,39,40,
                           42,43,44,45,46,47,48,49,57,59,66,128,130,143,144,
                           156,158,159,160,161,162,163,164,165,166,167,168,
                           243,244,1403,1410,1415,1416,1417,1419,1420,1433,
                           1466,1467,1523,1526,1528,1529,1537,1975,1976,1984,1985,
                           2015,2016,2018,2024,2095,2097,2100,2109,2117,2121,2127,2132,2140,2143,
                           2149,2155,2228,2229,2230,2232,2233,2234,2235,2236,2240,2245,
                           2249,2250,2251,2252,2253,2254,2255,2256,2257,2258,
                           2262,2263,2264,2265,2266,2267,2268,2269,2270,
                           2271,2272,2273,2274,2275,2276,2277,2278,2279,2280,2281,
                           2594,2595,2596,2597,
                           2599,2600,2601,2602,
                           2605,2606,2607,2608,2609,2610,
                           2612,2613,2614,2615,2616,
                           2618,2619,2620,2621,2622,2623),
  char_col_name = c("total_pop","total_dwellings","pop_density","land_area","num_male",
                       "age_25","age_30","age_35","age_40","age_45","age_50","age_55","age_60","age_65_up","age_mean","age_med",
                       "homes_detached_house","homes_semidetached_house","homes_rowhouse","homes_duplex_apt","homes_lowrise_apt",
                       "homes_highrise_apt","homes_other_stationary","homes_mobile",
                       "avg_ppl_household","married_ppl","single_ppl",
                       "avg_total_income","avg_aftertax_income","med_fulltime_income","avg_fulltime_income",
                       "income_none","income_under_10k","income_10k","income_20k","income_30k","income_40k","income_50k","income_60k",
                       "income_70k","income_80k","income_90k","income_100k_up",
                       "med_total_household_income","med_aftertax_household_income",
                       "indigenous_ppl","nonindigenous_ppl",
                       "home_owner","home_renter","home_gov_or_ind_band",
                       "condo","non_condo","avg_rooms_home",
                       "home_cost_under_30pct","home_cost_over_30pct",
                       "can_citizen_ppl","non_citizen_ppl",
                       "non_immigrant_ppl","immigrant_ppl","non_perm_res_ppl",
                       "no_move_last_yr","moved_last_yr","no_move_5yrs","moved_last_5yrs",
                       "school_no_hs","school_hs","school_college","school_uni_degree",
                       "edu_field_education","edu_field_arts_comms","edu_field_humanities","edu_field_socsci_law","edu_field_bus_admin",
                       "edu_field_science","edu_field_math_cs","edu_field_engin_arch","edu_field_agri_res_env","edu_field_health",
                       "edu_field_pers_protect_transp","edu_field_other",
                       "workforce_participation_rate","workforce_employment_rate","workforce_unemployment_rate",
                       "work_lastyear_didnotwork","work_lastyear_worked","work_lastyear_fulltime","work_lastyear_parttime","work_lastyear_avg_weeks",
                       "worktype_employee","worktype_selfemp",
                       "occup_cat_snr_mgmt","occup_cat_busfin","occup_cat_science","occup_cat_health","occup_cat_edu_law_socserv","occup_cat_arts_rec",
                       "occup_cat_sales_serv","occup_cat_trades_transp","occup_cat_natres_agr","occup_cat_manuf_util",
                       "occup_ind_agr_forest","occup_ind_mine_og","occup_ind_util","occup_ind_constr","occup_ind_manuf","occup_ind_wholesale_trd",
                       "occup_ind_retail_trd","occup_ind_transp_warehs","occup_ind_info_culture","occup_ind_fin_insure","occup_ind_realestate",
                       "occup_ind_prof_sci_tech_serv","occup_ind_mgmt","occup_ind_admsupport_wastemgmt","occup_ind_edu","occup_ind_health_socasst",
                       "occup_ind_arts_ent_rec","occup_ind_accom_food_svc","occup_ind_other","occup_ind_pubadmin",
                       "work_loc_home","work_loc_foreign","work_loc_notfixed","work_loc_workplace",
                       "commute_same_subdiv","commute_same_div","commute_same_prov","commute_diff_prov",
                       "commute_transp_cardriver","commute_transp_carpass","commute_transp_pubtrans","commute_transp_walk",
                       "commute_transp_bike","commute_transp_other",
                       "commute_time_under15","commute_time_15","commute_time_30","commute_time_45","commute_time_over60",
                       "commute_start_5am","commute_start_6am","commute_start_7am","commute_start_8am","commute_start_9am","commute_start_noon")
)

new_data <- inner_join(data, field_names, by='CHARACTERISTIC_ID') %>%
  select(GEO_NAME, char_col_name, value) %>%
  pivot_wider(id_cols = GEO_NAME, names_from = char_col_name, values_from = value)


ev_data = read.csv("Data/ontario_evs_by_fsa_2023-03-31.csv")

new_data <- inner_join(new_data, ev_data, by=join_by(GEO_NAME==FSA))

new_data <- mutate(new_data,
  num_male = num_male/total_pop,
  
  age_25 = age_25/total_pop,
  age_30 = age_30/total_pop,
  age_35 = age_35/total_pop,
  age_40 = age_40/total_pop,
  age_45 = age_45/total_pop,
  age_50 = age_50/total_pop,
  age_55 = age_55/total_pop,
  age_60 = age_60/total_pop,
  age_65_up = age_65_up/total_pop,
  
  homes_detached_house = homes_detached_house/total_dwellings,
  homes_semidetached_house = homes_semidetached_house/total_dwellings,
  homes_rowhouse = homes_rowhouse/total_dwellings,
  homes_duplex_apt = homes_duplex_apt/total_dwellings,
  homes_lowrise_apt = homes_lowrise_apt/total_dwellings,
  
  homes_highrise_apt = homes_highrise_apt/total_dwellings,
  homes_other_stationary = homes_other_stationary/total_dwellings,
  homes_mobile = homes_mobile/total_dwellings,
  
  married_ppl = married_ppl/total_pop,
  single_ppl = single_ppl/total_pop,
  
  income_none = income_none/total_pop,
  income_under_10k = income_under_10k/total_pop,
  income_10k = income_10k/total_pop,
  income_20k = income_20k/total_pop,
  income_30k = income_30k/total_pop,
  income_40k = income_40k/total_pop,
  income_50k = income_50k/total_pop,
  income_60k = income_60k/total_pop,
  income_70k = income_70k/total_pop,
  income_80k = income_80k/total_pop,
  income_90k = income_90k/total_pop,
  income_100k_up = income_100k_up/total_pop,
  
  indigenous_ppl = indigenous_ppl/total_pop,
  nonindigenous_ppl = nonindigenous_ppl/total_pop,
  
  home_owner = home_owner/total_pop,
  home_renter = home_renter/total_pop,
  home_gov_or_ind_band = home_gov_or_ind_band/total_pop,
  
  condo = condo/total_dwellings,
  non_condo = non_condo/total_dwellings,
  
  home_cost_under_30pct = home_cost_under_30pct/total_pop,
  home_cost_over_30pct = home_cost_over_30pct/total_pop,
  
  can_citizen_ppl = can_citizen_ppl/total_pop,
  non_citizen_ppl = non_citizen_ppl/total_pop,
  non_immigrant_ppl = non_immigrant_ppl/total_pop,
  immigrant_ppl = immigrant_ppl/total_pop,
  non_perm_res_ppl = non_perm_res_ppl/total_pop,
  
  no_move_last_yr = no_move_last_yr/total_pop,
  moved_last_yr = moved_last_yr/total_pop,
  no_move_5yrs = no_move_5yrs/total_pop,
  moved_last_5yrs = moved_last_5yrs/total_pop,
  
  school_no_hs = school_no_hs/total_pop,
  school_hs = school_hs/total_pop,
  school_college = school_college/total_pop,
  school_uni_degree = school_uni_degree/total_pop,
  
  edu_field_education = edu_field_education/total_pop,
  edu_field_arts_comms = edu_field_arts_comms/total_pop,
  edu_field_humanities = edu_field_humanities/total_pop,
  edu_field_socsci_law = edu_field_socsci_law/total_pop,
  edu_field_bus_admin = edu_field_bus_admin/total_pop,
  edu_field_science = edu_field_science/total_pop,
  edu_field_math_cs = edu_field_math_cs/total_pop,
  edu_field_engin_arch = edu_field_engin_arch/total_pop,
  edu_field_agri_res_env = edu_field_agri_res_env/total_pop,
  edu_field_health = edu_field_health/total_pop,
  edu_field_pers_protect_transp = edu_field_pers_protect_transp/total_pop,
  edu_field_other = edu_field_other/total_pop,
  
  work_lastyear_didnotwork = work_lastyear_didnotwork/total_pop,
  work_lastyear_worked = work_lastyear_worked/total_pop,
  work_lastyear_fulltime = work_lastyear_fulltime/total_pop,
  work_lastyear_parttime = work_lastyear_parttime/total_pop,
  work_lastyear_avg_weeks = work_lastyear_avg_weeks/total_pop,
  
  worktype_employee = worktype_employee/total_pop,
  worktype_selfemp = worktype_selfemp/total_pop,
  
  occup_cat_snr_mgmt = occup_cat_snr_mgmt/total_pop,
  occup_cat_busfin = occup_cat_busfin/total_pop,
  occup_cat_science = occup_cat_science/total_pop,
  occup_cat_health = occup_cat_health/total_pop,
  occup_cat_edu_law_socserv = occup_cat_edu_law_socserv/total_pop,
  occup_cat_arts_rec = occup_cat_arts_rec/total_pop,
  occup_cat_sales_serv = occup_cat_sales_serv/total_pop,
  occup_cat_trades_transp = occup_cat_trades_transp/total_pop,
  occup_cat_natres_agr = occup_cat_natres_agr/total_pop,
  occup_cat_manuf_util = occup_cat_manuf_util/total_pop,
  
  occup_ind_agr_forest = occup_ind_agr_forest/total_pop,
  occup_ind_mine_og = occup_ind_mine_og/total_pop,
  occup_ind_util = occup_ind_util/total_pop,
  occup_ind_constr = occup_ind_constr/total_pop,
  occup_ind_manuf = occup_ind_manuf/total_pop,
  occup_ind_wholesale_trd = occup_ind_wholesale_trd/total_pop,
  occup_ind_retail_trd = occup_ind_retail_trd/total_pop,
  occup_ind_transp_warehs = occup_ind_transp_warehs/total_pop,
  occup_ind_info_culture = occup_ind_info_culture/total_pop,
  occup_ind_fin_insure = occup_ind_fin_insure/total_pop,
  occup_ind_realestate = occup_ind_realestate/total_pop,
  occup_ind_prof_sci_tech_serv = occup_ind_prof_sci_tech_serv/total_pop,
  occup_ind_mgmt = occup_ind_mgmt/total_pop,
  occup_ind_admsupport_wastemgmt = occup_ind_admsupport_wastemgmt/total_pop,
  occup_ind_edu = occup_ind_edu/total_pop,
  occup_ind_health_socasst = occup_ind_health_socasst/total_pop,
  occup_ind_arts_ent_rec = occup_ind_arts_ent_rec/total_pop,
  occup_ind_accom_food_svc = occup_ind_accom_food_svc/total_pop,
  occup_ind_other = occup_ind_other/total_pop,
  occup_ind_pubadmin = occup_ind_pubadmin/total_pop,
  
  work_loc_home = work_loc_home/total_pop,
  work_loc_foreign = work_loc_foreign/total_pop,
  work_loc_notfixed = work_loc_notfixed/total_pop,
  work_loc_workplace = work_loc_workplace/total_pop,
  
  commute_same_subdiv = commute_same_subdiv/total_pop,
  commute_same_div = commute_same_div/total_pop,
  commute_same_prov = commute_same_prov/total_pop,
  commute_diff_prov = commute_diff_prov/total_pop,
  
  commute_transp_cardriver = commute_transp_cardriver/total_pop,
  commute_transp_carpass = commute_transp_carpass/total_pop,
  commute_transp_pubtrans = commute_transp_pubtrans/total_pop,
  commute_transp_walk = commute_transp_walk/total_pop,
  
  commute_transp_bike = commute_transp_bike/total_pop,
  commute_transp_other = commute_transp_other/total_pop,
  
  commute_time_under15 = commute_time_under15/total_pop,
  commute_time_15 = commute_time_15/total_pop,
  commute_time_30 = commute_time_30/total_pop,
  commute_time_45 = commute_time_45/total_pop,
  commute_time_over60 = commute_time_over60/total_pop,
  
  commute_start_5am = commute_start_5am/total_pop,
  commute_start_6am = commute_start_6am/total_pop,
  commute_start_7am = commute_start_7am/total_pop,
  commute_start_8am = commute_start_8am/total_pop,
  commute_start_9am = commute_start_9am/total_pop,
  commute_start_noon = commute_start_noon/total_pop,
  BEV = (BEV/total_pop)*10000,
  PHEV = (PHEV/total_pop)*10000,
  TotalEV = (TotalEV/total_pop)*10000
)

write.csv(new_data, "Data/fulldata_norm.csv")