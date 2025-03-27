library(tidyverse)
rm(list=setdiff(ls(), c("newdata")))
setwd('C:/Users/gxcoppol/New York State Office of Information Technology Services/LMAS - PWS/PWS_Prioritization')


temp<-newdata %>% 
  filter(PWS=="YES") %>% 
  select(LAKE_HISTORY_ID,LOCATION_PWL_ID) %>% distinct()
att<-read.csv("2022.Data/2022_lmas_evaluations.csv")
att<-att %>% 
  rename(LOCATION_PWL_ID=seg_id) %>% distinct()
att<-merge(temp,att,by=c('LOCATION_PWL_ID'),all.x=TRUE)

#remove NYC reservoirs
#Remove NYC reservoirs 
att<-att %>% filter(!(LAKE_HISTORY_ID %in%
                        c('1302WES0067','1302BOY0076','1302EAS0089','1302BOG0086',
                          '1302MID0062','1302DIV0083','1302CRO0059',
                          '1302TIT0103','1302CRO0109','1302AMA0050','1302MUS0044A','1302NEW0044',
                          '1702KEN1063','1302GIL0061','1302GLE0074','1302KIR0052','1404CAN0402A',
                          '1403PEP0358A','1306RON0815A','1402NEV0058B','1307ASH0848','1202SCH0638A',
                          '1301JER1042','1702HIL1052','1501STA1011','1301TIO0152','1301SUM0193',
                          '1311THO0274','0905CRA0309','1307KIN0837A','1702BAR1108',
                          '1000CHA0001','0300ONT0000')))

rm(list=setdiff(ls(), c("newdata",'att','temp')))
nopwl<-att %>% filter(is.na(LOCATION_PWL_ID))
att<-att %>% filter(!is.na(LOCATION_PWL_ID))




# assessed PWS -----------------------------------

table1_expanded<-att %>% 
  filter(n_years_sampled >=2| n_samples_collected>=6) %>% 
  filter(use== 'source_of_water_supply',
         within_period=='TRUE') %>% distinct()

table1_excursions<-table1_expanded %>% filter(n_exceedances>=1) %>%  
  select(LAKE_HISTORY_ID,LOCATION_PWL_ID, parameter) %>% 
  group_by(LAKE_HISTORY_ID, LOCATION_PWL_ID) %>%
  summarise(excursion_parms = toString(parameter)) %>%
  ungroup() %>%
  mutate(excursions = 'Y') %>% 
  distinct()

table1a_assessment_results<-table1_expanded %>% filter(n_exceedances==0, !LAKE_HISTORY_ID %in% table1_excursions$LAKE_HISTORY_ID) %>%  
  select(LAKE_HISTORY_ID,LOCATION_PWL_ID) %>%
  distinct() %>% 
  mutate(excursion_parms = NA,
         excursions = 'N') %>% 
  bind_rows(table1_excursions)

doh<-read.csv("data.requests/PWS_IDs.csv")
doh<-doh %>%
  select(LOCATION_PWL_ID,LAKE_HISTORY_ID,PWS.ID) %>% distinct() 

table1b_PWS_info<-merge(table1a_assessment_results, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x = T)



# unassessed PWS w/ data --------------------------------

#remove impaired
remove<-table1a_assessment_results %>% select(LAKE_HISTORY_ID,LOCATION_PWL_ID) %>% distinct() %>% mutate(remove="yes")
table2_expanded<-merge(remove,att,by=c('LAKE_HISTORY_ID','LOCATION_PWL_ID'),all=TRUE)
table2_expanded<-table2_expanded %>% filter(is.na(remove)) %>% select(-remove) %>% 
  #filter to !is.na records
  filter(!is.na(n_samples_collected),use!='fishing')


table2_excursions<-table2_expanded %>% filter(n_exceedances>=1) %>%  
  select(LAKE_HISTORY_ID,LOCATION_PWL_ID,within_period,use, parameter) %>% 
  group_by(LAKE_HISTORY_ID, LOCATION_PWL_ID,within_period) %>%
  summarise(excursion_parms = toString(parameter)) %>%
  ungroup() %>%
  mutate(within_period=ifelse(within_period=="TRUE","recent","old")) %>%  distinct() %>% 
  spread(within_period,excursion_parms) %>% 
  mutate(excursions = 'Y') %>% 
  distinct()

table2a_assessment_results<-table2_expanded %>% filter(n_exceedances==0, !LAKE_HISTORY_ID %in% table2_excursions$LAKE_HISTORY_ID) %>%  
  select(LAKE_HISTORY_ID,LOCATION_PWL_ID) %>%
  distinct() %>% 
  mutate(old = NA, recent = NA,
         excursions = 'N') %>% 
  bind_rows(table2_excursions)

doh<-read.csv("data.requests/PWS_IDs.csv")

table2b_PWS_info<-merge(table2a_assessment_results, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)



# stay calm output processing - violation_data tab ---------------------------
# 14 lakes from table 2a that have excursions in the last 10 years
# 2 lakes ('1003PAT0027','1104POR0152E') are absent from violation_data tab 
# one lake ('1301QUE0184A') not sampled in the last 10 years
within_period<- table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID

violation<-read.csv('2022_lmas_evaluations_violation_data.csv')


# general stats on violation_data tab
violation_summary<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
         year >= 2012,
         parameter %in% c('phosphorus','iron','manganese')) %>% 
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(max_prop = max(result)/threshold,
            mean_prop = mean(result)/threshold,
            n = n(),
            years_since_last_sample = 2023-max(year),
            total_years_sampled = n_distinct(year)) %>% 
  distinct()

# collapsed list of excursion parameters for each lake
param<- violation_summary %>%  group_by(LAKE_HISTORY_ID) %>% 
  summarise(excursion_parameters = toString(parameter))


# maximum median value for each exceedance parameter from violation_data
violation_med<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
         year >= 2012,
         parameter %in% c('phosphorus','iron','manganese')) %>% 
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(med_prop = median(result)/threshold,
            n = n()) %>% 
  distinct() %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  filter(med_prop == max(med_prop))





# phosphorus rank via raw dataset -----------------------
p_raw_dat<-newdata %>% mutate(year = as.numeric(substr(SAMPLE_DATE,1,4))) %>% 
  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
         year >= 2012,
         CHARACTERISTIC_NAME == 'PHOSPHORUS, TOTAL',
         INFORMATION_TYPE == 'OW',
         !is.na(RSLT_RESULT_VALUE))

# for some reason only could find data for 10/11 lakes in the raw dataset when filtering on my own
wo<- table2a_assessment_results %>% filter(!is.na(recent),
                                           !LAKE_HISTORY_ID %in% p_raw_dat$LAKE_HISTORY_ID)

p_summary<-p_raw_dat %>% group_by(LAKE_HISTORY_ID) %>% summarise(med_rw_phos_prop = median(RSLT_RESULT_VALUE)/.02,
                                                                 n = n()) %>% 
  arrange(desc(med_rw_phos_prop)) %>% 
  mutate(rank_rw_phos = row_number()) %>% 
  left_join(param, by = 'LAKE_HISTORY_ID')



# top vs bottom
# only 7 waterbodies have open water phosphorus samples in the last 10 years
#p_summary_tb<-p_raw_dat %>% group_by(LAKE_HISTORY_ID, INFORMATION_TYPE) %>% summarise(med = median(RSLT_RESULT_VALUE)) %>% 
#  spread(INFORMATION_TYPE, med) %>% 
# mutate(diff= BS-OW)

  

  
# phosphorus rank via stay calm output -----------------------

p_sc_summary<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
         year >= 2012,
         parameter == 'phosphorus') %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  summarise(med_sc_phos_prop = median(result)/threshold,
            n = n()) %>% 
  distinct() %>% 
  arrange(desc(med_sc_phos_prop))

p_sc_summary$rank_sc_phos<-c(1:7)

# there are only 7 lakes with phosphorus data in the last 10 years in the stay calm output
v_test<-table2a_assessment_results %>% filter(!is.na(recent), !LAKE_HISTORY_ID %in% p_sc_summary$LAKE_HISTORY_ID)



# rank based on index score ----------------------------

# format exceedance_summary dataset to have one row per site for phosphorus in order to sum total samples for each lake
att_phosphorus<- att %>% filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
                                parameter == 'phosphorus',
                                within_period == 'TRUE',
                                use == 'primary_contact_recreation') %>% 
  select(-use) %>%
  mutate(parameter = 'phosphorus')


# format exceedance_summary dataset to have one row per site for rest of parameters in order to sum total samples for each lake
# sum samples and exceedances for each lake
att_bind<-att %>%  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
                          use == 'source_of_water_supply',
                          within_period == 'TRUE',
                          parameter %in% c('iron','manganese')) %>% 
  select(-use) %>% 
  bind_rows(att_phosphorus) %>% 
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(n_samples_collected = sum(n_samples_collected),
            n_years_sampled = sum(n_years_sampled),
            n_exceedances = sum(n_exceedances)) %>% 
  mutate(exceedance_rate = n_exceedances/n_samples_collected)
 
# combine exceedance rate with median values and arrange
score<- violation_med %>% left_join(att_bind, by = c('LAKE_HISTORY_ID','parameter')) %>% 
  distinct() %>% 
  mutate(med_multiparameter_prop = med_prop,
         score = med_prop * exceedance_rate) %>% 
  arrange(desc(score)) %>% 
  left_join(param, by = 'LAKE_HISTORY_ID')

score$rank_multiparameter<-c(1:11)


# combine all ranks to compare ------------------------

rank_table<- score %>% mutate(score_parameter = parameter) %>% 
  select(exceedance_rate, LAKE_HISTORY_ID, score, rank_multiparameter,med_multiparameter_prop,score_parameter) %>% 
  left_join(p_summary, by = 'LAKE_HISTORY_ID') %>% 
  left_join(p_sc_summary, by = 'LAKE_HISTORY_ID') %>% 
  select(LAKE_HISTORY_ID,
         score,
         exceedance_rate,
         med_multiparameter_prop,
         med_rw_phos_prop,
         med_sc_phos_prop,
         rank_multiparameter,
         rank_rw_phos,
         rank_sc_phos,
         score_parameter) %>% 
  left_join(param, by = 'LAKE_HISTORY_ID')


write.csv(rank_table, 'rank_table_within_period.csv', row.names = F, na = '')