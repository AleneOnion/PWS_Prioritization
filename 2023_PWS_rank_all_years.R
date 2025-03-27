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

att<-att %>% filter(!(LAKE_HISTORY_ID %in%
                        c('1302WES0067','1302BOY0076','1302EAS0089','1302BOG0086',
                          '1302MID0062','1302DIV0083','1302CRO0059',
                          '1302TIT0103','1302CRO0109','1302AMA0050','1302MUS0044A','1302NEW0044',
                          '1702KEN1063','1302GIL0061','1302GLE0074','1302KIR0052','1404CAN0402A',
                          '1403PEP0358A','1306RON0815A','1402NEV0058B','1307ASH0848','1202SCH0638A',
                          '1301JER1042','1702HIL1052',
                          # lakes removed for reasons described in the unassessed without data section
                          '1501STA1011','1301TIO0152','1301SUM0193',
                          '1311THO0274','0905CRA0309','1307KIN0837A','1307KIN0837','1307KIN0838A', '1301BRO0155',
                          '0402RUS0033','1702BAR1108','1000CHA0001','0300ONT0000',
                          '1005DOL0424','1301LUS0204',
                          # lakes removed for reasons described in the unassessed with data section
                          '0302SOD0096','0601OTS0404','0601OTS0404','0601OTS0404','0705SEN0369','0705SEN0369','0705SEN0369','1201MAR0570','1301POP0191','1308TAG0869',
                          '0705SEN0369','0705SEN0369','1301QUE0184A','1301STI0187A','1301STI0187A')))

rm(list=setdiff(ls(), c("newdata",'att')))
nopwl<-att %>% filter(is.na(LOCATION_PWL_ID))
att<-att %>% filter(!is.na(LOCATION_PWL_ID))


look<-c('1303UPP0223','1311DUA0290','1404STA0462','1202TAN0655B',
        '1306MIL0515','1201UWB0662','0503HOR0035','1101UWB1081')

look<-newdata %>% filter(LAKE_HISTORY_ID %in% c('1303UPP0223','1311DUA0290','1404STA0462','1202TAN0655B',
                                                '1306MIL0515','1201UWB0662','0503HOR0035','1101UWB1081'),
                         LOCATION_TYPE == 'CENTROID') %>% 
  select(LAKE_HISTORY_ID, LAKE_POND_NUMBER, LOCATION_X_COORDINATE, LOCATION_Y_COORDINATE) %>% distinct()
write.csv(look, 'no_SDWIS_ID.csv')
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

ranking<-read.csv("ranked_PWS_2023.csv")
table2a_assessment_results<-merge(table2a_assessment_results,ranking,by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'),all.x=TRUE)
table2a_assessment_results<-table2a_assessment_results %>% arrange(desc(score))

doh<-read.csv("data.requests/PWS_IDs.csv")

table2b_PWS_info<-merge(table2a_assessment_results, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)

# DOH prioritized waterbodies
DOH_prioritized_table2<-unique(table2b_PWS_info[!is.na(table2b_PWS_info$PWS.ID),]$LAKE_HISTORY_ID)





# stay calm output processing - violation_data tab ---------------------------
# 14 lakes from table 2a that have excursions in the last 10 years
# 2 lakes ('1003PAT0027','1104POR0152E') are absent from violation_data tab 
# one lake ('1301QUE0184A') not sampled in the last 10 years

within_period<- table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID
outside_period<-table2a_assessment_results[!is.na(table2a_assessment_results$old),]$LAKE_HISTORY_ID

violation<-read.csv('2022_lmas_evaluations_violation_data.csv')


# general stats on violation_data tab
violation_summary_within<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
         year >= 2013,
         parameter %in% c('phosphorus','iron','manganese')) %>% 
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(max_prop = max(result)/threshold,
            mean_prop = mean(result)/threshold,
            n = n(),
            years_since_last_sample = 2023-max(year),
            total_years_sampled = n_distinct(year)) %>% 
  distinct()%>% 
  mutate(within_period = 'TRUE') 

# general stats on violation_data tab
violation_summary<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% outside_period,
         year >= 2002,
         parameter %in% c('ammonia','chloride','fluoride','iron','manganese','phosphorus'),
         !is.na(threshold)) %>%
  mutate(result_standardized = result/threshold) %>% # have to standardize before grouping because thresholds are different for ammonia
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(max_prop = max(result_standardized),
            mean_prop = mean(result_standardized),
            n = n(),
            years_since_last_sample = 2023-max(year),
            total_years_sampled = n_distinct(year)) %>% 
  distinct() %>% 
  mutate(within_period = 'FALSE') %>% 
  filter(!LAKE_HISTORY_ID %in% violation_summary_within$LAKE_HISTORY_ID) %>% #filter afterwards because lakes in both periods cant be filtered out when >2002
  bind_rows(violation_summary_within)



# collapsed list of excursion parameters for each lake
#param<- violation_summary %>%  group_by(LAKE_HISTORY_ID, within_period) %>% 
#  summarise(excursion_parameters = toString(parameter))

param<-violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% violation_summary$LAKE_HISTORY_ID,
         year >= 2002,
         attaining_wqs == 'no',
         parameter %in% c('ammonia','chloride','fluoride','iron','manganese','phosphorus')) %>% 
  select(LAKE_HISTORY_ID, within_period, parameter) %>%
  distinct() %>% 
  group_by(LAKE_HISTORY_ID, within_period) %>% 
  summarise(excursion_parameters = toString(parameter)) %>% 
  mutate(within_period = as.character(within_period))


# general stats on violation_data tab
violation_med_within<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% table2a_assessment_results[!is.na(table2a_assessment_results$recent),]$LAKE_HISTORY_ID,
         year >= 2013,
         parameter %in% c('phosphorus','iron','manganese')) %>% 
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(med_prop = median(result)/threshold,
            n = n()) %>% 
  distinct()%>% 
  group_by(LAKE_HISTORY_ID) %>% 
  filter(med_prop == max(med_prop)) %>% 
  mutate(within_period = 'TRUE') 

# general stats on violation_data tab
violation_med<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% outside_period,
         year >= 2002,
         parameter %in% c('ammonia','chloride','fluoride','iron','manganese','phosphorus'),
         !is.na(threshold)) %>%
  mutate(result_standardized = result/threshold) %>% # have to standardize before grouping because thresholds are different for ammonia
  group_by(LAKE_HISTORY_ID, parameter) %>% 
  summarise(med_prop = median(result)/threshold,
            n = n()) %>% 
  distinct() %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  filter(med_prop == max(med_prop)) %>% 
  mutate(within_period = 'FALSE') %>% 
  filter(!LAKE_HISTORY_ID %in% violation_med_within$LAKE_HISTORY_ID) %>%  #filter afterwards because lakes in both periods cant be filtered out when >2002
  bind_rows(violation_med_within)




# rank based on index score ----------------------------

# format exceedance_summary dataset to have one row per site for phosphorus in order to sum total samples for each lake
att_phosphorus<- att %>% filter(LAKE_HISTORY_ID %in% violation_med$LAKE_HISTORY_ID,
                                parameter == 'phosphorus',
                                #within_period == 'TRUE',
                                use == 'primary_contact_recreation') %>% 
  select(-use) %>%
  mutate(parameter = 'phosphorus')


# format exceedance_summary dataset to have one row per site for rest of parameters in order to sum total samples for each lake
# sum samples and exceedances for each lake
att_bind<-att %>%  filter(LAKE_HISTORY_ID %in% violation_med$LAKE_HISTORY_ID,
                          use == 'source_of_water_supply',
                          #within_period == 'TRUE',
                          parameter %in% c('ammonia','chloride','fluoride','iron','manganese','phosphorus')) %>% 
  select(-use) %>% 
  bind_rows(att_phosphorus) %>% 
  group_by(LAKE_HISTORY_ID, parameter, within_period) %>% 
  summarise(n_samples_collected = sum(n_samples_collected),
            n_years_sampled = sum(n_years_sampled),
            n_exceedances = sum(n_exceedances)) %>% 
  mutate(exceedance_rate = n_exceedances/n_samples_collected,
         within_period = as.character(within_period))


# bring back in PWLID
pwl<-table2a_assessment_results %>% 
  filter(LAKE_HISTORY_ID %in% att_bind$LAKE_HISTORY_ID) %>% 
  select(LAKE_HISTORY_ID, LOCATION_PWL_ID) %>% distinct()


# combine exceedance rate with median values and arrange
score<- violation_med %>% left_join(att_bind, by = c('LAKE_HISTORY_ID','parameter','within_period')) %>% 
  distinct() %>% 
  mutate(med_multiparameter_prop = med_prop,
         score = med_prop * exceedance_rate,
         n_years_10yr = n_years_sampled,
         n_samples_10yr = n_samples_collected)

score[score$within_period=='FALSE',]$n_years_10yr<-0
score[score$within_period=='FALSE',]$n_samples_10yr<-0

score<-score %>% arrange(n_samples_10yr, desc(score)) %>% 
  left_join(param, by = c('LAKE_HISTORY_ID','within_period')) 


score$rank_multiparameter<-c(1:length(score$LAKE_HISTORY_ID))

score<-score %>% select(LAKE_HISTORY_ID,
                          score,
                          exceedance_rate,
                          med_multiparameter_prop,
                          rank_multiparameter,
                          parameter,
                          excursion_parameters,
                          n_years_10yr,
                          n_samples_10yr) %>% 
  filter(!is.na(excursion_parameters)) %>% 
  left_join(pwl, by = 'LAKE_HISTORY_ID')

# 4 lakes originally in table2a_assessment_results were left out
# 0602UWB0108 wan't sampled in the last 20 years
# 1003PAT0027, 1104CAM0152, 1104POR0152E there's no data anywhere, these share the same PWL ID's of lakes that were sampled
# 1308TAG0869, 1302MAH0053 met water quality standards in the last 20 years, but didnt meet them beyond 20 so they were excluded from the final dataframe
ch<-table2a_assessment_results[table2a_assessment_results$excursions=='TRUE' &
                                 !table2a_assessment_results$LAKE_HISTORY_ID %in% score$LAKE_HISTORY_ID,]

#write.csv(score, 'ranked_PWS_2023.csv', row.names = F, na = '')


# ranked lakes that overlap with DOH priorities
DOH_prioritized_ranked<-score %>% filter(LAKE_HISTORY_ID %in% DOH_prioritized_table2) 

#write.csv(DOH_prioritized_ranked,'DOH_prioritized_ranked.csv', row.names = F)






# table 3 from r markdown --------------------------
table3<-att %>% filter(is.na(n_samples_collected)) %>%
  select(LAKE_HISTORY_ID, LOCATION_PWL_ID) %>%
  distinct() 


doh<-read.csv("data.requests/PWS_IDs.csv")

table3sidwis<-merge(table3, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)

DOH_prioritized_table3<-table3 %>% filter(LAKE_HISTORY_ID %in% table3sidwis[!is.na(table3sidwis$PWS.ID),]$LAKE_HISTORY_ID)




# no pws from markdown ----------------------------
doh<-read.csv("data.requests/PWS_IDs.csv")

pnum<-read.csv("unassessed.with.p.number.on.chapter.x.csv")
pnum<-pnum %>% filter(found!='no',!(LAKE_HISTORY_ID %in% c('0202CHA0122','0705CAY0296','0302UWB0008','0601UWB0395','1003RAY0091',
                                                           '0501ELM0019','1301UWB0413', '1306UWB0683','1101DOR0098')))

pnum<-merge(nopwl,pnum,by=c('LAKE_HISTORY_ID'),all=TRUE)
nopwl<-pnum %>% filter(is.na(found)) %>% distinct()
pnum<-pnum %>% filter(!is.na(found))%>% distinct()

#adding the coordinates for Lindsey
coords<-newdata %>% filter(LOCATION_TYPE=="CENTROID") %>% select(LAKE_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
pnum<-merge(pnum,coords,by=c('LAKE_HISTORY_ID'),all.x=TRUE)

nopwl_sdwis<-merge(nopwl, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)
pnum_sdwis<-merge(pnum, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)

DOH_prioritized_no_PWL<-pnum_sdwis %>% filter(!is.na(PWS.ID))




# no data with PWL ----------------------

table3<-att %>% filter(is.na(n_samples_collected)) %>%
  select(LAKE_HISTORY_ID, LOCATION_PWL_ID) %>%
  distinct() 


no_dat_lake_info<-newdata %>% filter(LAKE_HISTORY_ID %in% table3$LAKE_HISTORY_ID) %>% 
  select(LOCATION_HISTORY_ID,
         LAKE_HISTORY_ID,
         LAKE_WATERBODY_NAME,
         LAKE_WATERBODY_TYPE,
         LAKE_BEACH_PRESENCE,
         LAKE_FIN,
         LAKE_ALTERNATE_NAME,
         LAKE_POND_NUMBER,
         PWS,
         MUNICIPALITY,
         LOCATION_PWL_ID,
         LOCATION_NAME,
         LOCATION_TYPE,
         LOCATION_X_COORDINATE,
         LOCATION_Y_COORDINATE,
         LOCATION_WATERBODY_CLASSIFICATION) %>% 
  distinct() %>% 
  mutate(location_id = paste(LAKE_HISTORY_ID, '_DH',sep =''))


doh<-read.csv("data.requests/PWS_IDs.csv")

table3sidwis<-merge(table3, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)

#write.csv(no_dat_lake_info, 'no_dat_lake_info.csv', row.names = F, na='')




# no pwl id --------------------------

doh<-read.csv("data.requests/PWS_IDs.csv")
nopwl_sdwis<-merge(nopwl, doh, by = c('LAKE_HISTORY_ID','LOCATION_PWL_ID'), all.x= T)

doh<-read.csv("data.requests/PWS_IDs.csv")

pnum<-read.csv("unassessed.with.p.number.on.chapter.x.csv")
pnum<-pnum %>% filter(found!='no',!(LAKE_HISTORY_ID %in% c('0202CHA0122','0705CAY0296','0302UWB0008','0601UWB0395','1003RAY0091',
                                                           '0501ELM0019','1301UWB0413', '1306UWB0683','1101DOR0098')))

pnum<-nopwl %>%  select(-found) %>% merge(pnum,by=c('LAKE_HISTORY_ID'),all=TRUE)
nopwl<-pnum %>% filter(is.na(found)) %>% distinct()
pnum<-pnum %>% filter(!is.na(found))%>% distinct()


no_dat<-c('0402UWB0030',
'0501ELM0019',
'0801LOW0594',
'0906CAR0050',
'1004UWB0212',
'1101DOR0098',
'1201COL0670',
'1201DOL0709',
'1201FOR0681',
'1301UWB0413',
'1306GLE0668',
'1306UWB0683',
'1307SAU0834')

missin<-pnum %>% filter(!LAKE_HISTORY_ID %in% no_dat)



# compile list of lakes to sample this season with their data -------------------------------

no_data_DOH_pimer<-data.frame('LAKE_HISTORY_ID' = c(DOH_prioritized_table3$LAKE_HISTORY_ID, DOH_prioritized_no_PWL$LAKE_HISTORY_ID),
                              'update_rank' = c(1:(length(DOH_prioritized_table3$LAKE_HISTORY_ID)+length(DOH_prioritized_no_PWL$LAKE_HISTORY_ID))),
                              'group' = c(rep('no_data',length(DOH_prioritized_table3$LAKE_HISTORY_ID)),rep('no_data_no_pwl',length(DOH_prioritized_no_PWL$LAKE_HISTORY_ID))))


# only going to do lakes with no data in the last 10 years that have excursions
DOH_prioritized_ranked<-DOH_prioritized_ranked %>% arrange(n_years_10yr, rank_multiparameter)
DOH_prioritized_ranked$update_rank<-1:length(DOH_prioritized_ranked$LAKE_HISTORY_ID)
  
ranked_DOH_pimer<-DOH_prioritized_ranked %>% select(LAKE_HISTORY_ID, update_rank) %>% 
  mutate(group = 'with_data')
  



pws_2023_primer<-newdata %>% filter(LAKE_HISTORY_ID %in% 
                               c(DOH_prioritized_table3$LAKE_HISTORY_ID,
                                 DOH_prioritized_no_PWL$LAKE_HISTORY_ID),
                             LOCATION_TYPE=='CENTROID') %>% 
  select(LAKE_HISTORY_ID,
         LAKE_WATERBODY_NAME,
         LOCATION_X_COORDINATE,
         LOCATION_Y_COORDINATE,
         LOCATION_WATERBODY_CLASSIFICATION) %>% 
  distinct() %>% 
  mutate(location_id = paste(LAKE_HISTORY_ID, '_DH',sep ='')) %>% 
  left_join(no_data_DOH_pimer, by = 'LAKE_HISTORY_ID')


pws_2023<-newdata %>% filter(LAKE_HISTORY_ID %in% 
                                      c(DOH_prioritized_ranked$LAKE_HISTORY_ID),
                                    LOCATION_TYPE=='CENTROID') %>% 
  select(LAKE_HISTORY_ID,
         LAKE_WATERBODY_NAME,
         LOCATION_X_COORDINATE,
         LOCATION_Y_COORDINATE,
         LOCATION_WATERBODY_CLASSIFICATION) %>% 
  distinct() %>% 
  mutate(location_id = paste(LAKE_HISTORY_ID, '_DH',sep ='')) %>% 
  left_join(ranked_DOH_pimer, by = 'LAKE_HISTORY_ID') %>% 
  mutate(update_rank = update_rank + length(pws_2023_primer$LAKE_HISTORY_ID)) %>% 
  bind_rows(pws_2023_primer)

write.csv(pws_2023, 'pws_2023.csv', row.names = F, na ='')
