pws_2022<-read.csv('pws_2022.csv') %>% 
  mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', LOCATION_ID)))

violation<-read.csv('2022_lmas_evaluations_violation_data.csv')


# general stats on violation_data tab
violation_summary_within<- violation %>% mutate(LAKE_HISTORY_ID = toupper(sub('_.*', '', site_id))) %>% 
  filter(LAKE_HISTORY_ID %in% pws_2022$LAKE_HISTORY_ID) %>% 
  select(LAKE_HISTORY_ID, year) %>% distinct() %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  summarise(year_recent = max(year)) %>% 
  distinct()

length(unique(pws_2022$LAKE_HISTORY_ID))
length(unique(pws_2022$LAKE_HISTORY_ID))


new_data_summary<- newdata %>% 
  filter(LAKE_HISTORY_ID %in% pws_2022$LAKE_HISTORY_ID,
         !is.na(RSLT_RESULT_VALUE)) %>% 
  mutate(year = substr(SAMPLE_DATE,1,4)) %>% 
  select(LAKE_HISTORY_ID, year) %>% distinct() %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  summarise(year_recent = max(year)) %>% 
  distinct()


att_summary<-att %>% filter(LAKE_HISTORY_ID %in% pws_2022$LAKE_HISTORY_ID,
                            use == 'source_of_water_supply')

length(unique(att_summary$LAKE_HISTORY_ID))

one<-violation_summary_within %>% filter(!LAKE_HISTORY_ID %in% att_summary$LAKE_HISTORY_ID)


test<-table2a_assessment_results %>% filter(LAKE_HISTORY_ID %in% pws_2022$LAKE_HISTORY_ID) %>% 
  filter(LAKE_HISTORY_ID %in% att_summary$LAKE_HISTORY_ID)

a_b<-read.csv('a_b.csv')

a<-a_b %>% filter(a_b == 'a')

c<-a_b %>% filter(a_b == 'b') %>% filter(LAKE_HISTORY_ID%in% a$LAKE_HISTORY_ID)





# test to see which PWS were the field season spreadsheet --------------------------
field_season<-read.csv('field_season.csv')

fs<-field_season[duplicated(field_season$LOCATION_ID),]


dupframe<-field_season[field_season$LOCATION_ID %in% fs$LOCATION_ID,]

pws<-field_season %>% filter(PURPOSE == 'PWS Screening') %>% select(LOCATION_ID) %>% distinct() %>% 
  mutate(LAKE_HISTORY_ID = sub('_.*', '',LOCATION_ID))

np<-table3 %>% filter(!LAKE_HISTORY_ID %in% pws$LAKE_HISTORY_ID)


