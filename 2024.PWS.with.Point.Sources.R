#Identifying PWS with point sources and excursions that were caused by those point sources

points<-read.csv("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current/new_database/permitted_facilities_discharging_to_lakes.csv")
points<-points %>% select(SPDES_PERMIT_NUM,LAKE_HISTORY_ID) %>% distinct()

PWS<-newdata %>% filter(PWS=="YES",LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,DEC_REGION) %>% distinct()

PWS<-merge(PWS,points,by=c('LAKE_HISTORY_ID'),all.x=TRUE)

PWS<-PWS %>% filter(!is.na(SPDES_PERMIT_NUM)) %>% 
  select(SPDES_PERMIT_NUM,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,DEC_REGION)

#assessment data
ass<-read.csv("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Report Automation/Lake reports/Current_Script/Lake_Reports/Trends.and.Distributions/assessments.csv")

#first confirming that all the PWS in this list have been sampled
sampled<-ass %>% 
  filter(attaining_wqs=="yes",CHARACTERISTIC_NAME=="Phosphorus, Total") %>% 
  mutate(LAKE_HISTORY_ID=gsub("_.*","",LOCATION_HISTORY_ID)) %>% 
  group_by(LAKE_HISTORY_ID,CHARACTERISTIC_NAME) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  select(LAKE_HISTORY_ID,CHARACTERISTIC_NAME,n) %>% distinct() 
PWS<-merge(PWS,sampled,by=c("LAKE_HISTORY_ID"),all.x = TRUE)
unsampled<-PWS %>% filter(is.na(n))
sampled<-PWS
PWS<-PWS %>% select(-CHARACTERISTIC_NAME,-n)

ass<-ass %>% 
  mutate(LAKE_HISTORY_ID=gsub("_.*","",LOCATION_HISTORY_ID)) %>% 
  group_by(LAKE_HISTORY_ID,CHARACTERISTIC_NAME) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  filter(attaining_wqs=="no",use %in% c("source_of_water_supply","fishing")|CHARACTERISTIC_NAME=="Phosphorus, Total") %>% 
  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,SAMPLE_DATE,year,CHARACTERISTIC_NAME,attaining_wqs,n,result,units,use) %>% distinct() 

PWS<-merge(PWS,ass,by=c("LAKE_HISTORY_ID"),all.x = TRUE)

PWS<-PWS %>% 
  filter(!is.na(CHARACTERISTIC_NAME)) %>% 
  mutate(CHARACTERISTIC_NAME=paste(CHARACTERISTIC_NAME,"(",n,")")) %>% 
  select(SPDES_PERMIT_NUM,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,CHARACTERISTIC_NAME,n,DEC_REGION) %>% distinct() %>% 
  group_by(SPDES_PERMIT_NUM,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,DEC_REGION) %>% 
  summarise(excursions=paste0(CHARACTERISTIC_NAME, collapse = "|||| ")) %>% 
  ungroup() %>% 
  filter(excursions!="Dissolved Oxygen ( [:digit:] )",
         excursions!="Phosphorus, Total ( 20 )",
         excursions!="Phosphorus, Total ( 1 )",
         excursions!="Phosphorus, Total ( 31 )",
         excursions!="Phosphorus, Total ( 58 )|||| Dissolved Oxygen ( 1 )")

#So my take away is that Owasco has background metals concentrations that are not caused by nutrient loading/anoxia.
#All the other lakes have elevated metal concentrations likely due to nutrient loding/anoxia but we need to confirm these assessments 
#Chodikee 2 events needed
#Hollister 1 events needed
#Upper Saranac 6 profiles only needed
#Tuxedo 6 profiles only needed

#Bryam and Kiamesha already have completed assessments and no additional sampling is necessary
print(PWS)

#Cranberry lake is entirely unsampled
#but it's a non-community system so we aren't considering those true PWS
print(unsampled)

#NOTE that some are still unassessed without excursions. with full assessments they may have excursions
print(sampled %>% filter(n<6))
