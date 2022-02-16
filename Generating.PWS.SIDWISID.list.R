rm(list=ls())
setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Current")
location<-read.csv("new_database/L_LOCATION.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
lake<-read.csv("new_database/L_LAKE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/PWS_Prioritization")

lake<-lake %>% select(LAKE_HISTORY_ID,PWS) %>% distinct() %>% 
  mutate(PWS=toupper(PWS)) %>% 
  filter(PWS=="YES") %>% select(-PWS)
locs<-location %>% select(LAKE_HISTORY_ID,LOCATION_PWL_ID) %>% distinct()
  #remove NA values for lakes with PWLIDs
withpwl<-locs %>% filter(!is.na(LOCATION_PWL_ID)) %>% distinct()
locs<-locs %>% select(LAKE_HISTORY_ID) %>% distinct()
locs<-merge(locs,withpwl,by=c('LAKE_HISTORY_ID'),all=TRUE)
draft<-merge(lake,locs,by=c('LAKE_HISTORY_ID'),all.x=TRUE)


doh<-read.csv("data.requests/PWSrelated.by.doh.csv",na.strings=c("","NA"))
doh<-doh %>%
  rename(LAKE_HISTORY_ID=LakeID,
         PWS.ID=Water_SystemID) %>% 
  filter(!is.na(LAKE_HISTORY_ID)) %>% 
  select(LAKE_HISTORY_ID,PWS.ID) %>% distinct()

draft<-merge(draft,doh,by=c('LAKE_HISTORY_ID'),all=TRUE)

#these lakes have multiple PWLIDs so the intakes need to be specific for those segments
junk<-draft %>% filter(LAKE_HISTORY_ID %in% c('0202CHA0122','0300ONT0000','0705CAY0296','0705SEN0369','1000CHA0001')) %>% 
  select(LAKE_HISTORY_ID,PWS.ID) %>% distinct()
#create distinct SIDWIS id table for now
draft<-draft %>% 
  distinct(PWS.ID,.keep_all = TRUE)
write.csv(draft,file="data.requests/PWS_IDs.csv",row.names=FALSE)
write.csv(junk,file="data.requests/PWL_multiples_need_sorting.csv",row.names=FALSE)