colnames(lmas)
unique(lmas$PWS.Type)
unique(lmas$Primary.Source)
unique(lmas$Owner.Type)
unique(lmas$Activity.Status)
unique(lmas$Facility.Type.Description)
unique(lmas$Facility.Activity)
unique(lmas$Primacy.Type)
unique(lmas$Availability.Description)

rm(list=ls())
setwd("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
source("new_database/Reading.LMAS.Data.R")
setwd("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/PWS_Prioritization")
rm(list=setdiff(ls(), c('newdata')))

temp<-newdata %>% 
  filter(SAMPLE_DATE>'2020-01-01') %>% 
  mutate(combined=paste(CHARACTERISTIC_NAME,INFORMATION_TYPE,RSLT_RESULT_SAMPLE_FRACTION,sep = "_"))  %>% 
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE,RSLT_LABORATORY_QUALIFIER,RSLT_VALIDATOR_QUALIFIER) %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>% 
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R"),
         combined %in% c('CHLOROPHYLL A_OW_TOTAL','DEPTH, SECCHI DISK DEPTH_SD_NA','PHOSPHORUS, TOTAL_OW_TOTAL')) %>%  
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE) %>% 
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,.keep_all = TRUE) 

draft<-draft %>% filter(seg_id %in% c('0105-0021','1005-0049','1301-0190','1301-0183',
                                      '1301-0234','1301-0153','1304-0017','1311-0001','1501-0007','1501-0002'),
                        use=="source_of_water_supply") %>% 
  select(seg_id,segment_assessment,use_assessment,parameter) %>% distinct()


rmarkdown::render("PWS_Prioritization.Rmd")


rmarkdown::render("Assessments_Analysis.Rmd")


forplot<-draft1 %>% select(LOCATION_PWL_ID,parameter_simple,use,use_assessment) %>% 
  filter(parameter_simple=="dissolved_oxygen",
         use_assessment!="IR3_fully-supported_unconfirmed") %>% 
  distinct()
classes<-draft %>% select(LOCATION_PWL_ID,class) %>% distinct()
fish<-merge(forplot,classes,by=c('LOCATION_PWL_ID'),all.x = TRUE)

junk<-lmas %>% filter(LAKE_HISTORY_ID=="1306UWB6070") %>% distinct() %>% 
  select(class,segment_assessment,use,use_assessment,parameter) %>% distinct()


junk<-lmas %>% filter(use=="source_of_water_supply",
                      !(use_assessment %in% c('IR5_impaired_confirmed','IR1_stressed_confirmed','IR4a_impaired_confirmed','not-applicable'))) %>%
  select(LOCATION_PWL_ID,waterbody) %>% distinct()

junk<-lmas %>% filter(use=="source_of_water_supply",
                      use_assessment=="IR3_unassessed") %>%
  select(LOCATION_PWL_ID,waterbody) %>% distinct()
junk<-draft %>% filter(use_assessment=="IR5_impaired_confirmed",LOCATION_PWL_ID=="1306-0037")



junk<-forplot %>% 
  filter(class_sample!='a',
         LOCATION_PWL_ID %in% c('1308-0014','1307-0020','1307-0020','1301-0234','1301-0229','1201-0160','1201-0139','1201-0113','1001-0001','1000-0004','0902-0034','0801-0281','0705-0030','0705-0026','0705-0014','0303-0025','0303-0022','0303-0002','0302-0058','0302-0020','0301-0018')) %>% 
  distinct()



junk<-lmas %>% filter(substring(class,1,1)!="a") %>% distinct()
junk<-junk %>% select(LOCATION_PWL_ID,LAKE_WATERBODY_NAME,class,LAKE_FIN) %>% distinct()

draft %>% filter(use=="source_of_water_supply",parameter!="not-applicable",parameter!='No_Data') %>% select(parameter) %>% distinct() %>% arrange(parameter)

draft %>% 
  filter(LOCATION_PWL_ID %in% c('0303-0002','0303-0065')) %>% 
  distinct() %>% 
  mutate(assessment=paste(use_assessment,parameter,sep="__")) %>% 
  select(waterbody,use,assessment) %>% distinct() %>% 
  spread(waterbody,assessment) %>% 
  filter(use!="epa_appended_listing",use!="shellfishing")

junk<-draft %>% 
  filter(LOCATION_PWL_ID %in% c('0303-0002','0303-0065')) 


junk<-location %>% 
  filter(LAKE_HISTORY_ID %in% c('1301WIC0183A','1306UWB6000','1201UWB0662','1301UWB0028D','1301UWB0028F',
                                '1301UWB0028E','1501UWB1007A','0302UWB0008','0601UWB0396','0601UWB0395',
                                '1601UWB1117A','0302UWB5258','1404TRA5563','1307SAU0834','1301MEL0331',
                                '0503HOR0035','1301FER0028C','1101DOR0098','0404DAN0092F','1702BRY1106',
                                '1404BLA0462A','1304BEA0345A'),
         LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()


rmarkdown::render("PWS_for_TMDLs.Rmd")

rmarkdown::render("PWS_unconfirmed_priorities.Rmd")
