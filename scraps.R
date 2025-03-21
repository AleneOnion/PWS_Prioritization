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
rm(list=setdiff(ls(), c("newdata")))

temp<-newdata %>% 
  filter(SAMPLE_DATE>"2020-01-01") %>% 
  mutate(combined=paste(CHARACTERISTIC_NAME,INFORMATION_TYPE,RSLT_RESULT_SAMPLE_FRACTION,sep = "_"))  %>% 
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE,RSLT_LABORATORY_QUALIFIER,RSLT_VALIDATOR_QUALIFIER) %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>% 
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R"),
         combined %in% c("CHLOROPHYLL A_OW_TOTAL","DEPTH, SECCHI DISK DEPTH_SD_NA","PHOSPHORUS, TOTAL_OW_TOTAL")) %>%  
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE) %>% 
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,.keep_all = TRUE) 

draft<-draft %>% filter(seg_id %in% c("0105-0021","1005-0049","1301-0190","1301-0183",
                                      "1301-0234","1301-0153","1304-0017","1311-0001","1501-0007","1501-0002"),
                        use=="source_of_water_supply") %>% 
  select(seg_id,segment_assessment,use_assessment,parameter) %>% distinct()


rmarkdown::render("PWS_Prioritization.Rmd")


rmarkdown::render("Assessments_Analysis.Rmd")


forplot<-draft1 %>% select(LOCATION_PWL_ID,parameter_simple,use,use_assessment) %>% 
  filter(parameter_simple=="dissolved_oxygen",
         use_assessment!="IR3_fully-supported_unconfirmed") %>% 
  distinct()
classes<-draft %>% select(LOCATION_PWL_ID,class) %>% distinct()
fish<-merge(forplot,classes,by=c("LOCATION_PWL_ID"),all.x = TRUE)

junk<-lmas %>% filter(LAKE_HISTORY_ID=="1306UWB6070") %>% distinct() %>% 
  select(class,segment_assessment,use,use_assessment,parameter) %>% distinct()


junk<-lmas %>% filter(use=="source_of_water_supply",
                      !(use_assessment %in% c("IR5_impaired_confirmed","IR1_stressed_confirmed","IR4a_impaired_confirmed","not-applicable"))) %>%
  select(LOCATION_PWL_ID,waterbody) %>% distinct()

junk<-lmas %>% filter(use=="source_of_water_supply",
                      use_assessment=="IR3_unassessed") %>%
  select(LOCATION_PWL_ID,waterbody) %>% distinct()
junk<-draft %>% filter(use_assessment=="IR5_impaired_confirmed",LOCATION_PWL_ID=="1306-0037")



junk<-forplot %>% 
  filter(class_sample!="a",
         LOCATION_PWL_ID %in% c("1308-0014","1307-0020","1307-0020","1301-0234","1301-0229","1201-0160","1201-0139","1201-0113","1001-0001","1000-0004","0902-0034","0801-0281","0705-0030","0705-0026","0705-0014","0303-0025","0303-0022","0303-0002","0302-0058","0302-0020","0301-0018")) %>% 
  distinct()



junk<-lmas %>% filter(substring(class,1,1)!="a") %>% distinct()
junk<-junk %>% select(LOCATION_PWL_ID,LAKE_WATERBODY_NAME,class,LAKE_FIN) %>% distinct()

draft %>% filter(use=="source_of_water_supply",parameter!="not-applicable",parameter!="No_Data") %>% select(parameter) %>% distinct() %>% arrange(parameter)

draft %>% 
  filter(LOCATION_PWL_ID %in% c("0303-0002","0303-0065")) %>% 
  distinct() %>% 
  mutate(assessment=paste(use_assessment,parameter,sep="__")) %>% 
  select(waterbody,use,assessment) %>% distinct() %>% 
  spread(waterbody,assessment) %>% 
  filter(use!="epa_appended_listing",use!="shellfishing")

junk<-draft %>% 
  filter(LOCATION_PWL_ID %in% c("0303-0002","0303-0065")) 


junk<-location %>% 
  filter(LAKE_HISTORY_ID %in% c("1301WIC0183A","1306UWB6000","1201UWB0662","1301UWB0028D","1301UWB0028F",
                                "1301UWB0028E","1501UWB1007A","0302UWB0008","0601UWB0396","0601UWB0395",
                                "1601UWB1117A","0302UWB5258","1404TRA5563","1307SAU0834","1301MEL0331",
                                "0503HOR0035","1301FER0028C","1101DOR0098","0404DAN0092F","1702BRY1106",
                                "1404BLA0462A","1304BEA0345A"),
         LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()


rmarkdown::render("PWS_for_TMDLs.Rmd")

rmarkdown::render("PWS_unconfirmed_priorities.Rmd")
rmarkdown::render("PWS_screening_priorities.Rmd")


lake<-lake %>% select(LAKE_HISTORY_ID,PWS) %>% distinct() %>% 
  mutate(PWS=toupper(PWS)) %>% 
  filter(PWS=="YES")
locs<-location %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_PWL_ID) %>% distinct()
pws<-merge(lake,locs,by=c("LAKE_HISTORY_ID"),all.x=TRUE)


rmarkdown::render("PWS_unconfirmed_priorities.Rmd")
rmarkdown::render("PWS_screening_priorities.Rmd")



setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Current")
location<-read.csv("new_database/L_LOCATION.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
lake<-read.csv("new_database/L_LAKE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/PWS_Prioritization")
lake<-lake %>% select(LAKE_HISTORY_ID,PWS) %>% distinct() %>% 
  mutate(PWS=toupper(PWS)) %>% 
  filter(PWS=="YES")
pws<-lake
doh<-read.csv("data.requests/PWS_IDs.csv")
doh<-doh %>%
  select(LAKE_HISTORY_ID,PWS.ID) %>% distinct() 
pws<-merge(pws,doh,by=c("LAKE_HISTORY_ID"),all.x=TRUE)
pws<-pws %>% filter(is.na(PWS.ID))


rmarkdown::render("PWS_unconfirmed_priorities.Rmd")
rmarkdown::render("PWS_screening_priorities.Rmd")
rmarkdown::render("PWS_priorities.Rmd")
rmarkdown::render("PWS_priorities_for_DOH.Rmd")




junk<-draft %>% filter(seg_id %in% c("1401-0079","1401-0099","0601-0010","1101-0060","1401-0058","1308-0018","1701-0357","1701-0353","1701-0353",
                                    "1701-0241","1701-0125","1701-0125","1701-0018","1501-0063","1501-0050","1501-0049","1501-0017","1404-0038","1404-0033","1404-0032","1402-0059","1402-0055","1402-0045","1402-0035","1402-0031","1401-0141","1401-0129","1401-0124","1401-0091","1401-0080","1401-0058","1311-0007","1311-0006","1310-0045","1310-0044","1310-0033","1310-0014","1310-0002","1310-0001","1309-0031","1308-0023","1308-0018","1308-0018","1308-0018","1308-0014","1308-0003","1306-0119","1306-0075","1306-0060","1306-0060","1305-0010","1304-0033","1304-0001","1303-0021","1302-0151","1302-0150","1302-0147","1302-0141","1302-0141","1302-0140","1302-0136","1302-0121","1302-0120","1302-0118","1302-0115","1302-0103","1302-0096","1302-0089","1302-0083","1302-0080","1302-0054","1302-0053","1302-0007","1302-0006","1302-0004","1302-0002","1301-0270","1301-0267","1301-0263","1301-0214","1301-0156","1301-0149","1301-0147","1301-0143","1301-0142","1301-0140","1301-0126","1301-0091","1301-0059","1301-0053","1301-0043","1301-0042","1301-0037","1301-0035","1301-0034","1301-0025","1301-0008","1204-0006","1202-0014","1201-0175","1201-0144","1201-0142","1201-0113","1201-0110","1201-0050","1201-0050","1201-0046","1201-0019","1201-0016","1104-0293","1104-0258","1104-0255","1104-0252","1104-0235","1104-0232","1104-0205","1104-0193","1104-0105","1104-0105","1104-0105","1104-0075","1104-0074","1104-0051","1104-0050","1104-0047","1104-0037","1104-0031","1104-0024","1104-0002","1104-0002","1103-0023","1103-0015","1103-0002","1102-0029","1102-0014","1102-0011","1101-0084","1101-0036","1101-0036","1101-0012","1006-0016","1006-0016","1006-0016","1006-0016","1006-0016","1006-0016","1005-0047","1005-0040","1005-0009","1004-0090","1004-0068","1004-0067","1004-0050","1003-0109","1003-0079","1003-0076","1003-0068","1003-0048","1003-0048","1001-0027","0906-0065","0906-0064","0906-0038","0906-0020","0906-0016","0906-0008","0906-0003","0906-0001","0906-0001","0905-0180","0905-0093","0904-0059","0903-0114","0903-0081","0903-0061","0903-0060","0903-0044","0902-0159","0902-0159","0902-0157","0902-0104","0902-0104","0902-0102","0902-0056","0902-0036","0902-0034","0902-0030","0801-0373","0801-0321","0801-0206","0801-0205","0801-0204","0801-0176","0801-0172","0801-0165","0707-0004","0707-0004","0707-0004","0706-0009","0706-0009","0705-0050","0705-0050","0705-0050","0705-0050","0705-0050","0705-0040","0705-0030","0705-0029","0705-0026","0705-0025","0705-0025","0705-0021","0705-0021","0705-0021","0705-0021","0705-0021","0705-0014","0705-0003","0705-0003","0705-0003","0704-0025","0704-0001","0704-0001","0703-0087","0703-0082","0703-0047","0703-0022","0703-0021","0703-0015","0702-0011","0702-0011","0701-0002","0602-0116","0602-0112","0602-0111","0602-0109","0602-0108","0602-0098","0602-0096","0602-0093","0602-0092","0602-0090","0602-0086","0602-0077","0602-0074","0602-0053","0602-0041","0602-0040","0602-0019","0602-0018","0602-0017","0602-0014","0602-0007","0601-0109","0601-0068","0601-0066","0601-0016","0601-0013","0601-0012","0502-0015","0502-0012","0502-0011","0502-0002","0502-0001","0404-0039","0403-0024","0403-0002","0402-0032","0402-0032","0402-0011","0402-0011","0402-0011","0402-0004","0402-0004","0402-0002","0303-0043","0303-0002","0302-0031","0302-0021","0302-0020","0302-0017","0302-0012","0301-0035","0202-0072","0202-0020","0202-0004","0202-0003","0201-0016","0104-0060","0104-0004","0104-0001")) %>% distinct()





library(tidyverse)
library(knitr)
#Reading in new database
#setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/Current")
setwd("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
location<-read.csv("new_database/L_LOCATION.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
lake<-read.csv("new_database/L_LAKE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#setwd("C:/Users/leneo/Dropbox/Alene/Rscripts/PWS_Prioritization")
setwd("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/PWS_Prioritization")

rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP.Variance/2023.01.one.sample.analysis.Rmd")


lake<-lake %>% select(LAKE_HISTORY_ID,PWS) %>% distinct() 
locs<-location %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_PWL_ID) %>% distinct()
pws<-merge(lake,locs,by=c('LAKE_HISTORY_ID'),all.x=TRUE)

#draft<-read.csv("C:/Users/leneo/Dropbox/Alene/Rscripts/PWS_Prioritization/2022_stayCALM_ponded_internal-draft-assessments.csv")
draft<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/PWS_Prioritization/2022_stayCALM_ponded_internal-draft-assessments.csv")
draft<-draft %>% 
  select(seg_id:parameter) %>% 
  distinct() %>% 
  rename(LOCATION_PWL_ID=seg_id) %>% 
  mutate(value=1)
draft<-merge(pws,draft,by=c('LOCATION_PWL_ID'),all.x=TRUE)

draft<-draft %>% filter(LOCATION_PWL_ID=="1306-0037") %>% distinct()


rmarkdown::render("2023.01.12.PWS.wqs.attainment.Rmd")

'0402RUS0033','1307KIN0837','1307KIN0838A'

junk<-att %>% filter(LAKE_HISTORY_ID %in% c('0905STA0281','1301ALC0185','1403LIL0311','1303WAL0257','1302MAH0053','0601WIL0297','0602UWB0108'))


list.files("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Documents/Paperwork/seasonal.position/applicants/2024")