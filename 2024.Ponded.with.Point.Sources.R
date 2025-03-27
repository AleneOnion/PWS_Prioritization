#pulling excursion data for permitted discharges in the EBS list that discharge to ponded waters

library(tidyverse)
library(pins)
library(dplyr)


# Load NYSDEC WQS ---------------------------------------------------------
data("nysdec_wqs", package = "stayCALM")

# DB file path ------------------------------------------------------------
db_path <- file.path(
  "C:",
  "Users",
  "amonion",
  "New York State Office of Information Technology Services",
  "BWAM - Lakes Database"
)


# Reading in new database -------------------------------------------------

csv_list <- list(lake = "L_LAKE.csv",
                 location = "L_LOCATION.csv",
                 results = "results.csv")

raw_list <- lapply(csv_list, function(file_i) {
  read.csv(
    file.path(db_path, "Current", "new_database", file_i),
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )
})


# Combine Tables ----------------------------------------------------------
raw_df <- raw_list$results %>%
  rename(LOCATION_HISTORY_ID = RSLT_LOCATION_HISTORY_ID) %>%
  left_join(x = raw_list$location,
            y = .,
            by = 'LOCATION_HISTORY_ID') %>%
  left_join(x = raw_list$lake,
            y = .,
            by = 'LAKE_HISTORY_ID')

#remove columns with special characters
raw_df2<-raw_df %>%
  mutate(RSLT_INTERPRETED_QUALIFIER=iconv(RSLT_INTERPRETED_QUALIFIER, from = "ISO-8859-1", to = "UTF-8")) %>%
  select(-RSLT_DETECTION_QUANT_LMT_UNIT)

# Cleaning ----------------------------------------------------------------
clean_df <- raw_df2 %>%
  janitor::clean_names() %>%
  rename(loc_history_id = location_history_id) %>%
  rename_with(~stringr::str_remove(.x, "lake_|location_|rslt_")) %>%
  rename(date = event_lmas_sample_date,
         fraction = result_sample_fraction,
         seg_id = pwl_id,
         site_id = loc_history_id,
         data_provider = event_lmas_data_provider,
         depth = profile_depth) %>%
  filter(!is.na(characteristic_name)) %>%
  mutate(
    across(where(is.character), tolower),
    date = as.Date(date,
                   format = "%Y-%m-%d"),
    parameter = case_when(
      characteristic_name %in% "chlorophyll a" ~ "chlorophyll_a",
      characteristic_name %in% "dissolved oxygen" ~  "dissolved_oxygen",
      characteristic_name %in% "nitrogen, ammonia (as n)" ~ "ammonia",
      characteristic_name %in% "nitrogen, nitrate (as n)" ~ "nitrate",
      characteristic_name %in% "nitrogen, nitrite" ~ "nitrite",
      characteristic_name %in% "nitrogen, nitrate-nitrite" ~ "nitrate_nitrite",
      characteristic_name %in% "phosphorus, total"  ~ "phosphorus",
      characteristic_name %in% "sulfate (as so4)" ~ "sulfate",
      characteristic_name %in% "total dissolved solids" ~ "total_dissolved_solids",
      characteristic_name %in% "hardness (as caco3)" ~ "hardness",
      TRUE ~ characteristic_name
    ),
    method_speciation = case_when(
      characteristic_name %in% "nitrogen, ammonia (as n)" ~ "as N",
      characteristic_name %in% "nitrogen, nitrate (as n)" ~ "as N",
      characteristic_name %in% "nitrogen, nitrite" ~ "as N",
      characteristic_name %in% "nitrogen, nitrate-nitrite" ~ "as N",
      TRUE ~ "none"
    ),
    fraction = case_when(
      parameter %in% "ph" ~ "total",
      parameter %in% "dissolved_oxygen" ~ "dissolved",
      TRUE ~ fraction
    ),
    units = if_else(parameter %in% "ph",
                    "ph_units",
                    result_unit),
    sample_id = paste(site_id, information_type, date, sep = "_"),
    value = result_value,
    replicate = "1",
    #If on of the qualifier columns indicates the value represents a non-detect
    #(i.e., "u") and there is no quantitation limit reported, then the value
    #will be represented as a zero.
    value = if_else(
      condition = grepl("u", validator_qualifier),
      true = 0,
      false = value
    ),
    validator_qualifier = if_else(
      parameter %in% "ph" & data_provider %in% "csl",
      "t",
      validator_qualifier
    )
  )

# Standardize Units -------------------------------------------------------
corr_units_df <- stayCALM::standard_units(
  .data = clean_df,
  .wqs_data = stayCALM::nysdec_wqs)
# Subset Columns ----------------------------------------------------------

lmas_df <- corr_units_df %>%
  select(
    seg_id,
    site_id,
    sample_id,
    info_type = information_type,
    depth,
    date,
    fraction,
    parameter,
    method_speciation,
    value,
    units,
    quantitation_limit,
    interpreted_qualifiers = interpreted_qualifier,
    validator_qualifiers = validator_qualifier,
    result_type,
    data_provider,
    replicate
  )

#pulling excursion data
lake_ir<-stayCALM::wqs_violations(
  .data = lmas_df,
  .period = stayCALM::gen_10_year_period(end_date = "2024-01-01"),
  .targeted_assessment = TRUE,
  .wipwl_df = stayCALM::wipwl_df,
  .wqs_df = stayCALM::nysdec_wqs,
  .tmdl_df = stayCALM::tmdl_df
)


# Create a data frame of Lake and Reservoir WI/PWL assessments.
lake_ir <- lake_ir$violation_data |>
  dplyr::distinct() |>
  dplyr::filter(water_type %in% "Lake/Reservoir")
#filter to the past 10 years
lakesdata<-lake_ir %>%
  filter(as.numeric(year)>2011)

#lakes with spedes
lakes<-newdata %>% 
  filter(LAKE_HISTORY_ID %in% c('0705CAY0296','1301CHO0437','1310COP0108','1309HOL0913','13-NRTH-0.4','1302PEA0093','1104SAC0314','1003FLO0086','1104SCH0374','0707SKA0193','13-WHAL-0.5','1306WIL0769','1306SIL0807')) %>% 
  select(LOCATION_PWL_ID,LAKE_HISTORY_ID,LAKE_WATERBODY_NAME) %>% distinct() %>% 
  rename(seg_id=LOCATION_PWL_ID) %>% 
  filter(!is.na(seg_id))

lakesdata<-lakesdata %>% filter(seg_id %in% c(unique(lakes$seg_id)))
lakesdata<-merge(lakes,lakesdata,by=c('seg_id'),all.x=TRUE)
lakesdata<-lakesdata %>% 
  group_by(LAKE_HISTORY_ID,seg_id,LAKE_WATERBODY_NAME,presentable_name,attaining_wqs) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(attaining_wqs %in% c('yes','no')) %>% 
  mutate(summary=paste(n," ",attaining_wqs,sep="")) %>% 
  group_by(LAKE_HISTORY_ID,seg_id,LAKE_WATERBODY_NAME,presentable_name) %>% 
  summarise(summary=str_c(summary, collapse=",")) %>% 
  ungroup() %>% 
  select(LAKE_HISTORY_ID,seg_id,LAKE_WATERBODY_NAME,presentable_name,summary) %>% distinct() %>% 
  mutate(summary=paste(presentable_name,"(",summary,")",sep="")) %>% 
  filter((grepl("no,",summary))|(grepl("no)",summary))) %>%
  select(LAKE_HISTORY_ID,seg_id,LAKE_WATERBODY_NAME,summary) %>% distinct() %>% 
  group_by(LAKE_HISTORY_ID,seg_id,LAKE_WATERBODY_NAME) %>% 
  summarise(summary=str_c(summary, collapse="|")) %>% 
  ungroup()


