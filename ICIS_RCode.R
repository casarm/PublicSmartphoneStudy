#load libraries 
library(tidyverse)
library(lmerTest)
library(lme4)
#library(janitor)
#library(report)
#library(car)
#library(rstatix)
library(lubridate)
library(hablar)

#create ADEX tibble ("Edit files" are files from participants that contributed
#the full three days of recording. They are edited down to 4 hours)

ADEX_data <- plyr::ldply(list.files(pattern = "ADEX"), read_csv) %>% 
  group_by(Child_ChildID)
ADEX_Edit <- plyr::ldply(list.files(pattern = "Edit"), read_csv) %>% 
  group_by(Child_ChildID)
ADEX_data <- bind_rows(ADEX_Edit, ADEX_data) %>%
  mutate_at("Child_ChildID", str_replace, "C", "")

#Exclude participants due to no matching smartphone data (technical difficulties)
ADEX_data <-filter(ADEX_data, Child_ChildID != "220" & 
                     Child_ChildID !="171" &
                     Child_ChildID !="224" &
                     Child_ChildID != "276")

#Create Smartphone Use tibble

x5Min_Android <- list.files(pattern = "^.*QT.*")
x5Min_Android <- x5Min_Android  %>%
  set_names() %>% 
  map_dfr(read_csv, .id = "source") %>%
  mutate(Child_ChildID = word(source, sep = "_")) %>% 
  mutate(Phone_Type="Android") %>%
  group_by(Child_ChildID)
x5Min_Android$start_time<-parse_date_time(x5Min_Android$start_time,"dbIMp")
x5Min_Android$end_time<-parse_date_time(x5Min_Android$end_time,"dbIMp")


x5Min_iPhone <- list.files(pattern = "^.*RD.*")
x5Min_iPhone <- x5Min_iPhone %>%
  set_names() %>% 
  map_dfr(read_csv, .id = "source") %>%
  mutate(Child_ChildID  = word(source, sep = "_")) %>% 
  mutate(Phone_Type="iPhone") %>%
  group_by(Child_ChildID)

Smartphone_data<-bind_rows(x5Min_Android,x5Min_iPhone)


# Date/Time conversion ADEX data
ADEX_data$Clock_Time_TZAdj <- as.POSIXct(ADEX_data$Clock_Time_TZAdj, 
                                         format = "%m/%d/%Y %H:%M:%S") 

#Make ages into groups
ADEX_data <- ADEX_data %>% add_column(ChildAgeGroup=NA)
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "6"] <-  "6-9 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "7"] <-  "6-9 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "8"] <-  "6-9 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "9"] <-  "6-9 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "12"] <- "12-15 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "13"] <- "12-15 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "14"] <- "12-15 Months"
ADEX_data$ChildAgeGroup[ADEX_data$Child_Age == "15"] <- "12-15 Months"



#strip year and timezone (year/timezone info missing from (some) smartphone data)
ADEX_data <- mutate(ADEX_data, start_time = Clock_Time_TZAdj)
year(ADEX_data$start_time) <-2000
ADEX_data$start_time<-force_tz(ADEX_data$start_time, "UTC")
year(Smartphone_data$start_time) <-2000
Smartphone_data$start_time<-force_tz(Smartphone_data$start_time, "UTC")
year(Smartphone_data$end_time) <-2000
Smartphone_data$end_time<-force_tz(Smartphone_data$end_time, "UTC")


######FINAL DATA TIBBLE######

ADEX_Smartphone_data <-left_join(ADEX_data, Smartphone_data, by = c("Child_ChildID", "start_time"))

#remove first ADEX day of C237 to ensure match with smartphone data stays in the dataset

ADEX_Smartphone_data <- filter(ADEX_Smartphone_data, File_Name != "237_1_20210331.its")

#Select 4hrs from participants with longer recordings and drop ADEX rows less than 300 duration 
ADEX_Smartphone_data <- subset(ADEX_Smartphone_data, Audio_Duration ==300)
ADEX_Smartphone_data <- slice_head(ADEX_Smartphone_data, n = 50) 

#check on missing data
count<-ADEX_Smartphone_data%>%group_by(Child_ChildID)%>% count(is.na(duration))

#replace NAs in smartphone data duration with 0
ADEX_Smartphone_data <-mutate(ADEX_Smartphone_data, duration=replace_na(duration, 0))

#convert durations to time in s
ADEX_Smartphone_data$duration <- as.difftime(ADEX_Smartphone_data$duration,
                                             format = "%S", units = "secs")

#add naptime info
naptime <- as_tibble(read.csv("naptime.csv"))
naptime$participant<-as.character(naptime$participant)
naptime$starttime<-as.POSIXct(paste(naptime$date, naptime$starttime), format= "0000-%m-%d %I:%M %p", rm.na=TRUE)
year(naptime$starttime) <-2000
naptime$stoptime<-as.POSIXct(paste(naptime$date, naptime$stoptime), format= "0000-%m-%d %I:%M %p", rm.na=TRUE)
year(naptime$stoptime) <-2000
naptime <- naptime %>% mutate(napinterval=interval(force_tz(starttime, "UTC"), force_tz(stoptime, "UTC")))

ADEX_Smartphone_data<-ADEX_Smartphone_data %>% 
  mutate(interval=interval(start_time, start_time + 300))

find_overlaps_nap <- function(main_interval, main_participant) {
  result <- FALSE
  if (!is.interval(main_interval)) {
    return(NA)
  }
  for (i in 1:nrow(naptime)) {
    nap_participant <- as.character(naptime[i,match("participant",names(naptime))])
    if (main_participant == nap_participant) {
      nap_interval <- naptime[i, match("napinterval",names(naptime))][[1]]
      if (!is.interval(nap_interval) | is.na(int_start(nap_interval))) {
        next
      }
      if (int_overlaps(main_interval, nap_interval)) {
        result <- TRUE
      }
    }
  }
  return(result)
}    

generate_overlap_data <- function() {
  nap_result=c()
  for (i in 1:nrow(ADEX_Smartphone_data)) {
    main_participant= ADEX_Smartphone_data[i, match("Child_ChildID",names(ADEX_Smartphone_data))][[1]]
    main_interval=ADEX_Smartphone_data[i,match("interval", names(ADEX_Smartphone_data))][[1]]
    if (is.na(int_start(main_interval))) {
      nap_result=append(nap_result,NA)
      next
    }
    nap_result=append(nap_result,find_overlaps_nap(main_interval,main_participant))
  }
  nap_result
}

ADEX_Smartphone_data$naptimes <- generate_overlap_data()

#exclude participants with no data
ADEX_Smartphone_data_excl <-filter(ADEX_Smartphone_data, Child_ChildID != "223" & 
                                     Child_ChildID != "252"&
                                     Child_ChildID !="268")

#exclude rows with 0 smartphone usage
ADEX_Smartphone_data_excl2 <-filter(ADEX_Smartphone_data, duration !=0)

#exclude rows where infant is napping
ADEX_Smartphone_data_nonaps <-filter(ADEX_Smartphone_data, naptimes==FALSE)

#exlude participants with no data && no naps
ADEX_Smartphone_data_excl_nonaps <-filter(ADEX_Smartphone_data_excl, naptimes==FALSE)

#data from prior stages not publicly available due to privacy concerns
#write.csv(Smartphone, file="Smartphone_Public.csv")
#write.csv(Social_Media, file="Social_Media_Public.csv")

####AWC Test#####

#full dataset

AWC<-lmer(AWC ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data)
anova(AWC)

#full dataset with no naps
AWC_nonaps<-lmer(AWC ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_nonaps)
anova(AWC_nonaps)

#only dyads with actual smartphone usage recorded
AWC_excl<-lmer(AWC ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_excl)
anova(AWC_excl)

#only dyads with actual smartphone usage recorded && no naps
AWC_excl_nonaps<-lmer(AWC ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_excl_nonaps)
anova(AWC_excl_nonaps)

#only rows with some smartphone usage recorded
AWC_excl2<-lmer(AWC ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_excl2)
anova(AWC_excl2)

#####VOC Test#####
#full dataset

Child_Voc<-lmer(Child_Voc_Count~duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data)
anova(Child_Voc)

#full dataset with no naps
Child_Voc_nonaps<-lmer(Child_Voc_Count~duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_nonaps)
anova(Child_Voc_nonaps)

#only dyads with actual smartphone usage recorded
Child_Voc_excl<-lmer(Child_Voc_Count ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_excl)
anova(Child_Voc_excl)

#only dyads with actual smartphone usage recorded/no naps
Child_Voc_excl_nonaps<-lmer(Child_Voc_Count ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_excl_nonaps)
anova(Child_Voc_excl_nonaps)

#only rows with some smartphone usage recorded
Child_Voc_excl2<-lmer(Child_Voc_Count ~ duration*ChildAgeGroup + (1|Child_ChildID), data=ADEX_Smartphone_data_excl2)
anova(Child_Voc_excl2)

#does child voc predict smartphone usage?
CVCpredict<-lmer(as.numeric(duration) ~ Child_Voc_Count + (1|Child_ChildID), data=ADEX_Smartphone_data_excl_nonaps)
anova(CVCpredict)

#Child Voc Scatterplot
CVCscatterplot <- ggplot(data = ADEX_Smartphone_data_excl_nonaps, aes(x = Child_Voc_Count, y = as.numeric(duration), color = ChildAgeGroup)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Infant Vocalizations (no naptime)",
       y = "Maternal Smartphone Usage")
CVCscatterplot