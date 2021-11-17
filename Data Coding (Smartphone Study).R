#load libraries 
library(tidyverse)
library(lmerTest)
library(lme4)
library(janitor)
library(report)
library(car)
library(rstatix)
library(lubridate)

#set working directory
setwd("S:/CurrentStudies/Lab members studies/Smartphone study (Mercedes-MC)/Preparing data for analysis/Smartphone Data/Data")

#create tibbles (Participant_data, ADEX_data, x5Min_Android, x5Min_iPhone, & Android_data)
Participant_data <- plyr::ldply(list.files(pattern = "iPhone_SM_Data"), read_csv) %>% 
  group_by(Child_ChildID)
ADEX_data <- plyr::ldply(list.files(pattern = "ADEX"), read_csv) %>% 
  group_by(Child_ChildID)
x5Min_Android <- list.files(pattern = "^.*QT.*")
x5Min_Android <- x5Min_Android  %>%
  set_names() %>% 
  map_dfr(read_csv, .id = "source") %>%
  mutate(Child_ChildID = word(source, sep = "_")) %>% 
  group_by(Child_ChildID)
x5Min_iPhone <- list.files(pattern = "^.*RD.*")
x5Min_iPhone <- x5Min_iPhone %>%
  set_names() %>% 
  map_dfr(read_csv, .id = "source") %>%
  mutate(Child_ChildID  = word(source, sep = "_")) %>% 
  group_by(Child_ChildID)
Android_data <- list.files(pattern = "Android") 
Android_data <- Android_data  %>%
  set_names() %>% 
  map_dfr(read_csv, .id = "source") %>%
  mutate(Child_ChildID = word(source, sep = "_")) %>% 
  group_by(Child_ChildID)

#Select 4hrs from participants with longer recordings
ADEX_Edit <- plyr::ldply(list.files(pattern = "Edit"), read_csv) %>% 
  group_by(Child_ChildID)
ADEX_Edit <- slice_head(ADEX_Edit, n = 50) 
ADEX_data <- bind_rows(ADEX_Edit, ADEX_data)

# Date/Time conversion while retaining column placement
ADEX_data$Clock_Time_TZAdj <- as.POSIXct(ADEX_data$Clock_Time_TZAdj, 
                                         format = "%m/%d/%Y %H:%M:%S") 
ADEX_data$Audio_Duration <- duration(num = ADEX_data$Audio_Duration, 
                                     units = "seconds") #seconds
ADEX_data$Child_Age[ADEX_data$Child_Age == "6"] <-  "6-9 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "7"] <-  "6-9 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "8"] <-  "6-9 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "9"] <-  "6-9 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "12"] <- "12-15 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "13"] <- "12-15 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "14"] <- "12-15 Months"
ADEX_data$Child_Age[ADEX_data$Child_Age == "15"] <- "12-15 Months"
Android_data$`Formatted Start Time` <- as.POSIXct(Android_data$`Formatted Start Time`,
                                                  format = "%H:%M:%S (%d-%b-%Y)")
Android_data$`Formatted End Time` <- as.POSIXct(Android_data$`Formatted End Time`,
                                                format = "%H:%M:%S (%d-%b-%Y)") 
Android_data$Usage <- as.difftime(Android_data$Usage,
                                  format = "%S")
x5Min_Android$start_time <- as.POSIXct(x5Min_Android$start_time, 
                                       format = "%d-%b %H:%M") 
x5Min_Android$end_time <- as.POSIXct(x5Min_Android$end_time, 
                                     format = "%d-%b %H:%M")
x5Min_Android$duration <- as.difftime(x5Min_Android$duration,
                                      format = "%S", units = "secs")
x5Min_iPhone$duration <- as.difftime(x5Min_iPhone$duration,
                                     format = "%S", units = "secs")
Participant_data$Social_Media_Usage <- as.difftime(Participant_data$Social_Media_Usage,
                                                  format = "%H:%M", units = "secs")

#Android_SM_data (filter SM apps by row & select critical columns)
Android_data <- slice_head(Android_data, n = 50)
Android_SM_data <- Android_data %>%
  filter(grepl("Facebook|Instagram|Twitter|Snapchat|TikTok|Pinterest|Reddit|Tumblr|Quora|Telegram", 
               `Display Name`)) 
Android_SM_data <- aggregate(Android_SM_data$Usage, by=list(Child_ChildID=Android_SM_data$Child_ChildID), FUN=sum)
names(Android_SM_data)[2] <- "Social_Media_Usage"

#drop ADEX rows less than 300 duration and add Total AWC column
ADEX_data <- subset(ADEX_data, Audio_Duration ==300)
ADEX_Totals <- aggregate(ADEX_data$AWC, by=list(Child_ChildID=ADEX_data$Child_ChildID), FUN=sum)
ADEX_data <- full_join(ADEX_data, ADEX_Totals, by = "Child_ChildID")

#add phone type column to data
x5Min_Android$Phone_Type <- "Android"
x5Min_iPhone$Phone_Type <- "iPhone"
Android_SM_data$Phone_Type <- "Android"

######FINAL TIBBLES (Smartphone & Social_Media)######
ADEX_data <- ADEX_data %>% 
  mutate_at("Child_ChildID", str_replace, "C", "")
#Columns from ADEX data
AWC_5Min <- ADEX_data %>%
  select(AWC, Child_ChildID, Clock_Time_TZAdj) %>% 
  mutate_at("Child_ChildID", str_replace, "C", "")
names(AWC_5Min)[3] <- "start_time"

#Vectors: Infant_Ages & AWC_Total & Smartphone_Total 
Infant_Ages <- ADEX_data %>% 
  distinct(Child_ChildID, Child_Age) %>% 
  mutate_at("Child_ChildID", str_replace, "C", "")

AWC_Total <- ADEX_data %>% 
  distinct(Child_ChildID, x) %>% 
  mutate_at("Child_ChildID", str_replace, "C", "")

Infant_Genders <- ADEX_data %>% 
  distinct(Child_ChildID, Child_Gender, Child_Age) %>% 
  mutate_at("Child_ChildID", str_replace, "C", "")

Vectors_to_add <- full_join(Infant_Ages, AWC_Total)

#Join vectors to create final tibbles
Smartphone <- bind_rows(x5Min_Android, x5Min_iPhone)
Smartphone <- full_join(Smartphone, Vectors_to_add)
Smartphone <- inner_join(AWC_5Min, Smartphone, by = c( "Child_ChildID", "start_time"))

Smartphone_Totals <- Smartphone %>%                                        
  group_by(Child_ChildID) %>%                         
  summarise_at(vars(duration),              
               list(Smartphone_Total = sum))  
Smartphone <- full_join(Smartphone, Smartphone_Totals)

Social_Media <- full_join(Participant_data, Android_SM_data) %>%
  group_by(Child_ChildID, Phone_Type) %>%
  mutate_at("Child_ChildID", str_replace, "C", "") 
Social_Media <- full_join(Smartphone_Totals, Social_Media) %>% 
  group_by(Child_ChildID)
Social_Media <- full_join(Social_Media, AWC_Total)
Social_Media <- full_join(Social_Media, Infant_Ages)

#Rename columns for better interpretation in statistical formulas
names(Smartphone)[6] <- "Smartphone_5Min"
names(Smartphone)[2] <- "Participant"
names(Smartphone)[8] <- "Infant_Age"
names(Smartphone)[9] <- "AWC_Total"
names(Smartphone)[1] <- "AWC_5Min"

names(Social_Media)[1] <- "Participant"
names(Social_Media)[4] <- "Social_Media"
names(Social_Media)[5] <- "AWC_Total"
names(Social_Media)[6] <- "Infant_Age"

#convert character columns to factors
Smartphone <- as.data.frame(unclass(Smartphone),                    
                      stringsAsFactors = TRUE)
Social_Media <- as.data.frame(unclass(Social_Media),                     
                              stringsAsFactors = TRUE)

#data from prior stages not publicly available due to privacy concerns
setwd("C:/Users/casar/OneDrive/Desktop/Thesis Documents-LAPTOP-KKIED4QT")
write.csv(Smartphone, file="Smartphone_Public.csv")
write.csv(Social_Media, file="Social_Media_Public.csv")

#Q1: Smartphone_Total as numeric for t.test
Social_Media$Smartphone_Total <- as.numeric(Social_Media$Smartphone_Total)

#Q1: Replace all NA values with 0's
Social_Media <- replace(Social_Media, is.na(Social_Media), 0)

# Q1: Shapiro-Wilkes normality test for normality among Age Groups
Shapiro_Social_Media <- Social_Media
Shapiro_Social_Media$Smartphone_Total <- as.numeric(Shapiro_Social_Media$Smartphone_Total)
with(Shapiro_Social_Media, shapiro.test(Smartphone_Total[Infant_Age == "6-9 Months"]))#(p < .05 = cannot assume normality)
with(Shapiro_Social_Media, shapiro.test(Smartphone_Total[Infant_Age == "12-15 Months"]))#(p < .05 = cannot assume normality)

# Q1: F-Test to check equal variances 
Q1_ftest <- var.test(Smartphone_Total ~ Infant_Age, data = Social_Media)
Q1_ftest #(p < .05 = cannot assume equal variances among age groups)

#Q2: Levene's Test for Normality (p > .05 = assume normality)
leveneTest(AWC_Total ~ Infant_Age, data = Smartphone)

#Q4 social media data (participant 220 excluded)
Social_Media_29 <- subset(Social_Media, Participant != 220)

#Q4.5 social media data - no outlier (participant 169 excluded)
Social_Media_28 <- subset(Social_Media_29, Participant != 169)

#Remove mothers who experienced technical difficulties (Q1&Q2)
Social_Media <- subset(Social_Media, Participant != 220)
Social_Media <- subset(Social_Media, Participant != 237)
Smartphone <- subset(Smartphone, Participant != 220)
Smartphone <- subset(Smartphone, Participant != 237)

#Remove rows where Smartphone_5Min = 0 from Q3 
NewSmartphone <- subset(Smartphone, Smartphone_5Min != 0)

#Q4: Levene's Test for Normality (p > .05 = assume normality)
leveneTest(AWC_Total ~ Infant_Age, data = Social_Media_29)

#Q4.5: Levene's Test for Normality (p > .05 = assume normality)
leveneTest(AWC_Total ~ Infant_Age, data = Social_Media_28)
 
####### Research Question 1 (N = 28): ################################################
Q1 <- t.test(Smartphone_Total ~ Infant_Age, 
             data = Social_Media, 
             paired = FALSE, alternative = "two.sided", var.equal = FALSE)
####### Research Question 2 (N = 28): ################################################
Q2 <- lm(scale(AWC_Total, scale = FALSE) ~ scale(Smartphone_Total, scale = FALSE)*Infant_Age, 
            data = Social_Media)
####### Research Question 3 (N = 25): ################################################
Q3 <- lmer(AWC_5Min ~ Smartphone_5Min*Infant_Age + (1|Participant), 
         data = Smartphone)
####### Research Question 3.5 (No Smartphone_5Min = 0): #######################
Q3_No5Min0 <- lmer(AWC_5Min ~ Smartphone_5Min*Infant_Age + (1|Participant), 
           data = NewSmartphone)
#######Research Question 4 (N = 29): #################################################
Q4 <- lm(scale(AWC_Total, scale = FALSE) ~ scale(Social_Media, scale = FALSE)*Infant_Age, 
         data = Social_Media_29)
#######Research Question 4.5 (no outlier; N = 28): #####################################
Q4_No_Outlier <- lm(scale(AWC_Total, scale = FALSE) ~ scale(Social_Media, scale = FALSE)*Infant_Age, 
                data = Social_Media_28)


#check models
Q1
Q2
Q3
Q3_No5Min0
Q4
Q4_No_Outlier

#Q1 Statistics
Social_Media$Smartphone_Total <- as.numeric(Social_Media$Smartphone_Total)
Social_Media$AWC_Total <- as.numeric(Social_Media$AWC_Total)
Q1_Summary_Stats1 <- Social_Media %>%
  group_by(Infant_Age) %>%
  get_summary_stats(Smartphone_Total, type = "mean_sd")
Q1_Summary_Stats2 <- Social_Media %>%
  group_by(Infant_Age) %>%
  get_summary_stats(AWC_Total, type = "mean_sd")
Q1_Summary_Stats1
Q1_Summary_Stats2
Q1$p.value
Q1$estimate
Q1$conf.int
Q1_CohensD <- Social_Media %>% cohens_d(Smartphone_Total ~ Infant_Age, var.equal = FALSE)
Q1_CohensD
report(Q1)

#Q2 Statistics
TukeyHSD(Q2, "Infant_Age", ordered = TRUE)
anova(Q2)
summary(Q2)
report(Q2)

#Q3/3.5 Statistics
summary(Q3)
report(Q3)
report(Q3_No5Min0)

#Q4/4.5 Statistics
TukeyHSD(Q4, "Infant_Age", ordered = TRUE)
anova(Q4)
summary(Q4)
report(Q4)
TukeyHSD(Q4.5, "Infant_Age", ordered = TRUE)
anova(Q4.5)
summary(Q4_No_Outlier)
report(Q4_No_Outlier)

#Q1 Box Plot
Q1_boxplot <- boxplot(Smartphone_Total ~ Infant_Age, data = Social_Media,
        xlab = "Age Groups", ylab = "Smartphone Usage (seconds)",
        frame = FALSE, col = c("#00AFBB", "#FC4E07"))
Q1_boxplot
Q1_scatterplot <- ggplot(data = Social_Media, aes(x = Infant_Age, y = Smartphone_Total, color = Infant_Age)) +
  geom_point() +
  geom_smooth(method = "t.test", se = FALSE) +
  labs(x = "Infant Age Group",
       y = "Smartphone Total (seconds)") +
  scale_color_grey() + theme_bw()
Q1_scatterplot

#Q2 Box Plot & Scatterplot
Q2_boxplot <- ggplot(Smartphone, aes(x=Smartphone_Total, y=AWC_Total, fill=Infant_Age)) + 
  geom_boxplot() +
  facet_wrap(~Infant_Age) +
  labs(x = "Smartphone Usage (seconds)",
       y = "Adult Word Count")
Q2_boxplot
options("scipen" = 10)
Q2_scatterplot <- ggplot(data = Smartphone, aes(x = Smartphone_Total, y = AWC_Total, color = Infant_Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Total Smartphone Usage (seconds)",
       y = "Adult Word Count") +
  scale_color_grey()+ theme_bw()
Q2_scatterplot

#Q3 Scatterplot
Q3_scatterplot <- ggplot(data = Smartphone, aes(x = Smartphone_5Min, y = AWC_5Min, color = Infant_Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Maternal Smartphone Usage (seconds)",
       y = "Adult Word Count") +
  scale_color_grey()+ theme_bw()
Q3_scatterplot

#Q3.5 Scatterplot no rows where Smartphone_5Min = 0
Q3_scatterplot_No5Min0 <- ggplot(data = NewSmartphone, aes(x = Smartphone_5Min, y = AWC_5Min, color = Infant_Age)) +
  geom_point(size = 1.1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Maternal Smartphone Usage (seconds)",
       y = "Adult Word Count") +
  scale_color_grey()+ theme_bw()
Q3_scatterplot_No5Min0

My_Colours <- grey.colors(n = 29)

#Q4/4.5 Boxplots
Q4_boxplot <- ggplot(Social_Media_29, aes(x=Social_Media, y=AWC_Total, fill=Infant_Age)) +   
  geom_boxplot() +
  scale_x_continuous() +
  facet_wrap(~Infant_Age)  +
  labs(x = "Social Media Usage (seconds)",
       y = "Adult Word Count") +
  theme(text = element_text(size = 10))
(Q4_boxplot = Q4_boxplot + scale_fill_grey(start = 0, end = .9))
(Q4_boxplot = Q4_boxplot + theme_bw())

Q4_boxplot_no_outlier <- ggplot(SocialMedia_NoOutlier, aes(x=Social_Media, y=AWC_Total, fill=Infant_Age)) + 
  geom_boxplot() +
  facet_wrap(~Infant_Age)  +
  labs(x = "Social Media Usage (seconds)",
       y = "Adult Word Count")+
  theme(text = element_text(size = 10))
(Q4_boxplot_no_outlier = Q4_boxplot_no_outlier + scale_fill_grey(start = 0, end = .9))
(Q4_boxplot_no_outlier = Q4_boxplot_no_outlier + theme_bw())

Social_Media$Social_Media <- as.numeric(Social_Media$Social_Media)
Q4_Summary_Stats <- Social_Media %>%
  group_by(Infant_Age) %>%
  get_summary_stats(Social_Media, type = "mean_sd")
Q4_Summary_Stats

pwr.t.test(n = 15, d = 0.8, sig.level = 0.05, power = NULL, type = "two.sample")

#mean/standard deviation by age group
Young <- ADEX_data %>% filter(Child_Age < 10)
Old <- ADEX_data %>% filter(Child_Age > 10)
mean(Young$Child_Age)
sd(Young$Child_Age)
mean(Old$Child_Age)
sd(Old$Child_Age)

Smartphone_5Min_Totals <- Smartphone %>%                                       
  group_by(Infant_Age) %>%                         
  summarize(total = sum (Smartphone_5Min)) 

Smartphone_Totals <- Social_Media %>%                                       
  group_by(Infant_Age) %>%                         
  summarize(total = sum (Smartphone_Total)) 

AWC_Totals <- Social_Media %>%                                       
  group_by(Infant_Age) %>%                         
  summarize(total = sum (AWC_Total))

Females <- ADEX_data %>% group_by(Child_ChildID) %>% filter(Child_Gender == "FALSE")
Males <- ADEX_data %>% group_by(Child_ChildID) %>% filter(Child_Gender == "M")
Female_Total <- Females %>% 
  distinct(Child_ChildID, Child_Gender) 
Male_Total <- Males %>% 
  distinct(Child_ChildID, Child_Gender) 

Maternal_Ages <- c(32, 40, 30, 34, 28, 30, 46, 30, 30, 30, 28, 38, 30, 28, 39, 31, 
                   30, 25, 33, 32, 23, 36, 38, 31, 32, 32, 34, 39, 31, 31) 
mean(Maternal_Ages)
sd(Maternal_Ages)

YoungAgeAWC <- c(13847, 870, 10105, 7989, 6071, 4274, 6568, 9143, 3456, 4626, 3682, 6541, 7263, 7738, 3812)
sd(YoungAgeAWC)

Smartphone_Averages <- Smartphone %>%                                        
  group_by(Infant_Age) %>%                         
  summarise_at(vars(Smartphone_5Min),              
               list(Smartphone_Totals = sum)) 