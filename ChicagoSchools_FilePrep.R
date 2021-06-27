#libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

#load data
demo <- read.csv("Chicago Only/Chicago_Public_Schools_-_School_Profile_Information_SY1819.csv")
score <- read.csv("Chicago Only/Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv") 
ill_score <- read_xlsx("Illinois Total/2019-Report-Card-Public-Data-Set.xlsx", sheet = 5)
ill_demo <- read_xlsx("Illinois Total/2019-Report-Card-Public-Data-Set.xlsx", sheet = 1)

#narrow down demo df
demo <- demo %>% 
  select(1, 31, 37) %>% 
  mutate(Non_white_percent = ((Student_Count_Total - Student_Count_White)/Student_Count_Total)*100) %>% 
  mutate(White_percent = ((Student_Count_White)/Student_Count_Total)*100) 

#narrow down score df
score <- score %>% 
  filter(Primary_Category == "HS") %>% 
  select(1:3, 20, 26, 32, 42, 96:101, 113:115, 116, 122, 129, 137, 157:162)

#make combined school rating score
 score <- score %>% 
   mutate(growth2 = case_when(Student_Growth_Rating == "AVERAGE" ~ 0, 
                              Student_Growth_Rating == "BELOW AVERAGE" ~ -1, 
                              Student_Growth_Rating == "ABOVE AVERAGE" ~ 1, 
                              T ~ NA_real_)) %>% 
   mutate(attain2 = case_when(Student_Attainment_Rating == "ABOVE EXPECTATIONS" ~ 1,
                              Student_Attainment_Rating == "BELOW EXPECTATIONS" ~ -1, 
                              Student_Attainment_Rating == "FAR BELOW EXPECTATIONS" ~ -2,
                              Student_Attainment_Rating == "MET EXPECTATIONS" ~ 0, 
                              T ~ NA_real_)) %>% 
   mutate(culture2 = case_when(Culture_Climate_Rating == "WELL ORGANIZED" ~ 1, 
                               Culture_Climate_Rating == "ORGANIZED" ~ 0, 
                               Culture_Climate_Rating == "MODERATELY ORGANIZED" ~ -1, 
                               Culture_Climate_Rating == "PARTIALLY ORGANIZED" ~ -2, 
                               T ~ NA_real_)) %>% 
   mutate(creative2 = case_when(Creative_School_Certification == "EXCELLING" ~ 2, 
                                Creative_School_Certification == "STRONG" ~ 1, 
                                Creative_School_Certification == "DEVELOPING" ~ -1, 
                                Creative_School_Certification == "EMERGING" ~ -2, 
                                T ~ NA_real_)) %>% 
   mutate(involved2 = case_when(School_Survey_Involved_Families == "VERY STRONG" ~ 2,
                                School_Survey_Involved_Families == "STRONG" ~ 1,
                                School_Survey_Involved_Families == "NEUTRAL" ~ 0,
                                School_Survey_Involved_Families == "NOT ENOUGH DATA" ~ 0,
                                School_Survey_Involved_Families == "WEAK" ~ -1,
                                T ~ NA_real_)) %>% 
   mutate(support2 = case_when(School_Survey_Supportive_Environment == "VERY STRONG" ~ 2, 
                               School_Survey_Supportive_Environment == "STRONG" ~ 1, 
                               School_Survey_Supportive_Environment == "NEUTRAL" ~ 0, 
                               School_Survey_Supportive_Environment == "WEAK" ~ -1, 
                               T ~ NA_real_)) %>% 
   mutate(ambitious2 = case_when(School_Survey_Ambitious_Instruction == "VERY STRONG" ~ 2, 
                                 School_Survey_Ambitious_Instruction == "STRONG" ~ 1, 
                                 School_Survey_Ambitious_Instruction == "NEUTRAL" ~ 0, 
                                 T ~ NA_real_)) %>% 
   mutate(leader2 = case_when(School_Survey_Effective_Leaders == "VERY STRONG" ~ 2, 
                              School_Survey_Effective_Leaders == "STRONG" ~ 1, 
                              School_Survey_Effective_Leaders == "NEUTRAL" ~ 0, 
                              School_Survey_Effective_Leaders == "NOT ENOUGH DATA" ~ 0, 
                              School_Survey_Effective_Leaders == "WEAK" ~ -1, 
                              T ~ NA_real_)) %>% 
   mutate(collaborate2 = case_when(School_Survey_Collaborative_Teachers == "VERY STRONG" ~ 2, 
                                   School_Survey_Collaborative_Teachers == "STRONG" ~ 1, 
                                   School_Survey_Collaborative_Teachers == "NEUTRAL" ~ 0, 
                                   School_Survey_Collaborative_Teachers == "NOT ENOUGH DATA" ~ 0, 
                                   School_Survey_Collaborative_Teachers == "WEAK" ~ -1, 
                                   T ~ NA_real_)) %>% 
   mutate(safty2 = case_when(School_Survey_Safety == "NEUTRAL" ~ 0, 
                             School_Survey_Safety == "WEAK" ~ -1, 
                             School_Survey_Safety == "VERY WEAK" ~ -2, 
                             T ~ NA_real_)) %>% 
   mutate(community2 = case_when(School_Survey_School_Community == "VERY STRONG" ~ 2, 
                                School_Survey_School_Community == "STRONG" ~ 1, 
                                School_Survey_School_Community == "NEUTRAL" ~ 0, 
                                School_Survey_School_Community == "NOT ENOUGH DATA" ~ 0, 
                                School_Survey_School_Community == "WEAK" ~ -1, 
                                T ~ NA_real_)) %>% 
   mutate(parent2 = case_when(School_Survey_Parent_Teacher_Partnership == "VERY STRONG" ~ 2,
                              School_Survey_Parent_Teacher_Partnership == "STRONG" ~ 1,
                              School_Survey_Parent_Teacher_Partnership == "NEUTRAL" ~ 0,
                              School_Survey_Parent_Teacher_Partnership == "NOT ENOUGH DATA" ~ 0,
                              School_Survey_Parent_Teacher_Partnership == "WEAK" ~ -1,
                              T ~ NA_real_)) %>% 
   mutate(facilities2 = case_when(School_Survey_Quality_Of_Facilities == "VERY STRONG" ~ 2, 
                                  School_Survey_Quality_Of_Facilities == "STRONG" ~ 1, 
                                  School_Survey_Quality_Of_Facilities == "NEUTRAL" ~ 0, 
                                  School_Survey_Quality_Of_Facilities == "NOT ENOUGH DATA" ~ 0, 
                                  School_Survey_Quality_Of_Facilities == "WEAK" ~ -1, 
                                  T ~ NA_real_)) %>% 
   mutate(attendance2 = case_when(Student_Attendance_Year_1_Pct > mean(score$Student_Attendance_Year_1_Pct, na.rm = T) ~ 1, 
     Student_Attendance_Year_1_Pct < mean(score$Student_Attendance_Year_1_Pct, na.rm = T) ~ -1,
     T ~ NA_real_)) %>% 
   mutate(dropout2 = case_when(One_Year_Dropout_Rate_Year_1_Pct < mean(score$One_Year_Dropout_Rate_Year_1_Pct, na.rm = T) ~ 1,
     One_Year_Dropout_Rate_Year_1_Pct > mean(score$One_Year_Dropout_Rate_Year_1_Pct, na.rm = T) ~ -1,
     T ~ NA_real_)) %>% 
   mutate(freshman2 = case_when(Freshmen_On_Track_School_Pct_Year_1 > mean(score$Freshmen_On_Track_School_Pct_Year_1, na.rm = T) ~ 1, 
                                Freshmen_On_Track_School_Pct_Year_1 < mean(score$Freshmen_On_Track_School_Pct_Year_1, na.rm = T) ~ -1,
     T ~ NA_real_)) %>% 
   mutate(PSAT92 = case_when(PSAT_Grade_9_Score_School_Avg > mean(score$PSAT_Grade_9_Score_School_Avg, na.rm = T) ~ 1, 
                             PSAT_Grade_9_Score_School_Avg < mean(score$PSAT_Grade_9_Score_School_Avg, na.rm = T) ~ -1,
                                T ~ NA_real_)) %>% 
   mutate(PSAT102 = case_when(PSAT_Grade_10_Score_School_Avg > mean(score$PSAT_Grade_10_Score_School_Avg, na.rm = T) ~ 1, 
                             PSAT_Grade_10_Score_School_Avg < mean(score$PSAT_Grade_10_Score_School_Avg, na.rm = T) ~ -1,
                             T ~ NA_real_)) %>% 
   mutate(SAT2 = case_when(SAT_Grade_11_Score_School_Avg > mean(score$SAT_Grade_11_Score_School_Avg, na.rm = T) ~ 1, 
                           SAT_Grade_11_Score_School_Avg < mean(score$SAT_Grade_11_Score_School_Avg, na.rm = T) ~ -1,
                              T ~ NA_real_)) 
score <-  score %>% 
   mutate(QualityScore = rowSums(score[27:45], na.rm = T))

#create new dataset with demo and score 
school <- score %>% select(1:3, 21:22, 25, 46) %>% 
  left_join(demo %>% select(1,4, 5)) %>% 
  filter(!is.na(QualityScore) & !is.na(Non_white_percent))

#Narrow Ill. score and demo datasets
county <- c("Cook", "DeKalb", "DuPage", "Grundy", "Kankakee", "Kane", "Kendall",
            "McHenry", "Will")

ill_school <- ill_demo %>% 
  filter(County %in% county & (`District Type` == "HIGH SCHOOL" |
                                 `School Type` == "HIGH SCHOOL")) %>% 
  select(RCDTS, `School Name`, District, City, County, '% Student Enrollment - White') %>% 
  rename(White_percent = '% Student Enrollment - White', SchoolName = 'School Name') %>% 
  left_join(ill_score %>%  
  mutate(SATtotal = `SAT Reading Average` + `SAT Math Average`) %>% 
   select(RCDTS, SATtotal)) %>% 
  mutate(Chicago = case_when(City == "Chicago" ~ 1, 
                              T ~ 0)) 

  

#### Plots ####

ggplot(school, aes(x= Non_white_percent, y = SAT_Grade_11_Score_School_Avg)) +
  geom_point()

ggplot(school, aes(x= Non_white_percent, y = QualityScore)) +
  geom_point()

ggplot(school, aes(x= White_percent, y = SAT_Grade_11_Score_School_Avg)) +
  geom_point()

ggplot(ill_school, aes(x= White_percent, y = SATtotal, color = as.factor(Chicago))) +
  geom_point()


