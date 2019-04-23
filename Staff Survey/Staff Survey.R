#Staff Survey
#Merge HRH COP data call and Facility 


# .libPaths("C:/R/RLib")

library(ICPIutilities)
library(tidyverse)
library(readxl)
library(stringr)

master <- read_excel("Raw/SA Facility Data File_20190320_HR DataCall_DW_mbh_Mar26.xlsx", sheet = "Full Dataset", skip = 3)

data_call <- read_excel("Raw/COP_Partner_Data_Call_v3_MasterList.xlsx", sheet = "2. HRH Review", skip = 3)

facility <- read_excel("../Siyenza/Resources/Site List_CDC_Siyenza_with Site Leads_March 26 2019.xlsx", sheet = "Frenzy_Siyzenza_CDC")

df_facility <- facility %>% 
  filter(`Siyenza Site` %in% c("Yes")) %>% 
  select(Facility, `Siyenza Site`)
  

df_data_call <- data_call %>% 
  gather(type,value, 'NDOH Staff':'DSP Staff as of March 1, 2019') %>% 
  filter(type == 'DSP Staff as of March 1, 2019' ) %>% 
  select(Facility,Cadre,type, value)

df_data_call_nurses <- df_data_call %>% 
  # filter(Cadre %in% c('Professional Nurse','Enrolled Nurse')) %>% 
  filter(Cadre %in% c('Professional Nurse')) %>% 
  select(Facility, value) %>% 
  group_by_if(is.character) %>% 
  summarize(data_call_nurses = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  replace(., is.na(.), "")

df_nurses <-  master %>% 
  left_join(df_data_call_nurses, by = c("sitename"="Facility")) %>% 
  # replace(., is.na(.), "") %>% 
  rename(nurses_total_recommended = `Nurse - SDI`,
         nurses_current_staff= data_call_nurses) %>%
  mutate(nurses_total_recommended = ceiling(nurses_total_recommended),
         nurses_current_staff = ceiling(nurses_current_staff),
         # Cadre = "Nurses",
         nurses_DSP_update = "",
         nurses_PEPFAR_Calc = ifelse(is.na(nurses_total_recommended),0,nurses_total_recommended) - ifelse(is.na(nurses_current_staff),0,nurses_current_staff) ,
         nurses_DSP_plans_to_hire = "",
         nurses_Justification = "",
         nurses_Expected_Date_Hiring = "",
         # nurses_number_NDOH = "",
         # nurses_average_NDOH_LOE = "",
         # nurses_NDOH_vacancies = "",
         nurses_Total_Number_Hired = "")%>% 
  select(Partner, psnu, sitename, 
         # Cadre,
         nurses_total_recommended,
         nurses_current_staff,
         nurses_DSP_update,
         nurses_PEPFAR_Calc,
         nurses_DSP_plans_to_hire,
         nurses_Justification,
         nurses_Expected_Date_Hiring,
         # nurses_number_NDOH,
         # nurses_average_NDOH_LOE ,
         # nurses_NDOH_vacancies ,
         nurses_Total_Number_Hired) 
  

df_lay_counselors <-  master %>% 
  # replace(., is.na(.), "") %>% 
  rename(lay_total_recommended = `Lay Counselor (Case Manager) Recalculation - Phase 1`) %>% 
         # lay_PEPFAR_Calc = lay_total_recommended) %>%
  mutate(lay_total_recommended = round(lay_total_recommended),
         lay_current_staff = 0,
         # Cadre = "Lay Counselors",
         lay_DSP_update = "",
         lay_PEPFAR_Calc = round(lay_total_recommended),
         lay_DSP_plans_to_hire = "",
         lay_Justification = "",
         lay_Expected_Date_Hiring = "",
         # lay_number_NDOH = "",
         # lay_average_NDOH_LOE = "",
         # lay_NDOH_vacancies = "",
         lay_Total_Number_Hired = "")%>% 
  select(Partner, psnu, sitename, 
         # Cadre,
         lay_total_recommended,
         lay_current_staff,
         lay_DSP_update,
         lay_PEPFAR_Calc,
         lay_DSP_plans_to_hire,
         lay_Justification,
         lay_Expected_Date_Hiring,
         # lay_number_NDOH,
         # lay_average_NDOH_LOE,
         # lay_NDOH_vacancies,
         lay_Total_Number_Hired) 



df_chw <-  master %>% 
  # replace(., is.na(.), "") %>% 
  rename(chw_total_recommended = `Total Full-Time CHW Needed`,
         chw_current_staff = `Current CHW (1 WBPHCOT  = 0.5 FTE)`,
         chw_PEPFAR_Calc = `CHW for DSP to Hire`) %>%
  mutate(chw_total_recommended = round(chw_total_recommended),
         chw_current_staff = round(chw_current_staff),
         # Cadre = "Lay Counselors",
         chw_DSP_update = "",
         chw_PEPFAR_Calc = round(chw_PEPFAR_Calc),
         chw_DSP_plans_to_hire = "",
         chw_Justification = "",
         chw_Expected_Date_Hiring = "",
         # chw_number_NDOH = "",
         # chw_average_NDOH_LOE = "",
         # chw_NDOH_vacancies = "",
         chw_Total_Number_Hired = "")%>% 
  select(Partner, psnu, sitename, 
         # Cadre,
         chw_total_recommended,
         chw_current_staff,
         chw_DSP_update,
         chw_PEPFAR_Calc,
         chw_DSP_plans_to_hire,
         chw_Justification,
         chw_Expected_Date_Hiring,
         # chw_number_NDOH,
         # chw_average_NDOH_LOE,
         # chw_NDOH_vacancies,
         chw_Total_Number_Hired) 

df_final <- bind_cols(df_nurses,df_lay_counselors, df_chw) %>% 
  inner_join(df_facility, by = c("sitename" = "Facility")) %>% 
  select(-Partner1,-Partner2, -psnu1,-psnu2, -sitename1, -sitename2,-`Siyenza Site`) %>% 
  replace(., is.na(.), "") 


write.table(df_final, "Output/output_datacall.txt", sep = "\t", row.names = FALSE)
