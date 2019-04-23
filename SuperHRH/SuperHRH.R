# .libPaths("C:/R/RLib")

#load installed packages into R#
library(tidyverse)
library(readxl)


msd_siyenza <- read_tsv("../../COP19/Output/MSD_Siyenza.txt")

df_msd_siyenza <-  msd_siyenza %>% 
  gather(indicator, value,fy2018apr_HTS_TST_POS:Siyenza_uLTFU, na.rm = T ) %>% 
  mutate(indicator = paste0("msd_siyenza|", indicator, "|Total")) %>% 
  spread(indicator, value)

df_touchfoundation <-  read.csv("Touch Foundation/Outputs/TouchFoundation.csv") %>% 
  select(Facility, `Cadre.type`) %>% 
  mutate(`Cadre.type` = as.character(`Cadre.type`),
         `Cadre.type` = case_when(`Cadre.type` == "Pharmacist" ~ "Pharmacy",
                                  TRUE ~ `Cadre.type`)) %>% 
  group_by(Facility,`Cadre.type`) %>% 
  summarize(value= n()) %>% 
  ungroup() %>% 
  mutate(source = "TF", cadre = `Cadre.type`, disaggregate = "Total") %>% 
  select(-`Cadre.type`) %>% 
  rename_all(~tolower(.)) 

df_ra <-   read_excel("Raw/HRH_RA_long_targets_20180814_ref.xlsx", sheet = "data") %>% 
  filter(support_type %in% c("oth","sag","usg"), fte != 0,
         cadre_label %in% c("data capturer"        ,        
                            "doctor"               ,        
                            "staff/enrolled nurse" ,        
                            "lay counselor"        ,        
                            "professional nurse"   ,        
                            "CHW/CCG"              ,        
                            "pharmacist"           ,        
                            "linkage officer/case officer" )) %>% 
  mutate(cadre_label= case_when(cadre_label == "data capturer"                 ~ 'Data Capturer',
                                cadre_label == "doctor"                        ~ 'Medical Officer',
                                cadre_label == "staff/enrolled nurse"          ~ 'Enrolled Nurse',
                                cadre_label == "lay counselor"                 ~ 'Lay Counselor',
                                cadre_label == "professional nurse"            ~ 'Professional Nurse',
                                cadre_label == "CHW/CCG"                       ~ 'Community Health Worker',
                                cadre_label == "pharmacist"                    ~ 'Pharmacy',
                                cadre_label == "linkage officer/case officer"  ~ 'Linkage Officer',
                                TRUE ~ cadre_label),
    source  = paste0("HRH_RA_FTE_",support_type), cadre =  cadre_label,
         mechanismid = as.character(mechanismid),disaggregate = "Total") %>% 
         # indicator =  paste0(source,"_",support_type , "_", cadre_label)) %>% 
  select(source,
         facility,
         # fundingagency, primepartner = prime_partner, mechanismid,
         cadre, 
         disaggregate, 
         # categoryoptioncomboname = cadre_cat, 
         value = fte) %>% 
  group_by_if(is.character) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  ungroup()


df_HRID <- read_excel("Raw/FY19Q1_FactView as at 7 March 2019 at 15h45.xlsx", sheet = "qryFactView_Dataset") %>% 
  rename_all(~tolower(.)) %>% 
    gather(period, value, starts_with("fy20"), na.rm = TRUE) %>%
    filter(indicator %in% "HRH_HRID_FTE") %>% 
    mutate( source = "HRID",
            sitename = case_when(is.na(facility) ~ community, TRUE ~ facility),
            mechanismid = as.character(mechanismid),
            cadre =  disaggregate,
            disaggregate = "Total") %>% 
  filter(cadre %in% c("** Medical Doctors" ,
                             # "** Medical specialists",
                             "** Nurse-Professional",
                             "** Nurse - Professional",
                             "** Enrolled nurse",
                             "** Lay Counselor",
                             "** Lay testing counselor",
                             "** Data Capturer",
                             "** Retention officer",
                             "** Pharmaceutical and Related Professionals",
                             "** Pharmaceutical assistants" ,
                             "** Pharmaceutical Assistant",
                             "** Pharmacists" ,
                             "** Community health workers",
                             "** Outreach Team Leader - OTL"),
         period %in% "fy2019q1") %>% 
  mutate(cadre= case_when(cadre  == "** Data Capturer"                                           ~ 'Data Capturer',
                          cadre  == "** Medical Doctors"                                         ~ 'Medical Officer',
                          cadre  == "** Enrolled nurse"                                          ~ 'Enrolled Nurse',
                          cadre  %in% c("** Lay Counselor", "** Lay testing counselor")          ~ 'Lay Counselor',
                          cadre  %in% c("** Nurse-Professional", "** Nurse - Professional")      ~ 'Professional Nurse',
                          cadre  %in% c("** Community health workers", "** Community navigator") ~ 'Community Health Worker',
                          cadre  %in% c("** Pharmaceutical and Related Professionals",
                                                     "** Pharmaceutical assistants",
                                                     "** Pharmaceutical Assistant",
                                                     "** Pharmacists")                           ~ 'Pharmacy',
                          cadre  == "** Outreach Team Leader - OTL"                              ~ "Outreach Team Lead",
                          cadre  == "** Retention officer"                                       ~ "Linkage Officer",
                          TRUE ~ cadre)) %>% 
  select(facility,source, cadre,disaggregate, value) %>% 
  group_by_if(is.character) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  ungroup() 


df_staff_survey <-   read_excel("Raw/Staff Survey Output 20190423_IPsMERGED.xlsx") %>% 
  select(-`PEPFAR-funded Nurses|Justification, if DSP plan differs from PEPFAR Calculation`,
         -`PEPFAR-funded Nurses|Expected date of Hiring Completed`,
         -`Lay Counselor (Case Manager)|Expected date of Hiring Completed`,
         -`Lay Counselor (Case Manager)|Justification, if DSP plan differs from PEPFAR Calculation`,
         -`CHW |Justification, if DSP plan differs from PEPFAR Calculation`,
         -`CHW |Expected date of Hiring Completed`,
         -`OTL|Expected date of Hiring Completed`) %>%
  gather(indicator, value, `PEPFAR-funded Nurses|Total PEPFAR-Staff Recommended`:`OTL|Total Number of Staff Hired (by April 19th)`, na.rm = TRUE ) %>% 
  separate(indicator, c("cadre", "disaggregate"), sep = "[|]") %>% 
  filter(value != "n/a") %>% 
  mutate(source = "StaffSurvey",
         cadre = case_when(cadre  == "PEPFAR-funded Nurses"~ 'Professional Nurse',
                           cadre  == "CHW "  ~ 'Community Health Worker',
                           cadre  == "OTL"   ~ 'Outreach Team Lead',
                           cadre  == "Lay Counselor (Case Manager)"   ~ 'Lay Counselor',
                           TRUE ~ cadre),
         value= as.numeric(value)) %>% 
  select(`Site Name`, source, cadre,disaggregate, value) %>% 
  rename(facility =`Site Name`) %>% 
  mutate(disaggregate = case_when(cadre == "Professional Nurse" & disaggregate == "Current PEPFAR Staff in Place" ~ "Staff_Feburary19",
                                  cadre == "Professional Nurse" & disaggregate == "DSP Update to Current Staff in Place" ~ "Staff_March19",
                                  cadre == "Professional Nurse" & disaggregate == "Number of Staff DSP Plans to Hire" ~ "DSP_Hiring_plan",
                                  cadre == "Professional Nurse" & disaggregate == "PEPFAR Calculation of Staff to be Hired" ~ "PEPFAR_Hiring_calc",
                                  cadre == "Professional Nurse" & disaggregate == "Total Number of Staff Hired (by April 19th)" ~ "Total_Plan_April19",
                                  cadre == "Professional Nurse" & disaggregate == "Total PEPFAR-Staff Recommended" ~ "Total_PEPFAR_rec_April19",
                                  
                                  
                                  cadre == "Lay Counselor" & disaggregate == "Current PEPFAR Staff in Place" ~ "Staff_Feburary19",
                                  cadre == "Lay Counselor" & disaggregate == "DSP Update to Current Staff in Place" ~ "Staff_March19",
                                  cadre == "Lay Counselor" & disaggregate == "Number of Staff DSP Plans to Hire" ~ "DSP_Hiring_plan",
                                  cadre == "Lay Counselor" & disaggregate == "PEPFAR Calculation of Staff to be Hired" ~ "PEPFAR_Hiring_calc",
                                  cadre == "Lay Counselor" & disaggregate == "Total Number of Staff Hired (by April 19th)" ~ "Total_Plan_April19",
                                  cadre == "Lay Counselor" & disaggregate == "Total PEPFAR-Staff Recommended" ~ "Total_PEPFAR_rec_April19",
                                  
                                  
                                  cadre == "Community Health Worker" & disaggregate == "Current CHW in Place" ~ "Staff_Feburary19",
                                  cadre == "Community Health Worker" & disaggregate == "DSP Update to Current Staff in Place" ~ "Staff_March19",
                                  cadre == "Community Health Worker" & disaggregate == "Number of Staff DSP Plans to Hire" ~ "DSP_Hiring_plan",
                                  cadre == "Community Health Worker" & disaggregate == "PEPFAR Calculation of Staff to be Hired" ~ "PEPFAR_Hiring_calc",
                                  cadre == "Community Health Worker" & disaggregate == "Total Number of Staff Hired (by April 19th)" ~ "Total_Plan_April19",
                                  cadre == "Community Health Worker" & disaggregate == "Total PEPFAR-Staff Recommended" ~ "Total_PEPFAR_rec_April19",
                                  
                                  
                                  cadre == "Outreach Team Lead" & disaggregate == "DSP-Funded OTLs in Place" ~ "Staff_Feburary19",
                                  cadre == "Outreach Team Lead" & disaggregate == "DSP-Funded OTL's to be Hired" ~ "DSP_Hiring_plan",
                                  cadre == "Outreach Team Lead" & disaggregate == "Total Number of Staff Hired (by April 19th)" ~ "Total_Plan_April19",
                                  
                                  TRUE ~ disaggregate))

data_call <- read_excel("Raw/COP_Partner_Data_Call_v3_MasterList.xlsx", sheet = "2. HRH Review", skip = 3)


df_data_call <- data_call %>% 
  gather(type,value, 'NDOH Staff':'DSP Staff as of March 1, 2019') %>% 
  filter(type == 'DSP Staff as of March 1, 2019' ) %>% 
  mutate(source = "DataCall",
         cadre = case_when(Cadre == "Doctor" ~ "Medical Officer",
                           TRUE ~ Cadre),
         disaggregate = "Total") %>% 
  select(Facility,source,cadre,disaggregate, value) %>% 
  rename_all(~tolower(.)) %>% 
  group_by_if(is.character) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  ungroup() 

df_all <-  bind_rows(df_ra, df_HRID, df_staff_survey, df_touchfoundation, df_data_call)


df_final <-  df_all %>% 
  mutate(indicator = paste0(source, "|",cadre, "|", disaggregate)) %>%
  select(facility, indicator, value) %>% 
  spread(indicator,value) %>% 
  left_join(df_msd_siyenza, by = c("facility" = "sitename")) %>% 
  select(facility,snu1, psnu, fundingagency, primepartner,siyenza_site,
         `DataCall|Data capturer|Total`:`TF|Professional Nurse|Total`,
         `msd_siyenza|fy2018apr_HTS_TST_POS|Total`:`msd_siyenza|Siyenza_uLTFU|Total`) %>% 
  gather(indicator, value, `DataCall|Data capturer|Total`:`msd_siyenza|Siyenza_uLTFU|Total`, na.rm= T) %>% 
  separate(indicator, c("source", "indicator/cadre","disaggregate"), sep = "[|]") %>% 
  replace(., is.na(.), "")
  

# filter(siyenza_site %in% "yes")


write.table(df_final, "Output/SuperHRH.txt", sep = "\t", row.names = FALSE)

