# .libPaths("C:/R/RLib")

#load installed packages into R#
library(tidyverse)
library(readxl)
library(XLConnect)
library(openxlsx)
library(data.table)

##CDC###
TF_files <-  list.files('RAW/CDC/Final District reports', pattern="*.xlsx")
fullpath <- file.path('RAW/CDC/Final District reports', TF_files)

for (i in 1:length(fullpath))
{
  currentpath <- fullpath[i]
  df_TF <-  read_excel(currentpath, 
                           sheet = "Final District PRI", skip =5)
  if (file.exists(paste0('Outputs/TouchFoundation')))
  {
    write.csv(df_TF_CDC,paste('Outputs/TouchFoundation/TouchFoundation_CDC_',i,'.csv',sep=''))
    
  } 
  else 
  {
    dir.create(file.path(paste0('Outputs/TouchFoundation')))
    write.csv(df_TF_CDC,paste('Outputs/TouchFoundation/TouchFoundation_CDC_',i,'.csv',sep=''))
    
  }
}

###USAID##########

TF_files <-  list.files('RAW/USAID/District reports Final', pattern="*.xlsx")
fullpath <- file.path('RAW/USAID/District reports Final', TF_files)

for (i in 1:length(fullpath))
{
  currentpath <- fullpath[i]
  df_TF <-  read_excel(currentpath, 
                       sheet = "Final District PRI", skip =5)
  if (file.exists(paste0('Outputs/TouchFoundation')))
  {
    write.csv(df_TF,paste('Outputs/TouchFoundation/TouchFoundation_USAID_',i,'.csv',sep=''))
    
  } 
  else 
  {
    dir.create(file.path(paste0('Outputs/TouchFoundation')))
    write.csv(df_TF,paste('Outputs/TouchFoundation/TouchFoundation_USAID_',i,'.csv',sep=''))
    
  }
}


TF_files <- list.files('Outputs/TouchFoundation', pattern="*.csv")
fullpath <- file.path('Outputs/TouchFoundation', TF_files)
df_all <- do.call("rbind",lapply(fullpath,FUN=function(files){ read.csv(files)}))

write.csv(df_all,paste('Outputs/TouchFoundation.csv',sep=''),row.names=F)

