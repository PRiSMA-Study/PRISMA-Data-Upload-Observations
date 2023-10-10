#*****************************************************************************
#*site upload table
#*UPDATE EACH RUN: Update upload date at lines 34, 60, 86, 112, and 138 
#*input: data from TNT drive 
#*output: 1. number of observations each form
#*        2. last day of enrollment based on MNH02
#*        3. number of enrolled based on MNH02
#*Customize each time: 1. set upload date (rows 16-21)
#*Customize once: 1. uncomment the chunk of codes for India CMC and India SAS when 
#*                   data are available. (row 134-184, 211-283, 230-231)
#*                2. replace "SITE" with real name as in synapse for site India CMC
#*                   and India SAS. (row 133, 159, 209, 216)
#*****************************************************************************
rm(list = ls())
library(tidyverse)
library(data.table)
library(lubridate)

# set upload date (check AND UPDATE EACH RUN)
UploadDate_PA = "2023-10-06" #Pakistan
UploadDate_KE = "2023-10-06" #Kenya
UploadDate_ZA = "2023-10-06" #Zambia
UploadDate_GH = "2023-09-29" #Ghana
UploadDate_IC = "2023-10-06" #India CMC
UploadDate_IS = "yyyy-mm-dd" #India SAS

#*****************************************************************************
#*set directory and read data for each site
#*Calculate total observations for each form
#*****************************************************************************
#************************Pakistan************************
# set working directory to network drive 
site = "Pakistan"
setwd(paste("Z:/SynapseCSVs/Pakistan/2023-10-06"))

# list all csv files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

# revise all names to upper case 
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

# add filename mnh##_SITE
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)

#calcuate total obs for each form
obs_PA <-lapply(myfiles,function(x){
  nrow(x)
})

#list out data
list2env(myfiles, globalenv())

#************************Kenya************************
# set working directory to network drive 
site = "Kenya"
setwd(paste("Z:/SynapseCSVs/Kenya/2023-10-06"))

# list all csv files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

# revise all names to upper case 
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

# add filename mnh##_SITE
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)

#calcuate total obs
obs_KE <- lapply(myfiles,function(x){
  nrow(x)
})

#list out data
list2env(myfiles, globalenv())

#************************Zambia************************
# set working directory to network drive 
site = "Zambia"
setwd(paste("Z:/SynapseCSVs/Zambia/2023-10-06"))

# list all csv files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

# revise all names to upper case 
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

# add filename mnh##_SITE
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)

#calcuate total obs
obs_ZA <- lapply(myfiles,function(x){
  nrow(x)
})

#list out data
list2env(myfiles, globalenv())

#************************Ghana************************
# set working directory to network drive 
site = "Ghana"
setwd(paste("Z:/SynapseCSVs/Ghana/2023-09-29"))

# list all csv files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

# revise all names to upper case 
myfiles <- lapply(myfiles, function (x){
  upper <- toupper(names(x))
  setnames(x, upper)
})

# add filename mnh##_SITE
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)

#calcuate total obs
obs_GH <- lapply(myfiles,function(x){
  nrow(x)
})

#list out data
list2env(myfiles, globalenv())

# #************************India CMC************************
# # set working directory to network drive 
site = "India" #revise site name for India CMC as in synapse, not decided at this point
setwd(paste("Z:/SynapseCSVs/India_CMC/2023-10-06"))
# 
# # list all csv files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
# 
# # revise all names to upper case 
myfiles <- lapply(myfiles, function (x){
   upper <- toupper(names(x))
   setnames(x, upper)
 })
# 
# # add filename mnh##_SITE
names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
# 
# #calcuate total obs
obs_IC <- lapply(myfiles,function(x){
   nrow(x)
 })
# 
# #list out data
list2env(myfiles, globalenv())
# 
# #************************India SAS************************
#   # set working directory to network drive 
#   site = "SITE" #revise site name for India SAS as in synapse, not decided at this point
# setwd(paste("Z:/SynapseCSVs/",site,"/",UploadDate_IS, sep = ""))
# 
# # list all csv files
# temp = list.files(pattern="*.csv")
# myfiles = lapply(temp, read.csv)
# 
# # revise all names to upper case 
# myfiles <- lapply(myfiles, function (x){
#   upper <- toupper(names(x))
#   setnames(x, upper)
# })
# 
# # add filename mnh##_SITE
# names(myfiles) <- gsub(".csv", paste("_",site, sep = ""), temp)
# 
# #calcuate total obs
# obs_IS <- lapply(myfiles,function(x){
#   nrow(x)
# })
# 
# #list out data
# list2env(myfiles, globalenv())

#**************last enrollment day and total enrolled based on MNH02**************
lastPA <- mnh02_Pakistan %>%
  filter(CONSENT_IEORRES == 1) %>% 
  mutate(lastday = max(dmy(SCRN_OBSSTDAT)),
         date = dmy(SCRN_OBSSTDAT)) 
table(lastPA$lastday)

lastKE <- mnh02_Kenya %>%
  filter(CONSENT_IEORRES == 1) %>% 
  mutate(lastday = max(dmy(SCRN_OBSSTDAT)),
         date = dmy(SCRN_OBSSTDAT)) 
table(lastKE$lastday)

lastGH <- mnh02_Ghana %>%
  filter(CONSENT_IEORRES == 1) %>% 
  mutate(lastday = max(dmy(SCRN_OBSSTDAT)),
         date = dmy(SCRN_OBSSTDAT)) 
table(lastGH$lastday)

lastZA <- mnh02_Zambia %>%
  filter(CONSENT_IEORRES == 1) %>% 
  mutate(lastday = max(dmy(SCRN_OBSSTDAT)),
         date = dmy(SCRN_OBSSTDAT)) 
table(lastZA$lastday)

lastIC <- mnh02_India %>%
  filter(CONSENT_IEORRES == 1) %>% 
  mutate(lastday = max(ymd(SCRN_OBSSTDAT)),
         date = ymd(SCRN_OBSSTDAT)) 
table(lastIC$lastday)

lastIC <- mnh02_India %>%
   filter(CONSENT_IEORRES == 1) %>% 
   mutate(lastday = max(dmy(SCRN_OBSSTDAT)),
          date = dmy(SCRN_OBSSTDAT)) 
 table(lastIC$lastday)
# 
# #customize mnh02_SITE by using synapse name of site India SAS
# lastIS <- mnh02_SITE %>%
#   filter(CONSENT_IEORRES == 1) %>% 
#   mutate(lastday = max(dmy(SCRN_OBSSTDAT)),
#          date = dmy(SCRN_OBSSTDAT)) 
# table(lastZA$lastday)

#************************# of obs in each form************************
print(obs_PA)
print(obs_KE)
print(obs_GH)
print(obs_ZA)
print(obs_IC)
# print(obs_IS)
