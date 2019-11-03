setwd("C:/Users/Lenovo/Documents/WASH/7.Cleaning/cleaning/ki/outofcamp")
source("setup_ki_out.R")
source("cleaning_functions_KI.R")
set.up.cleaning.dir("raw_data")
dir <- "raw_data/20191030"

data <- read.csv(sprintf("%s/data.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_remainee == 1, "host", NA))))
data_old <- read.csv(sprintf("%s/data_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_new <- data %>% filter((!X_uuid %in% data_old$X_uuid))# & (as.Date(date_assessment) < as.Date(ignore_date)))

#ignore_date <- c("2019-08-07")
#data <- data %>% filter(as.Date(date_assessment) < as.Date(ignore_date)) 

## Overview of dates
dates <- read.csv(sprintf("%s/surveys_cleaned_old.csv", dir), stringsAsFactors = F)
dates2 <- data %>% dplyr::select(date_assessment) %>% table
dates2 - c(dates$Freq, rep(0,length(dates2) - nrow(dates)))

write.csv(dates2, sprintf("%s/surveys_cleaned.csv", dir), row.names = F)
dates2 <- data %>% dplyr::select(date_assessment, population_group) %>% table

#unique uuid's
dup <- find_duplicates_uuid(data)
which(data$X_uuid %in% dup$value)
uuid <- dup$value
log <- log.cleaning.change.extended(data_new, psu, uuid, action = "f",
                                    question.name="X_uuid",
                                    issue="UUID Duplicates",
                                    dir = dir)

## interview speed
times <- difftime(as.POSIXct(data$end,format="%Y-%m-%dT%H:%M:%OS"), 
                  as.POSIXct(data$start,format="%Y-%m-%dT%H:%M:%OS"), units='mins')
median(times)
hist(as.numeric(times), breaks=c(min(times),0,10,20,30,40,50,60,70,80,90,120,max(times)),
     xlim=c(-30,150), labels=T, freq=T)


# Sufficient water access overall but insufficient drinking water access 
flag <- data_new$sufficient_access %in% c("morethan_sufficient") & data_new$water_needs.drinking==0
data_new[which(flag), c("sufficient_access", "water_needs.drinking")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, psu, uuid, action = "f",
                                    question.name="water_needs.drinking",
                                    issue="More than sufficient overall access but no access to drinking water",
                                    dir = dir)
log <- log.cleaning.change.extended(data_new, psu, uuid, action = "change",
                                    question.name="sufficient_access",
                                    issue="Mor e than sufficient overall access but no access to drinking water",
                                    dir = dir, new.value = "sufficient")

# Selecting doesn't treat water but then selects a treatment method
flag <- data_new$treat_water %in% c("never") &  
  rowSums(data_new[,which(startsWith(names(data_new), "treat_techniques."))]) > 0
data_new[which(flag), c("treat_water", "treat_techniques")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, psu, uuid, action = "change",
                                    question.name="treat_techniques",
                                    issue="Does not treat water but selects treatment method",
                                    dir = dir, new.value = NA)

# Currently visible sewage in the area now but selecting no visible sewage present over the last 30 days in the area
flag <- data_new$visible_sewage %in% c("yes") & data_new$sewage_visability=="never"
data_new[which(flag), c("visible_sewage", "sewage_visability")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, psu, uuid, action = "change",
                                    question.name="sewage_visability",
                                    issue="Sewage visible but then selected never over the last 30 days",
                                    dir = dir, new.value = "sometimes")

execute.cleaning.changes(dir)
remove.contact.cleaned.data(dir,name = "data")
anonymise.cleaned.data(dir,name = "data")
