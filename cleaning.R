setwd("C:/Users/Lenovo/Documents/WASH/7.Cleaning/cleaning")
source("setup.R")
source("cleaning_functions.R")
set.up.cleaning.dir("raw_data")
dir <- "raw_data/20191016"
#kobo.xlsx.to.csv(dir, "(DRAFT1) WASH CLEANING", anonymise=F)

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


## Employment as primary source of livelihood but 0 income in previous 30 days from employment
flag <- data_new$primary_livelihood.employment == 1 & data_new$inc_employment < 1000
data_new[which(flag), c("primary_livelihood.employment", "inc_employment")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",
                                    question.name="inc_employment",
                                    issue="Employment is selected as primary livelihood source but income from employment is given as 0.",
                                    dir = dir)

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
# Location Change due to incorrect KML Files before 22nd September
flag <- data_new$cluster_location_id %in% c("cluster_location_id_0752") & data_new$date_assessment=="2019-09-22"
data_new[which(flag), c("cluster_location_id", "date_assessment")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, psu, uuid, action = "change",
                                    question.name="cluster_location_id",
                                    issue="Incorrect KML File",
                                    dir = dir, new.value = "cluster_location_id_0785")


# data frame with cluster id pairs as rows
cluster_id_changes<-data.frame(
  old = c("cluster_location_id_0752", "cluster_location_id_0744", "cluster_location_id_0730", "cluster_location_id_0736", "cluster_location_id_0754", "cluster_location_id_0756", "cluster_location_id_0755", "cluster_location_id_0742", "cluster_location_id_0738"),
  new = c("cluster_location_id_0785","cluster_location_id_0785","cluster_location_id_0785","cluster_location_id_0785", "cluster_location_id_0787","cluster_location_id_0789", "cluster_location_id_0788", "cluster_location_id_0775","cluster_location_id_0775"), 
 stringsAsFactors=FALSE
)

data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0752", 
                                   "cluster_location_id_0785", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0744", 
                                   "cluster_location_id_0785", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0730", 
                                   "cluster_location_id_0785", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0736", 
                                   "cluster_location_id_0785", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0754", 
                                   "cluster_location_id_0787", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0756", 
                                   "cluster_location_id_0789", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0755", 
                                   "cluster_location_id_0788", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0742", 
                                   "cluster_location_id_0775", data$cluster_location_id)
data$cluster_location_id <- ifelse(data$date_assessment == "2019-09-19" | 
                                     data$date_assessment == "2019-09-20" |
                                     data$date_assessment == "2019-09-21" |
                                     data$date_assessment == "2019-09-22" & 
                                     data$cluster_location_id == "cluster_location_id_0738", 
                                   "cluster_location_id_0775", data$cluster_location_id)

# data$cluster_location_id == "cluster_location_id_0752"

dates_to_change<-c("2019-09-19","2019-09-20","2019-09-21", "2019-09-22")


# look up new IDs
new_ids <- cluster_id_changes$new[match(data_new$cluster_location_id, cluster_id_changes$old)]

# turn to NA those ids that were not on the right date
new_ids[!(data$date %in% dates_to_change)] <- NA 

changes<-data.frame(
  new_values = new_ids,
  uuids = data$X_uuid,
  stringsAsFactors=FALSE)

# get rid of NAs
changes<-changes[!is.na(changes$new_values) , ]

# for each new_value <-> uuid pair, run log.cleaning.change.extended function

changelog <- purrr::map2(changes$new_values,changes$uuids,function(value,uuid){
  
  log.cleaning.change.extended(new_data, psu, uuid, action = "change",
                               question.name="cluster_location_id",
                               issue="Incorrect KML File",
                               dir = dir, new.value = value)
  
})


# map2 returned a list with one item per iteration, let's rbind them into a single table

changelog <- do.call(rbind, changelog)

execute.cleaning.changes(dir)
remove.contact.cleaned.data(dir,name = "data")
anonymise.cleaned.data(dir,name = "data")


#always run for translation 

data <- read.xlsx(sprintf("%s/data.xlsx",dir),sheetIndex = 1, encoding = "UTF-8")
write.csv(data, sprintf("%s/data_for_translation.csv",dir), row.names = F)
## translating other...
data <- read.csv(sprintf("%s/data_for_translation.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
dir <- "raw_data/20190930"
# Just todays date translations
data_old <- read.csv(sprintf("%s/data_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_new <- data %>% filter((!X_uuid %in% data_old$X_uuid))# & (as.Date(date_assessment) < as.Date(ignore_date)))
# All 
result <- translate.others.arabic(data)
dir <- "raw_data/20190930"
write.csv(result, sprintf("%s/translations.csv", dir), row.names = F, fileEncoding = "UTF-8")
uq <- unique(result$question.name)
for (i in 1:length(uq)){
  uuid <- result$uuid[which(result$question.name == uq[i])]
  log <- log.cleaning.change.extended(data, psu, uuid = uuid, action = "f",  
                                      question.name=uq[i], 
                                      issue="Other text, to be recoded",
                                      dir = dir)
}
