### This file prepares the data for computing government positions from party-level data
# Load required R libraries -------------------------------------------
#load, or first install and then load, a package for loading (and instialling if needed) packages
if (!require(install.load)) {
  install.packages("install.load", repos = "http://cran.us.r-project.org")
  require(install.load)
}

#then load the packages that will be used
install_load("reshape2", "manifestoR", "readr", "countrycode", "zoo", "lubridate")

# Set parameters ----------------------------------------------------------
setwd("E:/Manifesto analysis 2019") #set working directory
mp_setapikey("manifesto_apikey.txt") #set up the API key for access to the Manifesto data

source('basic general functions.r')
#source('https://raw.githubusercontent.com/demetriodor/govpositions/master/basic%20general%20functions.R')

# Load the original datafiles ----------------------
manifestos <- mp_maindataset() #loads the most current version by default, 2018b
parties<-read_csv('http://www.parlgov.org/static/data/development-utf-8/view_party.csv') #ParlGov party-level file
cabs <-read_csv('http://www.parlgov.org/static/data/development-utf-8/view_cabinet.csv') #ParlGov cabinets-level file
# Data preparation ----------------------
#get only relevant columns
parties<-parties%>%select(country_name, party_name_english,party_id, cmp)
cabs<-cabs%>%select      (country_name, party_name_english, party_id, cabinet_id, previous_cabinet_id, election_id, 
                          election_date, start_date, cabinet_name, caretaker, cabinet_party, seats, election_seats_total)
manifestos[manifestos$countryname=='Czech Republic', 'countryname']<-"Czechia" #replace Czech Republic with Czechia
parties[parties$country_name=='Czech Republic', 'country_name']<-"Czechia"
cabs[cabs$country_name=='Czech Republic', 'country_name']<-"Czechia"

#select EU+ countries of interset
countries.iso = c("AUT","BEL","BGR","CHE","CYP","CZE","DEU","DNK","ESP","EST", "FIN","FRA","GBR","GRC","HRV","HUN","IRL","ISL","ITA","LUX","LTU","LVA","MLT","NLD","NOR","POL","PRT","ROU","SVK","SVN","SWE")
countries<-countrycode(countries.iso, 'iso3c', 'country.name')
countries.list=countries #this is for later

#subset the data files for the EU+ countries only
manifestos<-manifestos %>% filter(countryname %in% countries)
parties<-parties %>% filter(country_name %in% countries)
cabs<-cabs %>% filter(country_name %in% countries)
cabs<-cabs[cabs$election_date > '1945-01-01',]

#run corrections (alignment) of election dates. 
source('election dates corrections.R')
#source('https://raw.githubusercontent.com/demetriodor/govpositions/master/election%20dates%20corrections.R')

# Get additional CMP codes--------------
#Get additional CMP codes if the party names match exactly 
parties$cmp2<-parties$cmp #create a copy variable

#for each row in parties, if the cmp code is NA and if the corresponding party name in Manifestos is not NA and the country is the same, record it in cmp2
for (i in 1:nrow(parties)){
  if (is.na(parties$cmp)==T)
    if (is.na(as.data.frame(manifestos[manifestos$countryname==parties$country_name[i] & manifestos$partyname==parties$party_name_english[i], 'party'][1,]))==T)
      next
  else
    parties$cmp2[i]<-as.data.frame(manifestos[manifestos$countryname==parties$country_name[i] & manifestos$partyname==parties$party_name_english[i], 'party'][1,])
  else
    next
}
parties$cmp2<-make.number(parties$cmp2)

source('cmp code corrections.R')#apply additional additions and corrections to the cmp code
#source('https://raw.githubusercontent.com/demetriodor/govpositions/master/cmp%20code%20corrections.R')

###align party codes between manifesots and parlgov
file_name <- "partyfacts-mapping.csv"
url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
download.file(url, file_name)
partyfacts <- read_csv(file_name, guess_max = 30000)
partyfacts<-partyfacts %>% filter(country %in% countries.iso)
dataset_1 <- partyfacts %>% filter(dataset_key == "manifesto")
dataset_2 <- partyfacts %>% filter(dataset_key == "parlgov")
links <-
  dataset_1 %>%
  inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))
links<-links[,c('dataset_party_id.x','dataset_party_id.y')]
colnames(links)<-c('mani','parlgov')
links<-links[!duplicated(links[,c('parlgov')]),]

parties<-merge(parties, links, by.y='parlgov', by.x='party_id', all.x=T)
parties$cmp2[is.na(parties$cmp2) & !is.na(parties$mani)]<-parties$mani[is.na(parties$cmp2) & !is.na(parties$mani)]#if the cmp code is still missing in parties but is not missing in the party facts dataset, impute it
parties$cmp2<-make.number(parties$cmp2)

# Merge parties and cabinets --------------------------------------------
parties$party<-parties$cmp2 #copy the column to use for merge
manipart<-merge(manifestos, parties, by="party") #keep only observations that have CMP codes in ParlGov 
manipart$new_id<-paste0(manipart$party_id, ".", manipart$edate) #index variable
cabs$new_id<-paste0(cabs$party_id, '.',cabs$election_date) #index variable
cabinets<-merge(manipart, cabs, by="new_id", all.y=T) #merge parties and cabinets

source('more cmp code corrections.R')#run additional cmp code assignments.
#source('https://raw.githubusercontent.com/demetriodor/govpositions/master/more%20cmp%20code%20corrections.R')

cabinets$seats_share <- (cabinets$seats / cabinets$election_seats_total) * 100 #calculate party seat share
#put the end dates of the cabinets in the file, based on the starting date of the next cabinet
for (i in 1:nrow(cabinets)){
  cabinets$end_date[i]<-cabinets$start_date[cabinets$previous_cabinet_id==cabinets$cabinet_id[i] & !is.na(cabinets$previous_cabinet_id)][1]
}
cabinets$end_date<-as.Date(cabinets$end_date-1)

# Make table of last elections --------------------------------------------
last.elections<-data.frame(matrix(NA, nrow=length(countries), ncol=3))
colnames(last.elections)<-c('country','manifesto.last','parlgov.last')
for (i in 1:length(countries)){
  last.elections[i,'country']<-countries[i]
  last.elections[i,'manifesto.last']<-(tail(manifestos$edate[manifestos$countryname==countries[i]],1))
  last.elections[i,'parlgov.last']<-(tail(cabs$election_date[cabs$country_name==countries[i]],1))
}
last.elections[,2]<-as.Date(last.elections[,2])
last.elections[,3]<-as.Date(last.elections[,3])

# Assign data collection date to end_date ---------------------------------
cabinets$end_date<-as.Date(ifelse(is.na(cabinets$end_date), as.Date('2018-12-31', "%Y-%m-%d"),cabinets$end_date ))

# Exclude parties that should not have positions --------------------------
for (i in 1:nrow(cabinets)){
  if (!is.na(cabinets$cmp2[i]) & cabinets$election_date[i] > last.elections$manifesto.last[last.elections$country==cabinets$country_name.y[i]])
    cabinets[i,1:174]<-NA
  else
    next
}

# Save the dataset and clean up --------------------------------------------------------
#write.csv (cabinets, 'cabinets2019.csv') #save the resulting file
#cabinets<-read.csv ('cabinets2019.csv', as.is=TRUE) #uncomment if you want to load the file directly once you have saved it
#save(cabinets, file = "cabinets2019.RData")# save in RData format
#load("abinets2019.RData") # to load the data

