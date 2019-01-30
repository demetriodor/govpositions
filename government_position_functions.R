### Functions for aggregating party positions into government positions and into time series
countries.list = c("Austria","Belgium","Bulgaria","Switzerland","Cyprus","Czechia","Germany","Denmark","Spain","Estonia","Finland","France","United Kingdom","Greece","Croatia","Hungary","Ireland","Iceland","Italy","Luxembourg","Lithuania","Latvia","Malta","Netherlands","Norway","Poland","Portugal","Romania","Slovakia","Slovenia","Sweden")
# Government position function --------------------------------------------
gov.pos<-function (x=NA,y=NA, data=cabinets, method = pos.add, name ='position', start='1945-01-01', end='2018-12-31', countries=countries.list) {
  
  dataset2 = party.pos(x=x, y=y, data=data, method=method, output='attach') #calculate the party score and attach to the dataframe
  colnames(dataset2)[ncol(dataset2)]<-'party.score' #rename the last column temporarity
  
  dataset2<- dataset2%>%
    filter(election_date > start & election_date < end & country_name.y%in%countries & cabinet_party==1)%>%
    select(cabinet_id, seats_share,country_name.y,start_date,end_date,election_date, party.score)
  
  #first, create a variable that calculates the party seats share from all parties in government *that have a valid position*
  for (i in unique(dataset2$cabinet_id)){
    dataset2[dataset2$cabinet_id==i,'rel_seats_share'] = dataset2[dataset2$cabinet_id==i,'seats_share'] / sum.allna(dataset2[dataset2$cabinet_id==i & !is.na(dataset2$party.score),'seats_share'])
  }
  
  #now aggregate per cabinet
  m = dataset2%>% group_by(cabinet_id) %>% summarise(country=country_name.y[1], election.date=election_date[1],
                                                     start.date = start_date[1], end.date = end_date[1],
                                                     simple.mean  = round(mean.allna(party.score),2), 
                                                     weighted.mean = round(weighted.mean(party.score, rel_seats_share, na.rm=T),2),
                                                     minimum = round(min.allna(party.score),2), 
                                                     maximum = round(max.allna(party.score),2),
                                                     range = abs (round(max.allna(party.score),2) -round(min.allna(party.score),2)),
                                                     parties.number = n(),
                                                     share.missing = round(sum.na(seats_share[is.na(party.score)])/sum.na(seats_share)*100,0)
  )                                  
  
  
  m<-m[order(m$country, m$start.date),]
  m
}

# Aggregation over time ---------------------------------------------------
gov.pos.time <-function (x=NA,y=NA, data=cabinets, method = pos.add, name ='position', start='1945-01-01', end='2018-12-31', countries=countries.list, time='month') {
  
  dat<-gov.pos (x=x,y=y, data=data, method=method, countries=countries,start='1945-01-01', end='2018-12-31') 
  dat$startY<-year(dat$start.date)
  dat$startYM<-as.yearmon(dat$start.date)
  dat$endY<-year(dat$end.date)
  dat$endYM<-as.yearmon(dat$end.date)  
  
  u.c<-countries                                
  u.m<-as.yearmon(seq.Date(from=as.Date(start), to=as.Date(end), by='month'))
  
  month.table<-expand.grid(u.c, u.m)
  colnames(month.table)<-c('country','month.year')
  month.table$simple.mean<-NA
  month.table$weighted.mean<-NA
  month.table$minimum<-NA
  month.table$maximum<-NA
  month.table$range<-NA
  month.table$parties.number<-NA
  month.table$share.missing<-NA
  
  
  for (i in 1:nrow(month.table)){
    try(month.table[i, 3:9] <-dat[which(dat$country==month.table$country[i] & dat$startYM <= month.table$month.year[i] & dat$endYM > month.table$month.year[i]), 6:12], silent=TRUE)
  }
  month.table<-month.table[order(month.table$country, month.table$month.year),]
  
  if (time=='month'){
    return (month.table)}
  else if (time=='year'){
    month.table$year<-year(month.table$month.year)  
    month.table$index<-paste0(month.table$country, '.', month.table$year)
    year.table = month.table%>% group_by(index) %>% summarise(country=country[1], year= year[1],
                                                              simple.mean = mean.allna(simple.mean), 
                                                              weighted.mean = mean.allna(weighted.mean),
                                                              minimum = mean.allna(minimum),
                                                              maximum = mean.allna(maximum),
                                                              range = mean.allna(range),
                                                              parties.number = mean.allna(parties.number),
                                                              share.missing = mean.allna(share.missing))
    return (year.table)
  }
}
