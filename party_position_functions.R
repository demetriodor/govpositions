### Functions for computing party positions from Manifesto data 
countries.list = c("Austria","Belgium","Bulgaria","Switzerland","Cyprus","Czechia","Germany","Denmark","Spain","Estonia","Finland","France","United Kingdom","Greece","Croatia","Hungary","Ireland","Iceland","Italy","Luxembourg","Lithuania","Latvia","Malta","Netherlands","Norway","Poland","Portugal","Romania","Slovakia","Slovenia","Sweden")
# Party position and salience calculation ---------------------------------
pos.add<-function (x=NA,y=NA, data=cabinets) {
  pos = if (is.na(y[1]))
    rowsum.allna(data[, x, drop=F])
  else if (is.na(x[1]))
    -rowsum.allna(data[, y, drop=F])
  else
    rowsum.allna(data[, x, drop=F])-rowsum.allna(data[, y, drop=F])
  pos
}

pos.add.sal<-function (x=NA, ..., data=cabinets) {
  pos = rowsum.allna(data[, x, drop=F])
  pos
}

pos.add.n<-function (x=NA,y=NA, data=cabinets) {
  pos = if (is.na(y[1]))
    round(rowsum.allna(data[,x, drop=F])*data[,'total']/100)  
  else if (is.na(x[1]))
    -round(rowsum.allna(data[,y, drop=F])*data[,'total']/100)  
  else
    round(rowsum.allna(data[,x, drop=F])*data[,'total']/100) - round(rowsum.allna(data[,y, drop=F])*data[,'total']/100)  
  pos
}

pos.add.n.sal<-function (x=NA, ..., data=cabinets) {
  pos = round(rowsum.allna(data[,x, drop=F])*data[,'total']/100)
  pos
}

pos.lbml<-function (x=NA,y=NA, data=cabinets) {
  pos = if (is.na(y[1]))
    log (round(rowsum.allna(data[, x, drop=F])*data[, 'total']/100) + 0.5) - log (0.5)
  else if (is.na(x[1]))
    - log (round(rowsum.allna(data[, y, drop=F])*data[, 'total']/100) + 0.5) + log (0.5)
  
  else
    log((round(rowsum.allna(data[, x, drop=F])*data[, 'total']/100) + 0.5)) - log((round(rowsum.allna(data[, y, drop=F])*data[, 'total']/100) + 0.5))
  pos
}

pos.lbml.sal<-function (x=NA, ..., data=cabinets) {
  pos = log((round(rowsum.allna(data[, x, drop=F])*data[, 'total']/100) + 1)/ data[, 'total'])
  pos
}

# Party position and salience wrap-up ---------------------------------
party.pos<-function (x=NA,y=NA, data=cabinets, method = pos.add, name = 'position', output = 'table', start='1945-01-01', end='2018-12-31', countries=countries.list, start_col='election_date', end_col='election_date', country_col='country_name.y', party_col='partyname') {
  data<-data.frame(data)
  dataset<- data%>%filter(data[,start_col] >= start & data[,end_col] <= end & data[,country_col]%in%countries) #filter for the required time and geo scope
  temp = method (x,y, data=dataset) #run the party position calculation function
  
  if (output=='vector'){
    assign(name, temp, envir = .GlobalEnv)
    return (temp)}
  else if (output=='attach'){
    data.out<-cbind(dataset, temp)
    colnames(data.out)[ncol(data.out)]<-name
    return(data.out)}
  else if (output=='table'){
    data.out<-cbind(dataset, temp)
    data.out = data.out[,c(country_col, start_col, party_col, 'temp')]
    colnames(data.out)[1:4]<-c('country','election.date', 'party.name', name) 
    data.out<-data.out[order(data.out$country, data.out$election.date),]
    return(data.out)}
}
