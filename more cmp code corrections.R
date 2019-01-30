#these are mostly cases where the parties are not coded for the actual elections, but records exist for previous ones, so we extrapolate from the previous election data
n<-which(colnames(cabinets)=='cmp2')
cabinets[cabinets$party_id.y==1536 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==42710 & manifestos$edate=='2006-10-01',] ,42710) #austria
cabinets[cabinets$party_id.y==1120 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==93002 & manifestos$edate=='2008-11-30',]  ,93002)#romania
cabinets[cabinets$party_id.y==5 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==93002 & manifestos$edate=='2008-11-30',]     ,93002)#romania
cabinets[cabinets$party_id.y==1015 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==93031 & manifestos$edate=='2012-12-09',]  ,93031)#romania
cabinets[cabinets$party_id.y==521 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==87071 & manifestos$edate=='2010-10-02',]   ,87071)#latvia
cabinets[cabinets$party_id.y==399 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==32953 & manifestos$edate=='2006-04-10',]   ,32953)#italy
cabinets[cabinets$party_id.y==226 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==32530 & manifestos$edate=='2006-04-10',]   ,32530)#italy
cabinets[cabinets$party_id.y==2268 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==32061 & manifestos$edate=='2013-02-24',]  ,32061)#italy
cabinets[cabinets$party_id.y==706 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==97322 & manifestos$edate=='2004-10-03',]   ,97322)#slovenia
cabinets[cabinets$party_id.y==179 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==97320 & manifestos$edate=='2000-10-15',]   ,97320)#slovenia
cabinets[cabinets$party_id.y==972 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==21521 & manifestos$edate=='2007-06-10',]   ,21521)#belgium
cabinets[cabinets$party_id.y==1160 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==80220 & manifestos$edate=='2013-05-12',]  ,80220)#bulgaria
cabinets[cabinets$party_id.y==1493 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==81032 & manifestos$edate=='2011-12-04',]  ,81032)#croatia
cabinets[cabinets$party_id.y==1384 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==81032 & manifestos$edate=='2011-12-04',]  ,81032)#croatia
cabinets[cabinets$party_id.y==1627 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==81032 & manifestos$edate=='2011-12-04',]  ,81032)#croatia
cabinets[cabinets$party_id.y==276 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==81061 & manifestos$edate=='2015-11-08',]   ,81061)#croatia
cabinets[cabinets$party_id.y==1592 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==34212 & manifestos$edate=='2015-01-25',]  ,34212)#greece
cabinets[cabinets$party_id.y==921 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==86061 & manifestos$edate=='2014-04-06',]  ,86061)#hungary
cabinets[cabinets$party_id.y==434 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==86061 & manifestos$edate=='2014-04-06',]  ,86061)#hungary
cabinets[cabinets$party_id.y==1045 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==88621 & manifestos$edate=='2008-10-12',]  ,88621)#lithuania
cabinets[cabinets$party_id.y==482 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==88450 & manifestos$edate=='2008-10-12',]  ,88450)#lithuania
cabinets[cabinets$party_id.y==2007 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==88630 & manifestos$edate=='2008-10-12',]  ,88630)#lithuania
cabinets[cabinets$party_id.y==1273 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==35060 & manifestos$edate=='2015-10-04',]  ,35060)#portugal
cabinets[cabinets$party_id.y==2 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==82530 & manifestos$edate=='2010-05-29',]  ,82530)#Czechia
cabinets[cabinets$party_id.y==2263 & is.na(cabinets$cmp2)==T,  c(3:8,2, 9:175,n)]<-c(manifestos[manifestos$party==82430 & manifestos$edate=='2013-10-26',]  ,82430)#Czechia
