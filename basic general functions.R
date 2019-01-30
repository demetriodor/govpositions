#make a number from a character vector and round it
make.number<-function (x, y) {round(as.numeric(as.character(x)), digits=y)}

sum.na<-function (x) sum(x, na.rm=T) #sum that aviods NAs, returns 0 if all NAs
sum.allna <- function(x) if (all(is.na(x))) NA else sum(x,na.rm=T) #sum that avoids NAs but returns NA if all NAs

rowsum.na<-function (x) rowSums(x, na.rm=T) #sum by row that avoids NAs 
rowsum.allna <- function(x) rowSums(x, na.rm=TRUE) * ifelse(rowSums(is.na(x)) == ncol(x), NA, 1) #sum by row that avoids NAs but gives NA for a row with all NAs

mean.na<-function (x) mean(x, na.rm=T) #sum that aviods NAs, returns infinity if all NAs
mean.allna <- function(x) if (all(is.na(x))) NA else mean(x,na.rm=T) #sum that avoids NAs but returns NA if all NAs

min.na<-function (x) min(x, na.rm=T) #sum that aviods NAs, returns 0 if all NAs
min.allna <- function(x) if (all(is.na(x))) NA else min(x,na.rm=T) #sum that avoids NAs but returns NA if all NAs

max.na<-function (x) max(x, na.rm=T) #sum that aviods NAs, returns 0 if all NAs
max.allna <- function(x) if (all(is.na(x))) NA else max(x,na.rm=T) #sum that avoids NAs but returns NA if all NAs
