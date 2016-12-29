library(dplyr)
library(Hmisc)
library(ggplot2)
library(gdata)
library(tools)
library(sandwich)
library(msm)
library(corrgram)
setwd("/Users/xzhuo/Projects/Police Violence")

# read data
person.df <- read.csv("data/MPVDatasetDownload.csv", stringsAsFactor=F)
head(person.df)

# get year
person.df$year <- sapply(person.df$Date.of.injury.resulting.in.death..month.day.year., 
                         function(x) as.integer(unlist(strsplit(x, split="/"))[3]))
head(person.df)
table(person.df$year)



table(person.df$Unarmed)
table(person.df$Victim.s.race)
nrow(person.df)

countPerCity <- function(person.df) {
  # count killings, unarmed killings, and breakdown by race per city
  
  # is.unarmed excludes Allegedly Armed, Unclear, and Vehicle
  person.df$is.unarmed <- person.df$Unarmed=="Unarmed"
  
  # break down by race  
  person.df$kills.white <- person.df$Victim.s.race=="White"
  person.df$kills.black <- person.df$Victim.s.race=="Black"
  person.df$kills.hispan <- person.df$Victim.s.race=="Hispanic"
  
  person.df$kills.unarmed.white <- (person.df$Victim.s.race=="White" & person.df$Unarmed=="Unarmed")
  person.df$kills.unarmed.black <- (person.df$Victim.s.race=="Black" & person.df$Unarmed=="Unarmed")
  person.df$kills.unarmed.hispan <- (person.df$Victim.s.race=="Hispanic" & person.df$Unarmed=="Unarmed")
  
  # aggregate by city
  gby <- group_by(person.df, Location.of.death..city., Location.of.death..state.) 
  x <- data.frame(dplyr::summarize(gby, 
                                   kills=n(),
                                   kills.unarmed=sum(is.unarmed),
                                   kills.white=sum(kills.white), 
                                   kills.black=sum(kills.black),
                                   kills.hispan=sum(kills.hispan), 
                                   kills.unarmed.white=sum(kills.unarmed.white), 
                                   kills.unarmed.black=sum(kills.unarmed.black),
                                   kills.unarmed.hispan=sum(kills.unarmed.hispan)))
  x <- rename(x, city=Location.of.death..city., state=Location.of.death..state.)
  return(x)
}


trim <- function (x) gsub("^\\s+|\\s+$", "", x) # remove white spaces

getRatePerCity <- function(person.df, year) {
  # return killings per million population in the given years
  # year is a vector of years (integer)
  
  person.df <- person.df[person.df$year %in% year, ]
  
  x <- countPerCity(person.df)
  
  # match with ACS PRINCIPLE CITY populations
  y <- read.csv("data/City2013.csv", stringsAsFactors=F)
  y <- rename(y, state=STUSAB, pop=T001_001, 
              white=T019_002, white2=T020_003, # white2: non-Hispanic white
              black=T019_003, black2=T020_004) # black2: non-Hispanic black
  
  # remove Peurto Rico
  y$state <- toupper(y$state)
  y <- filter(y, state != "PR")
  
  # select largest 100 cities
  y <- y[with(y, order(-pop)), ]
  y <- y[1:100,]
  
  # merge with police killings data
  y$city <- sapply(y$NAME, function(s) trim(unlist(strsplit(s, "city"))[1]))
  y <- select(y, city, state, NAME, pop, white, white2, black, black2)
  
  # try merge
  #x2 <- merge(y, x, by=c("city", "state"), all.x=T)
  
  # which cities weren't matched?
  #x2[is.na(x2$kills), c("city", "state")]
  
  # match manually
  #y[which(startsWith(y$city, "Anchorage municipality")), "city"]
  y[which(startsWith(y$city, "Anchorage municipality")), "city"] <- "Anchorage" # Anchorage municipality, AK; Anchorage, AK Metro Area
  
  #y[which(startsWith(y$city, "Arlington CDP")), "city"]
  y[which(startsWith(y$city, "Arlington CDP")), "city"] <- "Arlington" # Arlington CDP, VA; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area
  
  #y[which(startsWith(y$city, "Boise City")), "city"]
  y[which(startsWith(y$city, "Boise City")), "city"] <- "Boise" # Boise City
  
  #y[which(startsWith(y$city, "Lexington-Fayette")), "city"]
  y[which(startsWith(y$city, "Lexington-Fayette")), "city"] <- "Lexington" # Lexington-Fayette urban county, KY; Lexington-Fayette, KY Metro Area
  
  #y[which(startsWith(y$city, "Louisville/Jefferson")), "city"]
  y[which(startsWith(y$city, "Louisville/Jefferson")), "city"] <- "Louisville" # Louisville/Jefferson County metro government (balance), KY; Louisville/Jefferson County, KY-IN Metro Area
  
  #y[which(startsWith(y$city, "Nashville-Davidson")), "city"]
  y[which(startsWith(y$city, "Nashville-Davidson")), "city"] <- "Nashville" # Nashville-Davidson metropolitan government (balance), TN; Nashville-Davidson--Murfreesboro--Franklin, TN Metro Area
  
  #y[which(startsWith(y$city, "Urban Honolulu CDP")), "city"]
  y[which(startsWith(y$city, "Urban Honolulu CDP")), "city"] <- "Honolulu" # Urban Honolulu CDP, HI; Urban Honolulu, HI Metro Area
  
  #y[which(startsWith(y$city, "Paradise CDP")), "city"]
  y[which(startsWith(y$city, "Paradise CDP")), "city"] <- "Paradise" # Paradise CDP, NV; Las Vegas-Henderson-Paradise, NV Metro Area
  
  x2 <- merge(y, x, by=c("city", "state"), all.x=T)
  x2[is.na(x2$kills), c("city", "state")]
  
  # cities are included only if there's killing, so assume no killing in the five unmatched cities
  # min(x$kills) 
  x2[is.na(x2)] <- 0
  
  # calcualte rate of police killings per million population
  x2 <- mutate(x2, 
               rate=kills/pop*10^6, 
               rate.white=kills.white/white*10^6,
               rate.white2=kills.white/white2*10^6,
               rate.black=kills.black/black*10^6,
               rate.black2=kills.black/black2*10^6,
               unarmrate=kills.unarmed/pop*10^6,
               unarmrate.white=kills.unarmed.white/white*10^6,
               unarmrate.white2=kills.unarmed.white/white2*10^6,
               unarmrate.black=kills.unarmed.black/black*10^6,
               unarmrate.black2=kills.unarmed.black/black2*10^6)
  return(x2)
}

getYearlyRate <- function(person.df, year) {
  # similar as the above function, but edit column names to reflect year
  
  x <- getRatePerCity(person.df, c(year))
  rate.cols <- names(x)[grepl("rate", names(x))]
  x <- x[, c("city", "state", rate.cols)]
  names(x) <- c("city", "state", paste(rate.cols, ".", year, sep=""))
  return(x)
}



x2 <- getRatePerCity(person.df, c(13, 14, 15))
head(x2)

x2 <- merge(x2, getYearlyRate(person.df, 13), by=c("city", "state"))
x2 <- merge(x2, getYearlyRate(person.df, 14), by=c("city", "state"))
x2 <- merge(x2, getYearlyRate(person.df, 15), by=c("city", "state"))
head(x2)
names(x2)

# highlight Boston
x2$color <- factor(as.numeric(x2$city=="Boston"))



# How big does a city have to be in total population to be in the top 100?
y <- read.csv("data/City2013.csv", stringsAsFactors=F)
y <- rename(y, state=STUSAB, pop=T001_001, 
            white=T019_002, white2=T020_003, # white2: non-Hispanic white
            black=T019_003, black2=T020_004) # black2: non-Hispanic black

# remove Peurto Rico
y$state <- toupper(y$state)
y$city <- sapply(y$NAME, function(s) trim(unlist(strsplit(s, "city"))[1]))
y <- filter(y, state != "PR")

# select largest 100 cities
y <- y[with(y, order(-pop)), ]
y <- y[1:100,]
y[1, c("city", "state", "pop")]
y[100, c("city", "state", "pop")]

# correlation between whites and non-hispanic whites
rcorr(as.matrix(x2[, c("white", "white2")]), type="pearson") # corr 1

# correlation between blacks and non-hispanic blacks
rcorr(as.matrix(x2[, c("black", "black2")]), type="pearson") # corr 1



pdf("plots/citypop.pdf")
hist(y$pop, breaks=50)
dev.off()


# look at correlations
head(x2[, c("rate.black", "rate.black2")])
all(x2$rate.black == x2$rate.black2)
sum(x2$rate.black == x2$rate.black2)
rcorr(as.matrix(x2[, c("rate.black", "rate.black2")]), type="pearson") # corr 1

head(x2[, c("rate.white", "rate.white2")])
all(x2$rate.white == x2$rate.white2)
sum(x2$rate.white == x2$rate.white2)
rcorr(as.matrix(x2[, c("rate.white", "rate.white2")]), type="pearson") # corr 0.68

rates <- x2[, c("rate", "rate.black", "rate.white", 
                "unarmrate", "unarmrate.black", "unarmrate.white")]
cor(rates)
pdf("plots/corr_3yr.pdf")
corrgram(rates, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Killing Rate Data (Three Years Combined)")
dev.off()

cor(x2[, paste("rate.", 13:15, sep="")])
cor(x2[, paste("rate.white.", 13:15, sep="")])
cor(x2[, paste("rate.black.", 13:15, sep="")])
cor(x2[, paste("unarmrate.", 13:15, sep="")])
cor(x2[, paste("unarmrate.white.", 13:15, sep="")])
cor(x2[, paste("unarmrate.black.", 13:15, sep="")])


library(weights)
round(cor(rates), 2)
round(wtd.cors(rates, weight=x2$pop), 2)

round(cor(x2[, paste("rate.", 13:15, sep="")]), 2)
round(wtd.cors(x2[, paste("rate.", 13:15, sep="")], weight=x2$pop), 2)

round(cor(x2[, paste("rate.white.", 13:15, sep="")]), 2)
round(wtd.cors(x2[, paste("rate.white.", 13:15, sep="")], weight=x2$white), 2)

round(cor(x2[, paste("rate.black.", 13:15, sep="")]), 2)
round(wtd.cors(x2[, paste("rate.black.", 13:15, sep="")], weight=x2$black), 2)

round(cor(x2[, paste("unarmrate.", 13:15, sep="")]), 2)
round(wtd.cors(x2[, paste("unarmrate.", 13:15, sep="")], weight=x2$pop), 2)

round(cor(x2[, paste("unarmrate.white.", 13:15, sep="")]), 2)
round(wtd.cors(x2[, paste("unarmrate.white.", 13:15, sep="")], weight=x2$white), 2)

round(cor(x2[, paste("unarmrate.black.", 13:15, sep="")]), 2)
round(wtd.cors(x2[, paste("unarmrate.black.", 13:15, sep="")], weight=x2$black), 2)




# export datasets to csv
write.csv(x2, "data/data.csv", row.names=F)


# export data for d3 bar graphs

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$rate)
write.csv(df, "plots/hist/rate_all.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$rate.white)
write.csv(df, "plots/hist/rate_white.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$rate.white2)
write.csv(df, "plots/hist/rate_white2.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$rate.black)
write.csv(df, "plots/hist/rate_black.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$rate.black2)
write.csv(df, "plots/hist/rate_black2.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$unarmrate)
write.csv(df, "plots/hist/unarmed_rate.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$unarmrate.white)
write.csv(df, "plots/hist/unarmed_rate_white.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$unarmrate.white2)
write.csv(df, "plots/hist/unarmed_rate_white2.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$unarmrate.black)
write.csv(df, "plots/hist/unarmed_rate_black.csv", row.names=F)

df <- data.frame(name=paste(x2$city, x2$state, sep=","), value=x2$unarmrate.black2)
write.csv(df, "plots/hist/unarmed_rate_black2.csv", row.names=F)



