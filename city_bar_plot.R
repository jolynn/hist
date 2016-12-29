### create two bar plots: 
### - police killing by city 
### - police killing of unarmed people by city
### break down by race and order cities by rates

library(dplyr)
library(Hmisc)
library(ggplot2)
setwd("/Users/xzhuo/Dropbox/Police Violence")

# read data
person.df <- read.csv("data/MPVDatasetDownload.csv", stringsAsFactor=F)
head(person.df)
#drops <- c("X", paste("X", 1:29, sep="."))
#drops
#person.df <- person.df[ , !(names(person.df) %in% drops)]
#head(person.df)

report.df <- read.csv("data/PoliceViolenceReport2015.csv", stringsAsFactors=F)
head(report.df)

pop.msa <- read.csv("data/pop_msa2013.csv", stringsAsFactors=F)
head(pop.msa)
pop.msa <- pop.msa[,c("NAME", "T001_001")]
pop.msa <- rename(pop.msa, city=NAME, pop=T001_001)
head(pop.msa)

pop.city <- read.csv("data/pop_city2013.csv", stringsAsFactors=F)
head(pop.city)
pop.city <- pop.city[,c("QName", "STUSAB", "T001_001")]
pop.city <- rename(pop.city, city=QName, state=STUSAB, pop=T001_001)
head(pop.city)


# aggregate person-killing by city
names(person.df)
unique(person.df$Location.of.death..city.)
x <- data.frame(person.df %>% count(Location.of.death..city., Location.of.death..state.)) # create a new dataframe
head(x)
nrow(x)
x <- rename(x, city = Location.of.death..city., state = Location.of.death..state., kills = n)
head(x)

hist(x$kills)
hist(x$kills[x$kills<10])


# aggregate unarmed-person-killing by city
head(person.df)
unique(person.df$Unarmed)
nrow(person.df)
table(person.df$Unarmed) # unarmed only 665 out of 3485

# is.unarmed excludes Allegedly Armed, Unclear, and Vehicle
person.df$is.unarmed <- person.df$Unarmed=="Unarmed"
head(person.df)
gby <- group_by(person.df, Location.of.death..city., Location.of.death..state.) 
tmp <- data.frame(dplyr::summarize(gby, unarmed.kills = sum(is.unarmed)))
head(tmp)
tmp <- rename(tmp, city=Location.of.death..city., state=Location.of.death..state.)
head(tmp)

hist(tmp$unarmed.kills)
hist(tmp$unarmed.kills[tmp$unarmed.kills < 10]) 

# there are not many police killings of unarmed people by city
# merge with data frame x
x <- merge(x, tmp, by=c("city", "state"))
head(x)
nrow(x)
rcorr(as.matrix(x[,c(3,4)]), type="pearson") # correlation of 0.76


# break down by race
names(person.df)
table(person.df$Victim.s.race)

# construct new race variable: 
# white - 1, black - 2, hispanic - 3, other (including unknown) - 0
person.df$race <- 0
person.df[person.df$Victim.s.race=="White", "race"] <- 1
person.df[person.df$Victim.s.race=="Black", "race"] <- 2
person.df[person.df$Victim.s.race=="Hispanic", "race"] <- 3
head(person.df)

table(person.df[,c("Victim.s.race", "race")])

person.df$race <- factor(person.df$race, 
                         labels=c("other", "white", "black", "hispanic"))
table(person.df$race)


# aggregate police killings by race (no distinctin of armed/unarmed)
person.df$kills.w <- person.df$race=="white"
person.df$kills.b <- person.df$race=="black"
person.df$kills.h <- person.df$race=="hispanic"
person.df$kills.o <- person.df$race=="other"
head(person.df)

table(person.df[,c("race", "Unarmed")])
person.df$unarmed.kills.w <- (person.df$race=="white" & person.df$Unarmed=="Unarmed")
person.df$unarmed.kills.b <- (person.df$race=="black" & person.df$Unarmed=="Unarmed")
person.df$unarmed.kills.h <- (person.df$race=="hispanic" & person.df$Unarmed=="Unarmed")
person.df$unarmed.kills.o <- (person.df$race=="other" & person.df$Unarmed=="Unarmed")
head(person.df)

# summarize
gby <- group_by(person.df, Location.of.death..city., Location.of.death..state.) 
tmp <- data.frame(dplyr::summarize(gby, 
                            kills.w=sum(kills.w), kills.b=sum(kills.b),
                            kills.h=sum(kills.h), kills.o=sum(kills.o),
                            unarmed.kills.w=sum(unarmed.kills.w), 
                            unarmed.kills.b=sum(unarmed.kills.b),
                            unarmed.kills.h=sum(unarmed.kills.h), 
                            unarmed.kills.o=sum(unarmed.kills.o)))
head(tmp)
tmp <- rename(tmp, city=Location.of.death..city., state=Location.of.death..state.)
head(tmp)
x <- merge(x, tmp, by=c("city", "state"))
head(x)




# verify that sum of kills.w, kills,b, etc = kills
sum(x$kills==x$kills.w+x$kills.b+x$kills.h+x$kills.o)
nrow(x) # CORRECT

# verify that sum of unarmed.kills.w, unarmed.kills,b, etc = unarmed.kills
sum(x$unarmed.kills==x$unarmed.kills.w+x$unarmed.kills.b+x$unarmed.kills.h+x$unarmed.kills.o)
# CORRECT


# calcualte rate of police killings per million population
# match with ACS city populations
# per capita is too tiny
head(report.df)
is.factor(report.df$X2014.population..US.Census.)
report.df$X2014.population..US.Census.
report.df$pop <- as.numeric(gsub(",", "", report.df$X2014.population..US.Census.))
head(report.df)
nrow(report.df)

report.df$Police.Department
report.df <- report.df[-c(61, 62, 63),]
report.df$Police.Department
report.df$Police.Department
gsub("[[:digit:]]+.[[:space:]]", "", report.df$Police.Department)
report.df$city <- gsub("[[:digit:]]+.[[:space:]]", "", report.df$Police.Department)

head(pop.city)
report.df$acs.city <- NA
report.df$acs.state <- NA
report.df$acs.pop <- NA
for (i in 1:nrow(report.df)) {
  cname <- report.df[i, "city"]
  if (length(which(regexpr(cname, pop.city$city) != -1))==1) {
    ind <- which(regexpr(cname, pop.city$city) != -1)
    report.df[i, "acs.city"] <- pop.city[ind, "city"]
    report.df[i, "acs.state"] <- pop.city[ind, "state"]
    report.df[i, "acs.pop"] <- pop.city[ind, "pop"]
  } else if (length(which(regexpr(paste(cname, "city"), pop.city$city) != -1))==1) {
    ind <- which(regexpr(paste(cname, "city"), pop.city$city) != -1)
    report.df[i, "acs.city"] <- pop.city[ind, "city"]
    report.df[i, "acs.state"] <- pop.city[ind, "state"]
    report.df[i, "acs.pop"] <- pop.city[ind, "pop"]
  }
}
names(report.df)
selected.cols <- select(report.df, city, acs.city, State, acs.state, pop, acs.pop)
View(selected.cols)
rcorr(as.matrix(filter(select(report.df, pop, acs.pop), !is.na(acs.pop))))
# closely related, except Las Vegas


# check whether MSA pop estimates are compatible
report.df$msa.city <- NA
report.df$msa.state <- NA
report.df$msa.pop <- NA
for (i in 1:nrow(report.df)) {
  cname <- report.df[i, "city"]
  if (length(which(regexpr(cname, pop.msa$city) != -1))==1) {
    ind <- which(regexpr(cname, pop.msa$city) != -1)
    report.df[i, "msa.city"] <- pop.msa[ind, "city"]
    report.df[i, "msa.pop"] <- pop.msa[ind, "pop"]
  } else if (length(which(regexpr(paste(cname, "city"), pop.city$city) != -1))==1) {
    ind <- which(regexpr(paste(cname, "city"), pop.city$city) != -1)
    report.df[i, "msa.city"] <- pop.msa[ind, "city"]
    report.df[i, "msa.pop"] <- pop.msa[ind, "pop"]
  }
}
selected.cols <- select(report.df, city, msa.city, State, pop, msa.pop)
View(selected.cols)
rcorr(as.matrix(filter(select(report.df, pop, msa.pop), !is.na(msa.pop)))) # low 0.71


# better use population estimates from principal cities
head(x)
head(pop.city)
pop.city$state <- toupper(pop.city$state)

x$acs.city <- NA
x$acs.state <- NA
x$acs.pop <- NA
for (i in 1:nrow(x)) {
  cname <- x[i, "city"]
  if (length(which(regexpr(cname, pop.city$city) != -1))==1) {
    ind <- which(regexpr(cname, pop.city$city) != -1)
    if (x[i, "state"]==pop.city[ind, "state"]) {
      x[i, "acs.city"] <- pop.city[ind, "city"]
      x[i, "acs.state"] <- pop.city[ind, "state"]
      x[i, "acs.pop"] <- pop.city[ind, "pop"]  
    }
  } else if (length(which(regexpr(paste(cname, "city"), pop.city$city) != -1))==1) {
    ind <- which(regexpr(paste(cname, "city"), pop.city$city) != -1)
    if (x[i, "state"]==pop.city[ind, "state"]) {
      x[i, "acs.city"] <- pop.city[ind, "city"]
      x[i, "acs.state"] <- pop.city[ind, "state"]
      x[i, "acs.pop"] <- pop.city[ind, "pop"]  
    }
  }
}
head(x)
sum(!is.na(x$acs.city)) # 371 cities were matched

selected.cols <- select(x, city, acs.city, state, acs.state, acs.pop)
View(selected.cols)



# focus on 50 largest cities for now
pop.city <- pop.city[with(pop.city, order(-pop)), ]
head(pop.city)
write.csv(pop.city, "data/ordered_pop.csv", row.names=F)
write.csv(x, "data/matched.csv", row.names=F)



y <- read.csv("data/matched.csv", stringsAsFactors=F)
head(y)
sum(!is.na(y$checked)) # right, 50 cities
y <- filter(y, checked==1)
nrow(y)



# calculate rate per million population
y <- mutate(y, rate=kills/acs.pop*10^6)
y <- mutate(y, rate.w=kills.w/acs.pop*10^6)
y <- mutate(y, rate.b=kills.b/acs.pop*10^6)
y <- mutate(y, rate.h=kills.h/acs.pop*10^6)
y <- mutate(y, rate.o=kills.o/acs.pop*10^6)

y <- mutate(y, urate=unarmed.kills/acs.pop*10^6)
y <- mutate(y, urate.w=unarmed.kills.w/acs.pop*10^6)
y <- mutate(y, urate.b=unarmed.kills.b/acs.pop*10^6)
y <- mutate(y, urate.h=unarmed.kills.h/acs.pop*10^6)
y <- mutate(y, urate.o=unarmed.kills.o/acs.pop*10^6)

head(y)



# create plot
ggplot(y, aes(x=reorder(paste(city, state, sep=","), rate), y=rate)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings per million population")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), rate.b), y=rate.b)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings per million population - black")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), rate.w), y=rate.w)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings per million population - white")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), rate.h), y=rate.h)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings per million population - hispanic")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), rate.o), y=rate.o)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings per million population - other")


# unarmed
ggplot(y, aes(x=reorder(paste(city, state, sep=","), urate), y=urate)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings of UNARMED people per million population")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), urate.b), y=urate.b)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings of UNARMED people per million population - black")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), urate.w), y=urate.w)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings of UNARMED people per million population - white")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), urate.h), y=urate.h)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings of UNARMED people per million population - hispanic")

ggplot(y, aes(x=reorder(paste(city, state, sep=","), urate.o), y=urate.o)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("") + 
  ggtitle("rate of police killings of UNARMED people per million population - other")
