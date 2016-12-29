### create two bar plots: 
### - police killing by city 
### - police killing of unarmed people by city
### break down by race and order cities by rates

library(dplyr)
library(Hmisc)
library(ggplot2)
library(gdata)
library(tools)
library(sandwich)
library(msm)
library(corrgram)
setwd("/Users/xzhuo/Projects/Police Violence")

x2 <- read.csv("data/data.csv", stringsAsFactor=F)
head(x2)

# plotting
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_color_hue(2)
df <- x2[with(x2, order(rate)), ]
colvec <- ifelse(df$city=="Boston", "#F8766D", "black")

pdf("plots/test.pdf", )
ggplot(x2, aes(x=reorder(paste(city, state, sep=","), rate), y=rate, fill=color, group=color)) +
  geom_bar(stat='identity', width=0.8, position=position_dodge(width=0.8)) +
  coord_flip() +
  ylab("") + 
  xlab("") +
  #ggtitle(paste("rate of police killings (per million population),", title)) + 
  theme(text = element_text(size=6)) + 
  geom_hline(yintercept = quantile(x2$rate, 0.25), lty=3) + 
  geom_hline(yintercept = quantile(x2$rate, 0.75), lty=3) +
  guides(fill=FALSE) +
  scale_y_continuous(expand = c(0,0)) 
  #theme(axis.text.y = element_text(colour=colvec))
dev.off()


makeBarPlot <- function(x2, filename, title) {
  x2$color <- factor(as.numeric(x2$city=="Boston"))
  
  pdf(paste("plots/rate_", filename, ".pdf", sep=""))
  print(ggplot(x2, aes(x=reorder(paste(city, state, sep=","), rate), y=rate, fill=color)) +
    geom_bar(stat='identity', width=0.8) +
    coord_flip() +
    ylab("") + 
    xlab("") +
    ggtitle(paste("rate of police killings (per million population),", title)) + 
    theme(text = element_text(size=6)) + 
    geom_hline(yintercept = quantile(x2$rate, 0.25), lty=3) + 
    geom_hline(yintercept = quantile(x2$rate, 0.75), lty=3) +
    guides(fill=FALSE) + 
    scale_y_continuous(expand = c(0,0))) 
  dev.off()
  
  pdf(paste("plots/rate_black_", filename, ".pdf", sep=""))
  print(ggplot(x2, aes(x=reorder(paste(city, state, sep=","), rate.black), y=rate.black, fill=color)) +
    geom_bar(stat='identity', width=0.8) +
    coord_flip() +
    ylab("") + 
    xlab("") +
    ggtitle(paste("rate of police killings of blacks (per million black population),", title)) + 
    theme(text = element_text(size=6)) + 
    geom_hline(yintercept = quantile(x2$rate.black, 0.25), lty=3) + 
    geom_hline(yintercept = quantile(x2$rate.black, 0.75), lty=3) +
    guides(fill=FALSE) + 
    scale_y_continuous(expand = c(0,0)))
  dev.off()

  pdf(paste("plots/rate_white_", filename, ".pdf", sep=""))
  print(ggplot(x2, aes(x=reorder(paste(city, state, sep=","), rate.white), y=rate.white, fill=color)) +
    geom_bar(stat='identity', width=0.8) +
    coord_flip() +
    ylab("") + 
    xlab("") +
    ggtitle(paste("rate of police killings of whites (per million white population),", title)) + 
    theme(text = element_text(size=6)) + 
    geom_hline(yintercept = quantile(x2$rate.white, 0.25), lty=3) + 
    geom_hline(yintercept = quantile(x2$rate.white, 0.75), lty=3) +
    guides(fill=FALSE) + 
    scale_y_continuous(expand = c(0,0)))
  dev.off()
  
  
  ### UNARMED
  pdf(paste("plots/unarmrate_", filename, ".pdf", sep=""))
  print(ggplot(x2, aes(x=reorder(paste(city, state, sep=","), unarmrate), y=unarmrate, fill=color)) +
    geom_bar(stat='identity', width=0.8) +
    coord_flip() +
    ylab("") + 
    xlab("") +
    ggtitle(paste("rate of unarmed killings (per million population),", title)) + 
    theme(text = element_text(size=6)) + 
    geom_hline(yintercept = quantile(x2$unarmrate, 0.25), lty=3) + 
    geom_hline(yintercept = quantile(x2$unarmrate, 0.75), lty=3) +
    guides(fill=FALSE) + 
    scale_y_continuous(expand = c(0,0)))
  dev.off()
  
  pdf(paste("plots/unarmrate_black_", filename, ".pdf", sep=""))
  print(ggplot(x2, aes(x=reorder(paste(city, state, sep=","), unarmrate.black), y=unarmrate.black, fill=color)) +
    geom_bar(stat='identity', width=0.8) +
    coord_flip() +
    ylab("") + 
    xlab("") +
    ggtitle(paste("rate of unarmed police killings of blacks (per million black population),", title)) + 
    theme(text = element_text(size=6)) + 
    geom_hline(yintercept = quantile(x2$unarmrate.black, 0.25), lty=3) + 
    geom_hline(yintercept = quantile(x2$unarmrate.black, 0.75), lty=3) +
    guides(fill=FALSE) + 
    scale_y_continuous(expand = c(0,0)))
  dev.off()
  
  pdf(paste("plots/unarmrate_white_", filename, ".pdf", sep=""))
  print(ggplot(x2, aes(x=reorder(paste(city, state, sep=","), unarmrate.white), y=unarmrate.white, fill=color)) +
    geom_bar(stat='identity', width=0.8) +
    coord_flip() +
    ylab("") + 
    xlab("") +
    ggtitle(paste("rate of unarmed police killings of whites (per million white population),", title)) + 
    theme(text = element_text(size=6)) + 
    geom_hline(yintercept = quantile(x2$unarmrate.white, 0.25), lty=3) + 
    geom_hline(yintercept = quantile(x2$unarmrate.white, 0.75), lty=3) +
    guides(fill=FALSE) + 
    scale_y_continuous(expand = c(0,0)))
  dev.off()
}

makeBarPlot(getRatePerCity(person.df, c(13, 14, 15)), "all_yr", "2013-2015")
makeBarPlot(getRatePerCity(person.df, c(13)), "13", "2013")
makeBarPlot(getRatePerCity(person.df, c(14)), "14", "2014")
makeBarPlot(getRatePerCity(person.df, c(15)), "15", "2015")
