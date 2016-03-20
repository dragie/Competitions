#' ---------------------------------------------------------------
#' @version 1.0.0
#' @title Capital One Labs Coding Challenge
#' 
#' @description 
#' This script is used to analyze the US publicly available bay names data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------

#' Loading libraries
library(mime)
library(ggvis)
library(stringdist)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(knitr)
library(highr)

#' Set working directory
setwd("D:\\Capital One\\namesbystate\\")

#' Read files
files <- list.files(path = getwd(), pattern = "*.TXT")

#' Use lapply to apply the read.csv function to all values of files
babyData <- lapply(files, read.csv, header = FALSE, stringsAsFactors = FALSE)
babyData <- do.call(rbind,babyData)

#' Set column names
names(babyData) <- c("state", "gender", "year", "name", "count")

str(babyData)
summary(babyData)

#' Exploring data
#' Number of unique names by year and gender
ABY <- babyData %>%
  group_by(year, gender) %>%
  summarise(name_cnt = sum(count), unq_names = n_distinct(name)) 

ABY %>% 
  ggvis(~year, ~unq_names, stroke=~factor(gender)) %>% 
  layer_lines()

#'Looks like people are more creative with female names than male names
#There is some explosion around 1915, probably due to immigration
#Since 1944 (end of WW2) till around 2010 (recent recession) the influx of original names has been strong


ABN <- babyData %>%
  group_by(name) %>%
  summarise(name_cnt = sum(count), 
            nyears = n_distinct(year), 
            fy = min(year), 
            ly = max(year),
            std_cnt = sd(count),
            avg_cnt = mean(count),
            nsd = std_cnt/avg_cnt) 

#' Get peak year by name             
PeakYearF <- babyData %>%
  group_by(name) %>%
  filter(min_rank(desc(count * 10000 + year)) ==1)  %>% 
  select ( name, topYear = year, CntTopYear = count)

babyData   %>%
  filter( name %in% PeakYearF$name )  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()


#' Merge data back into ABN 
ABN <- ABN %>% inner_join(PeakYearF, by = "name") 
ABN
sample_n(ABN,5)

#' Top names in database by use
Top5 <- ABN  %>%
  ungroup() %>%
  arrange(desc(name_cnt)) %>%
  slice(1:5)

babyData   %>%
  filter( name %in% Top5$name )  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#'Even though these are the most popular names, their peak is in the past for most of them

#' Top names that have been in use for 100 years and exceeded 1M uses
tmp3 <- ABN %>%
  filter(name_cnt > 1000000, nyears >= 100) %>%
  arrange(desc(name_cnt))

babyData   %>%
  filter(name %in% tmp3$name)  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#' New names since 2008 that passed the 100 count nationally
tmp4 <- ABN %>%
  filter( fy >= 2008,  CntTopYear> 100) %>%
  arrange(desc(name_cnt)) %>%
  slice(1:6)

babyData   %>%
  filter( name %in% tmp4$name)  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#' Old names that haven't been used since 2000
tmp5 <- ABN %>%
  filter( fy < 1930, ly < 2000, CntTopYear> 500) %>%
  arrange(desc(name_cnt)) %>%
  slice(1:6)

babyData   %>%
  filter( name %in% tmp5$name)  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#' "One shot" names, had a one time high peak
tmp7 <- ABN %>%
  filter(  name_cnt > 1000, nsd >= 2) %>%
  arrange(desc(name_cnt)) %>%
  slice(1:6)

babyData   %>%
  filter( name %in% tmp7$name )  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#' Looking for similar names
relativePopFNames <- ABN %>% 
  filter(ly>1960,CntTopYear>200)  %>%
  select(name) 
vnames <-relativePopFNames$name

#' Anna
vdist<- stringdist("Anna",tolower(vnames),method="osa") 
filteredList <- vnames[vdist <=1 ]

snames <- ABN %>%
  filter( name %in% filteredList) %>%
  arrange(desc(name_cnt)) %>%
  slice(1:6)

babyData   %>%
  filter( name  %in% snames$name )  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#' Bill
vdist<- stringdist("Bill",tolower(vnames),method="osa") 
filteredList <- vnames[vdist <=1 ]

snames <- ABN %>%
  filter( name %in% filteredList) %>%
  arrange(desc(name_cnt)) %>%
  slice(1:6)

babyData   %>%
  filter( name  %in% snames$name )  %>%
  select (name, year, count) %>% 
  ggvis(~year, ~count, stroke = ~factor(name)) %>% 
  layer_lines()

#' Births by year for each gender
birthsyr_gender <- babyData %>% group_by(year,gender) %>% summarize(nbirths = sum(count))
ggplot(data = birthsyr_gender) + geom_line(aes(x = year,y = nbirths,color = gender)) + 
  ggtitle("Births by Year") + theme_bw()

#' Number of names by year
numnamesyr <- babyData %>% group_by(gender,year,name) %>% summarize(cnt = n())
numnamesyr2 <- numnamesyr %>% group_by(gender,year) %>% summarize(numnames = n())
ggplot(data = numnamesyr2) + geom_line(aes(x = year,y = numnames,color = gender)) + 
  ggtitle("Unique Names by Year") + theme_bw()

#' Question2 - popular names
topNames <- group_by(babyData, name) %>% summarize(n = sum(count)) %>% arrange(desc(n)) %>% head( . , 10)
topNames
topFemale  <- filter(babyData, gender == "F") %>% group_by(name) %>% summarize(n = sum(count)) %>%  arrange(desc(n)) %>% head( . , 10)
topFemale
topMale  <- filter(babyData, gender == "M") %>% group_by(name) %>% summarize(n = sum(count)) %>% arrange(desc(n)) %>% head( . , 10)
topMale

#' Question3 - gender ambiguous name
ambiguousName <- function (argData) {
  input <- argData  
  input$name <-  as.character(input$name)
  input <- input[,c(2,4:5)]
  a <- data.frame(summarize(group_by(input,name, gender), sum(count)))
  b <- data.frame(summarize(group_by(input,name), sum(count)))
  
  a  <- a[!duplicated(a["name"]),]
  
  b$Ratio <- abs((2*a$sum.count./b$sum.count.)-1)
  b <- arrange(b,Ratio)
  return(subset(b, Ratio==0))
}

ambiguousName(subset(babyData, year == 2013))
ambiguousName(subset(babyData, year == 1945))

#' Question 4 - percentage increase and decrease
#' Percentage calculation fpr 1980
babyData1980 <- subset(babyData, year == 1980)
babyData1980$name = as.character(babyData1980$name)
groupedData <- data.frame(summarize(group_by(babyData1980,name), sum(count)))
total <- sum(groupedData$sum.count.)
groupedData$Perc <- groupedData$sum.count./total*100
groupedData <- arrange(groupedData,-Perc)

#'  Percentage calculation for 2013
babyData2013 <- subset(babyData, year == 2013)
babyData2013$name <- as.character(babyData2013$name)
groupedData2 <- data.frame(summarize(group_by(babyData2013,name), sum(count)))
total <- sum(groupedData2$sum.count.)
groupedData2$Perc <- groupedData2$sum.count./total*100
groupedData2 <- arrange(groupedData2,-Perc)

#' Cross check names and sort
sortData <- subset(groupedData2, name %in% groupedData$name)
sortData <- arrange(sortData, name)

#' Inner Join and sort 
join <- subset(groupedData, name %in% sortData$name)
join <- arrange(join, name)

#' Calculate percentage difference
sortData$Diff <- sortData$Perc - join$Perc

sortData <- arrange(sortData, -Diff)
sortData[1,]

sortData <- arrange(sortData, Diff)
sortData[1,]
