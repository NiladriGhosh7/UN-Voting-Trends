votes<-read.csv("VotingData.csv")
library(caTools)
library(ggplot2)
#1 - Yes, 2 -Abstain, 3 - No, 8 - Not present, 9 - Not a member.

#install.packages("countrycode")
library(countrycode)
votes$Year <-votes$session+1945
votes$Country<-countrycode(votes$ccode,"cown","country.name")
?countrycode
?codelist
# positve_votes<-subset(votes,votes$vote==1)
# abstained_votes<-subset(votes,votes$vote==2)
# negatve_votes<-subset(votes,votes$vote==3)
# absent_votes<-subset(votes,votes$vote==8)
# notmember_votes<-subset(votes,votes$vote==9)

total_vote<-subset(votes,votes$vote<4)
total_vote

# 1. Percent of positive votes across all years and countries.

Positive_percentage<-(nrow(votes[votes$vote==1,])/nrow(votes[votes$vote<=3,]))*100
# Positive_percentage
a=nrow(votes[votes$vote==1,])

# 2. Trends in Positive, Negative and Abstain votes percent trends through the time 
# (3 line charts with smoothing required?).

By_Year = setNames(aggregate(votes$Year,by = list(Year=votes$Year),FUN="length"),c("Year","Total_Votes"))

By_Country = setNames(aggregate(votes$Country,by = list(votes$Country),FUN="length"),c("Country","Total_Votes"))

total_votes_by_year=setNames(aggregate(votes[votes$vote<=3,"Year"],by=list(votes[votes$vote<=3,"Year"]),
                                          FUN="length"),c("Year","Total_Votes"))

positive_votes_by_year=setNames(aggregate(votes[votes$vote==1,"Year"],by=list(votes[votes$vote==1,"Year"]),
                                          FUN="length"),c("Year","Positive_Votes"))

abstained_votes_by_year=setNames(aggregate(votes[votes$vote==2,"Year"],by=list(votes[votes$vote==2,"Year"]),
                                          FUN="length"),c("Year","Abstained_Votes"))

negative_votes_by_year=setNames(aggregate(votes[votes$vote==3,"Year"],by=list(votes[votes$vote==3,"Year"]),
                                          FUN="length"),c("Year","Negative_Votes"))

Votes_By_Year = Reduce(function(x,y) merge(x,y,all=TRUE),list(total_votes_by_year,positive_votes_by_year,abstained_votes_by_year,negative_votes_by_year))
Votes_By_Year$Positive_Percentage=round((Votes_By_Year$Positive_Votes/Votes_By_Year$Total_Votes)*100,2)
Votes_By_Year$Abstained_Percentage=round((Votes_By_Year$Abstained_Votes/Votes_By_Year$Total_Votes)*100,2)
Votes_By_Year$Negative_Percentage=round((Votes_By_Year$Negative_Votes/Votes_By_Year$Total_Votes)*100,2)



ggplot(Votes_By_Year,aes(x=Year,y=Positive_Percentage,col="red"))+geom_line()+geom_smooth()
ggplot(Votes_By_Year,aes(x=Year,y=Negative_Percentage,col="green"))+geom_line()+geom_smooth()
ggplot(Votes_By_Year,aes(x=Year,y=Abstained_Percentage,col="maroon"))+geom_line()+geom_smooth()

ggplot(Votes_By_Year)+geom_line(aes(x=Year,y=Positive_Percentage,col="Green"))+
  geom_line(aes(x=Year,y=Negative_Percentage,col="maroon"))+
  geom_line(aes(x=Year,y=Abstained_Percentage,col="red"))+
  xlab("Year")+
  ylab("Percentage")+
  ggtitle("Percentage Changes w.r.t. Year")
#alternative method
# library(dplyr)
# Year=votes%>%
#   group_by(votes$Year)%>%
#   summarise(Total_Votes=n())
# 
# By_Country=votes%>%
#   group_by(votes$Country)%>%
#   summarise(Total_Votes=n())

total_votes_by_country=setNames(aggregate(votes[votes$vote<=3,"Country"],by=list(votes[votes$vote<=3,"Country"]),
                                       FUN="length"),c("Country","Total_Votes"))

positive_votes_by_country=setNames(aggregate(votes[votes$vote==1,"Country"],by=list(votes[votes$vote==1,"Country"]),
                                          FUN="length"),c("Country","Positive_Votes"))

negative_votes_by_country=setNames(aggregate(votes[votes$vote==3,"Country"],by=list(votes[votes$vote==3,"Country"]),
                                          FUN="length"),c("Country","Negative_Votes"))

Votes_By_Country = Reduce(function(x,y) merge(x,y,all=TRUE),list(total_votes_by_country,positive_votes_by_country
                          ,negative_votes_by_country))

Votes_By_Country$Positive_percentage = round((Votes_By_Country$Positive_Votes/Votes_By_Country$Total_Votes)*100,2)
Votes_By_Country$Negative_percentage = round((Votes_By_Country$Negative_Votes/Votes_By_Country$Total_Votes)*100,2)



# 3. Find top 5 countries which vote mostly positive.


sorted_votes_by_country_positive = Votes_By_Country[order(Votes_By_Country$Positive_percentage,decreasing = TRUE),]
top_5_positive=sorted_votes_by_country_positive[1:5,c(1,3,5)]

# 4. Find top 5 countries which vote mostly negative.

sorted_votes_by_country_negative = Votes_By_Country[order(Votes_By_Country$Negative_percentage,decreasing = TRUE),]
top_5_negative=sorted_votes_by_country_negative[1:5,c(1,4,6)]


#Concatinating Data Frame

posneg<-rbind(top_5_positive,top_5_negative)
# 5. Create stacked and grouped bar charts showing votes distribution from 1985 in 3 
# categories, Positive, Negative, Abstain.

votes_factor <- votes
votes_factor$vote <- factor(votes_factor$vote)#Converting to factor to use as fill in ggplot
str(votes_factor)

#renamig factors
levels(votes_factor$vote)[levels(votes_factor$vote) == "1"] <- "Positive"
levels(votes_factor$vote)[levels(votes_factor$vote) == "2"] <- "Abstain"
levels(votes_factor$vote)[levels(votes_factor$vote) == "3"] <- "Negative"

#stacked bar 
ggplot(votes_factor, aes(Year, fill=vote)) + geom_bar()
ggplot(votes_factor[votes_factor$Year > 1985,], aes(Year, fill=vote)) + geom_bar()

#grouped bar
ggplot(votes_factor[votes_factor$Year > 1985,], aes(Year, fill=vote)) + geom_bar(position = "dodge")
ggplot(votes_factor[votes_factor$Year > 1985,], aes(Year, fill=vote)) + geom_bar(position = "fill")

