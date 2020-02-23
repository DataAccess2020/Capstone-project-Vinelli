library(tidyverse)
library(rio)
library(readr)
library(dplyr)

setwd("C:/Users/vinel/Documents/2_DATA ACCESS/CAPSTONE")
parlgov <- import("parlgov-stable.xlsx")

#Opening the datset -----------------------------------------------------------
election <- read_csv("C:/Users/vinel/Documents/2_DATA ACCESS/CAPSTONE/view_election.csv")
View(election)

#Opening the party dataset
party <- read_csv("C:/Users/vinel/Documents/2_DATA ACCESS/CAPSTONE/view_party.csv")
View(party)

#Selecting only the party family variables
family <- select(party, party_id, family_name, family_name_short)

#Merging the two datsets
election <- merge(election, family, by= "party_id")
View(election)

#Selecting variables-----------------------------------------------------------
election2 <- select(election, country_name, election_type, election_date, vote_share, party_name, party_name_english, party_id, family_name, family_name_short, left_right)
View(election2)

#Selecting countries and types of elections.
election3 <- filter(election2,
                    country_name %in% c( "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
                                         "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                                         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                                         "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
                                         "Slovenia", "Spain", "United Kingdom", "Sweden"))
#Removing non-european countries

#drop missing
election4 <- na.omit(election3)
dim(election4)

apply(election4, 2, function(x) sum(is.na(x)))

#Trying to see the list of countries but FAIL
election$country_name
election[, "country_name"]

#Finally managed to see the list of countries to check if they're all there and in fact found
#some missing. Corrected the list.
countries <- election4 %>% 
  group_by(country_name) %>%
  summarize(
    votes_m = mean(vote_share, na.rm = T)
  )
View(countries)


#Election starting from the first european elections
election5 <- filter(election4, 
                    election_date > "1979-06-03")

#Now I want to see if green parties do better at european elections
election6 <- filter(election5,
                    family_name_short == "eco")

# Simple mean of vote share for european elections and national.
eco_votes <- election6 %>% 
  group_by(election_type) %>%
  summarize(
    eco_votes_m = mean(vote_share, na.rm = T)
  )
eco_votes

t.test(election6$vote_share~election6$election_type)
#NOT statistically significant 


#Table of vote share for type of election by coutries
table1 <- election6 %>%
  group_by(country_name,
           election_type) %>%
  summarize_at(vars(vote_share), funs(mean))
View(table1)

#nope
election5$year <- 1



election7 <- mutate(
  election6,
  year = ifelse(election6$election_date <= "1979-10-23", 1970,
                ifelse(election6$election_date <= "1989-11-05", 1980,
                       ifelse(election6$election_date <= "1999-10-03", 1990,
                              ifelse(election6$election_date <= "2009-10-04", 2000,
                                     ifelse(election6$election_date <= "2019-12-12", 2010)))))
) 
View(election7)

#table of mean of votes by year and by election type
eco_votes_year <- election7 %>% 
  group_by(year,election_type) %>%
  summarize(
    eco_votes_m = mean(vote_share, na.rm = T)
  )
View(eco_votes_year)

parties <- election7 %>%
  group_by(country_name,
           election_type,
           party_name_english,
           year,
           election_date) %>%
  summarize_at(vars(vote_share), funs(mean))
View(parties)



#primo grafico 
election7 %>%
  count(election_type, year) %>%
  mutate(vote_share =n / sum(n)) %>%
  ggplot(aes(year, vote_share, color = election_type))+
  geom_line()+
  geom_point()+
  scale_color_hue(labels = c("european elections", "parliamentary elections"))+
  scale_y_continuous(labels = scales::percent)+  
  scale_x_continuous(breaks = seq(1970, 2010, by =10))+
  labs(x= "Year",
       y= "Percentages of votes",
       color = "")+
  ggtitle("Percentages of votes to green \n parties for type of election")

#primo grafico smooth
election7 %>%
  count(election_type, year) %>%
  mutate(vote_share =n / sum(n)) %>%
  ggplot(aes(year, vote_share, color = election_type))+
  geom_smooth(se =FALSE)+
  scale_color_hue(labels = c("european elections", "parliamentary elections"))+
  scale_y_continuous(labels = scales::percent)+  
  scale_x_continuous(breaks = seq(1970, 2010, by =10))+
  labs(x= "Year",
       y= "Percentages of votes",
       color = "")



election8 <- mutate(
  election7,
  year2 = ifelse(election7$election_date <= "1979-10-23", 1979,
ifelse(election7$election_date <= "1980-10-05", 1980,
ifelse(election7$election_date <= "1981-12-08", 1981,
ifelse(election7$election_date <= "1982-09-19", 1982,
ifelse(election7$election_date <= "1983-04-24", 1983,
ifelse(election7$election_date <= "1984-06-17", 1984,
ifelse(election7$election_date <= "1985-10-13", 1985,
ifelse(election7$election_date <= "1986-11-23", 1986,
ifelse(election7$election_date <= "1987-12-13", 1987,
ifelse(election7$election_date <= "1988-09-18", 1988,
ifelse(election7$election_date <= "1989-11-05", 1989,
ifelse(election7$election_date <= "1990-12-12", 1990,
ifelse(election7$election_date <= "1991-11-24", 1991,
ifelse(election7$election_date <= "1992-12-06", 1992,
ifelse(election7$election_date <= "1993-06-06", 1993,
ifelse(election7$election_date <= "1994-10-16", 1994,
ifelse(election7$election_date <= "1995-12-17", 1995,
ifelse(election7$election_date <= "1996-11-10", 1996,
ifelse(election7$election_date <= "1997-06-06", 1997,
ifelse(election7$election_date <= "1998-10-03", 1998,
ifelse(election7$election_date <= "1999-10-03", 1999,
ifelse(election7$election_date <= "2000-10-15", 2000,
ifelse(election7$election_date <= "2001-11-20", 2001,
ifelse(election7$election_date <= "2002-11-24", 2002,
ifelse(election7$election_date <= "2003-05-18", 2003,
ifelse(election7$election_date <= "2004-10-03", 2004,
ifelse(election7$election_date <= "2005-09-18", 2005,
ifelse(election7$election_date <= "2006-11-22", 2006,
ifelse(election7$election_date <= "2007-11-13", 2007,
ifelse(election7$election_date <= "2008-09-28", 2008,
ifelse(election7$election_date <= "2009-10-04", 2009,
ifelse(election7$election_date <= "2010-09-19", 2010,
ifelse(election7$election_date <= "2011-09-15", 2011,
ifelse(election7$election_date <= "2012-09-12", 2012,
ifelse(election7$election_date <= "2013-10-25", 2013,
ifelse(election7$election_date <= "2014-09-14", 2014,
ifelse(election7$election_date <= "2015-11-08", 2015,
ifelse(election7$election_date <= "2016-10-09", 2016,
ifelse(election7$election_date <= "2017-10-21", 2017,
ifelse(election7$election_date <= "2018-10-14", 2018,
ifelse(election7$election_date <= "2019-12-12", 2019)))))))))))))))))))))))))))))))))))))))))
)
View(election8)

#table of mean of votes for every year by election type
eco_votes_year2 <- election8 %>% 
  group_by(year2,election_type) %>%
  summarize(
    eco_votes_m = mean(vote_share, na.rm = T)
  )
View(eco_votes_year2)

#primo grafico ma con tutti gli anni
election8 %>%
  count(election_type, year2) %>%
  mutate(vote_share =n / sum(n)) %>%
  ggplot(aes(year2, vote_share, color = election_type))+
  geom_smooth(se= FALSE)+
  scale_color_hue(labels = c("european elections", "parliamentary elections"))+
  scale_y_continuous(labels = scales::percent)+  
  scale_x_continuous(breaks = seq(1979, 2019, by =5))+
  labs(x= "Year",
       y= "Percentages of votes",
       color = "")+
  ggtitle("Percentages of votes to green \n parties for type of election")






election9 <- filter(election8,
                    country_name %in% c ("Austria", "Belgium", "Czech Republic", "Denmark", 
                                                    "Estonia", "Finland", "France", "Germany", "Greece", 
                                                    "Hungary", "Ireland", "Italy", "Luxembourg", "Malta",
                                                    "Netherlands", "Portugal", "Slovenia", "Sweden", "United Kingdom"))
View(election9)


#secondo grafico prova
election9 %>%
  count(family_name_short, year2) %>%
  mutate(vote_share =n / sum(n)) %>%
  ggplot(election9, aes(x = year2, y = vote_share)) +
  geom_line(aes(col = election_type)) +
  facet_wrap(~country_name, ncol = 5) +
  ylab("Percentages of votes") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1979, 2019, by = 8)) +
  theme_bw()

#plot2
ggplot(election9, aes(x = year2, y = vote_share)) +
  geom_line(aes(col = election_type)) +
  facet_wrap(~country_name, ncol = 5) +
  scale_fill_manual(values = c("springgreen4", "steelblue"), labels= c("european elections", "parliamentary elections"),
                    "Type of election") +   ylab("Share of votes") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
    xlab("Year") +
  scale_x_continuous(breaks = seq(1979, 2019, by = 8)) +
  theme_bw()



#nope
ggplot(election9, aes(x = year2, y = vote_share)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~country_name, ncol = 5) +
  scale_fill_manual(values = c("tomato", "steelblue"), 
                    "Share of votes") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(1979, 2019, by = 8)) +
  ylab("Share of votes") +
  xlab("year") +
  theme_bw()

#trying to adjust the percenatges OLEE
ggplot(election9, aes(x = year2, y = vote_share, fill = election_type)) +
  geom_bar(stat = "identity", position = "dodge", col = "black", width = 1.0005) +
  facet_wrap(~country_name, ncol = 5) +
  scale_fill_manual(values = c("springgreen4", "steelblue"), labels= c("european elections", "parliamentary elections"),
                    "Type of election") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  ylab("Share of votes") +
  xlab("years") +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(election9, aes(x = year2, y = vote_share, fill = election_type)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  scale_fill_manual(values = c("springgreen4", "steelblue"), labels= c("european elections", "parliamentary elections"),
                    "Type of election") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  ylab("Share of votes") +
  xlab("years") +
  theme_bw() +
  theme(legend.position = "bottom")







#Quali Paesi hanno  partiti verdi solo alle europee?

table2 <- select(election8, country_name, election_type, party_name_english,party_id, year2)
table2 <- arrange(table2, country_name)
View(table2)
#problem: in election8 i removed missing which is what i am interested in right now
#let's use election3



table3 <- select(election3, country_name, election_type, vote_share, party_name_english, family_name_short, party_id, election_date)
table4 <- filter(table3,
                 family_name_short == "eco")
table4 <- arrange(table4, country_name)

View(table4)

table4 <- ifelse(table4$vote_share == "NA", country_name ,0)
View(table4)

##OLEEEEEEE
#voglio sapere quali paesi NON hanno partiti verdi alle parlamentari
#Tenere solo partiti verdi e solo elezioni parlamentari e vedere i missing

prova <- filter(election3,
                family_name_short =="eco",
                election_type == "parliament")
prova2 <- select(prova, country_name, party_name_english, election_date, vote_share)
prova2$missing <- ifelse (is.na(prova2$vote_share), 0 ,1)

#to see quickly all the missing values
prova2 <- arrange(prova2, missing, country_name, election_date)
View(prova2)
#to check if missing values correspond only to certain parties but there are still some greens.
prova2 <- arrange(prova2, country_name, election_date)
View(prova2)



#Portogallo nel 1983, 85, 87, 1991,95, 99, 2002, 2005 no green parties; dal 2011 si.
#Lithuania nel 1990
#Romania nel 1992 solo un partito, nel 96 nessuno.
#Ungheria nel 2014 solo un partito.
#Francia nel 1993 aveva solo un partito verde
#Italy no green parties nel 94 nè nel 96
#Lithuania no greens nel 1990

#Attention: mi viene il dubbio che alcuni Paesi abbiano avuto verdi alle parlamntari solo tardi
#Per esempio in spagna nel 2019.
spain <- filter(election3,
                country_name == "Spain",
                family_name_short =="eco")
View(spain)

#Come controllo questa cosa?

#Da che anno i verdi hanno gareggiato alle europee? e da che anno alle parlamentari?
seventeen <- filter(election3,
                    family_name_short =="eco",
                    election_date > "2016-12-11")
View(seventeen)


greens_enter <- filter(election4,
                       family_name_short == "eco")
View(greens_enter)


