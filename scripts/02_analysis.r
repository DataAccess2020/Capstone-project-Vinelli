# Analysis-------------------------------------------------------

# Simple mean of vote share for european elections and national elections.
eco_votes <- election6 %>% 
  group_by(election_type) %>%
  summarize(
    eco_votes_m = mean(vote_share, na.rm = T)
  )
eco_votes

t.test(election6$vote_share~election6$election_type)
#Resulted NOT statistically significant

#Table of vote share for type of election by coutries
table1 <- election6 %>%
  group_by(country_name,
           election_type) %>%
  summarize_at(vars(vote_share), funs(mean))
View(table1)

election7 <- mutate(
  election6,
  year = ifelse(election6$election_date <= "1979-10-23", 1970,
ifelse(election6$election_date <= "1989-11-05", 1980,
ifelse(election6$election_date <= "1999-10-03", 1990,
ifelse(election6$election_date <= "2009-10-04", 2000,
ifelse(election6$election_date <= "2019-12-12", 2010)))))
) 
View(election7)

#Table of mean of votes by year and by election type
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

#First graph------------------------------------------------------------------
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
  ggtitle("Percentages of votes to green parties for type of election")

election9 <- filter(election8,
                    country_name %in% c ("Austria", "Belgium", "Czech Republic", "Denmark", 
                                         "Estonia", "Finland", "France", "Germany", "Greece", 
                                         "Hungary", "Ireland", "Italy", "Luxembourg", "Malta",
                                         "Netherlands", "Portugal", "Slovenia", "Sweden", "United Kingdom"))
View(election9)

#Second graph---------------------------------------------------------------------
ggplot(election9, aes(x = year2, y = vote_share)) +
  geom_line(aes(col = election_type)) +
  facet_wrap(~country_name, ncol = 5) +
  scale_fill_manual(values = c("springgreen4", "steelblue"), labels= c("european elections", "parliamentary elections"),
                    "Type of election") +   ylab("Share of votes") +
  scale_color_hue(labels = c("european elections", "parliamentary elections"))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  xlab("Year") +
  scale_x_continuous(breaks = seq(1979, 2019, by = 8)) +
  theme_bw()+
  ggtitle("Percentages of votes to green parties among countries")


#Third graph----------------------------------------------------------------------
ggplot(election9, aes(x = year2, y = vote_share, fill = election_type)) +
  geom_bar(stat = "identity", position = "dodge", col = "black", width = 1.0005) +
  facet_wrap(~country_name, ncol = 5) +
  scale_fill_manual(values = c("springgreen4", "steelblue"), labels= c("european elections", "parliamentary elections"),
                    "Type of election") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  ylab("Share of votes") +
  xlab("years") +
  theme_bw() +
  theme(legend.position = "bottom")+
  ggtitle("Percentages of votes to green parties among countries")


#Fourth graph------------------------------------------------------------------------
ggplot(election9, aes(x = year2, y = vote_share, fill = election_type)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  scale_fill_manual(values = c("springgreen4", "steelblue"), labels= c("european elections", "parliamentary elections"),
                    "Type of election") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  ylab("Share of votes") +
  xlab("years") +
  theme_bw() +
  theme(legend.position = "bottom")+
  ggtitle("Percentages of votes to green parties for type of election over time")



#Trying to see which country, and when, did not have green parties at the parliamentary elections.
parliament_eco <- filter(election3,
                family_name_short =="eco",
                election_type == "parliament")
parliament_eco2 <- select(parliament_eco, country_name, party_name_english, election_date, vote_share)

#Creating a variable to see quickly all the missing values
parliament_eco2$missing <- ifelse (is.na(parliament_eco2$vote_share), 0 ,1)

parliament_eco2 <- arrange(parliament_eco2, missing, country_name, election_date)
View(parliament_eco2)
#Checking if missing values correspond only to certain parties but there are still some greens.
parliament_eco2 <- arrange(parliament_eco2, country_name, election_date)
View(parliament_eco2)



#Portugal in 1983, 1985, 1987, 1991, 1995, 1999, 2002, 2005 had no green parties; Starts from 2011.
#Lithuania in 1990
#Romania in 1992 had only one green party, in 96 none.
#Hungary had one green party in 2014.
#France had only one green party in 1993.
#Italy had no green parties in 1994 and 1996.
#Lithuania had no greens in 1990

#Attention: Checking if some Countries had green parties only later on.
#For example Spain in 2019.
spain <- filter(election3,
                country_name == "Spain",
                family_name_short =="eco")
View(spain)

#Come controllo questa cosa?

#Since when did Greens partecipated in european elections? and parliamentary?
greens_enter <- filter(election4,
                       family_name_short == "eco")
View(greens_enter)



