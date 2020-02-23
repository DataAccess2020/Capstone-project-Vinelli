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
  ggtitle("Percentages of votes to green \n parties for type of election")

