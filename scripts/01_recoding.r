# Recoding ---------------------------------------------------------------------------

source(here::here("scripts","00_setup.R"))

# Working directory from .Rproj
here::here("")

#Opening datasets-----------------------------------------------------------------

#Opening the election datset
election <- read_csv("data/view_election.csv")
View(election)

#Opening the party dataset
party <- read_csv("data/view_party.csv")
View(party)

#Selecting only the party family variables from the party dataset
family <- select(party, party_id, family_name, family_name_short)

#Merging the two datsets
election <- merge(election, family, by= "party_id")
View(election)

#Selecting variables-----------------------------------------------------------
election2 <- select(election, country_name, election_type, election_date, vote_share, party_name, party_name_english, party_id, family_name, family_name_short, left_right)
View(election2)

#Selecting countries 
election3 <- filter(election2,
                    country_name %in% c( "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
                                         "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                                         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                                         "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
                                         "Slovenia", "Spain", "United Kingdom", "Sweden"))

#Dropping missing values---------------------------------------------------------------
election4 <- na.omit(election3)
dim(election4)

apply(election4, 2, function(x) sum(is.na(x)))

#Finally managed to see the list of countries to check if they're all there 
#and in fact I found some missing, so I corrected the list.
countries <- election4 %>% 
  group_by(country_name) %>%
  summarize(
    votes_m = mean(vote_share, na.rm = T)
  )
View(countries)


#Election starting from the first european elections
election5 <- filter(election4, 
                    election_date > "1979-06-03")

#Filtering the party family of ecologist parties
election6 <- filter(election5,
                    family_name_short == "eco")




