library(dplyr)
library(ggplot2)

checkins <- read.csv('./beers-data/beers_contest_checkin.csv')
beers_contest_player <- read.csv('./beers-data/beers_contest_player.csv')
############################
# Convert Datetime to Date #
############################

checkins$checkin_time <- as.character(checkins$checkin_time)
checkins$checkin_date <- as.Date(substr(checkins$checkin_time, 1, 10))

#create full year and id array

checkin_date <- as.Date(rep(seq.Date(as.Date('2016-01-10'), Sys.Date(), by = 'day'), 36))
contest_player_id <- rep(0:35, each=(Sys.Date()+1) - as.Date('2016-01-10'))
full.year.id <- as.data.frame(cbind(checkin_date,contest_player_id))
full.year.id$checkin_date <- as.Date(checkin_date)


######################################
# Calculate beers by date #
######################################

cum.checkins <- checkins %>%
                group_by(contest_player_id, checkin_date) %>%
                summarize(beers = n()) 
               
#merge full year with partial data

full.checkins <- merge(full.year.id,cum.checkins,all = TRUE)

#convert NA to 0

full.checkins[is.na(full.checkins)] <- 0

#set pace line for contestant 0

full.checkins$beers[full.checkins$contest_player_id == 0] <- .273

#merge on player name
player.name <- beers_contest_player[,c(2,7)]

player.name <- rename(player.name, contest_player_id = player_id)

full.checkins <- merge(full.checkins, player.name, all = TRUE)
full.checkins$user_name <- as.character(full.checkins$user_name)
full.checkins$user_name[full.checkins$contest_player_id == 0] <- 'Pace Line'

#Calculate cumulative beers

full.checkins <- full.checkins %>%
                 filter(!is.na(user_name)) %>%
                 arrange(contest_player_id, checkin_date) %>%
                 group_by(contest_player_id) %>%
                 mutate(cum.beers=cumsum(beers))

ggplot(full.checkins, aes(x=checkin_date, y=cum.beers, group = user_name , color = user_name))  + geom_line() + ylim(0,50)



