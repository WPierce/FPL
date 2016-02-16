#load libraries
library(lpSolve)
library(stringr)
library(RCurl)
library(jsonlite)
library(plyr)
# scrape the data
df = ldply(1:638, function(x){
# Scrape responsibly kids, we don't want to ddos
# the Fantasy Premier League's website
Sys.sleep(0.1)
url = sprintf("http://fantasy.premierleague.com/web/api/elements/%s/?format=json", x)
json = fromJSON(getURL(url))
json$now_cost = json$now_cost / 10
data.frame(json[names(json) %in%
c('web_name', 'team_name', 'type_name', 'now_cost', 'total_points', 'form', 'status', 'clean_sheets')])
})
print("Scraping finished")

#convert form from factor into num so it can be used to optimise
df$form <- as.numeric(levels(df$form))[df$form]

#Create the constraints
num_gk = 2
num_def = 5
num_mid = 5
num_fwd = 3
max_cost = 104.5
#Create current team constraint 

# Create vectors to constrain by position
df$Goalkeeper = ifelse(df$type_name == "Goalkeeper", 1, 0)
df$Defender = ifelse(df$type_name == "Defender", 1, 0)
df$Midfielder = ifelse(df$type_name == "Midfielder", 1, 0)
df$Forward = ifelse(df$type_name == "Forward", 1, 0)

#discount any player who is injured or not available
df$Available = ifelse(df$status == "a", 1, 0)

# Create vector to constrain by max number of players allowed per team
team_constraint = unlist(lapply(unique(df$team_name), function(x, df){
ifelse(df$team_name==x, 1, 0)
}, df=df))
# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", rep("<=", 21), "=")

# The vector to optimize against
objective = df$form

# Put the complete matrix together
const_mat = matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
df$now_cost, team_constraint, df$Available),
nrow=(6 + length(unique(df$team_name))),
byrow=TRUE)
const_rhs = c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(3, 20), 15)
# And solve the linear system
x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
print(arrange(df[which(x$solution==1),], desc(Goalkeeper), desc(Defender), desc(Midfielder), desc(Forward)))
