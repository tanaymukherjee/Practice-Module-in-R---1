# Homework Assignment 1
# STA 9750
# Tanay Mukherjee

# ----------------------------------------------
# The Ultimate Halloween Candy Power Ranking

library(dplyr)
library(tidyr)
library(ggplot2)

library(fivethirtyeight)
data(candy_rankings)
str(candy_rankings)
head(candy_rankings)

# 1. Find the top 5 best rated and top 5 worst rated candy.
candy_rankings %>% View()

candy_rank <- candy_rankings %>% select(competitorname, winpercent) %>% arrange(desc(winpercent))

head(candy_rank, n=5)
tail(candy_rank, n=5)

# 2. Plot winpercent against sugarpercent. Do you see any association?
# Now, plot winpercent against pricepercent. Do you see any association?

candy_rankings %>% ggplot(aes(x=sugarpercent, y=winpercent)) + geom_point() +
  xlab("Sugar Percent") + ylab ("Win Percent") + ggtitle ("Scatter plot: Sugar Percent v/s Win Percent") + geom_smooth()


candy_rankings %>% ggplot(aes(x=pricepercent, y=winpercent)) + geom_point() +
  xlab("Price Percent") + ylab ("Win Percent") + ggtitle ("Scatter plot: Price Percent v/s Win Percent") +  geom_smooth()


library(GGally)
candy_corr <- candy_rankings %>% select(competitorname, sugarpercent, pricepercent, winpercent)
ggcorr(candy_corr, label = TRUE)




x = candy_rankings$sugarpercent
y = candy_rankings$pricepercent
z = candy_rankings$winpercent

# Computation of sample slope, intercept and residuals
lm(z~x)
summary(lm(z~x))

lm(z~y)
summary(lm(z~y))







# 3. Consider all the logical-type variables in the dataset.
# For each logical variable, find the average difference in winpercent between 
# the treats that satisfy the condition and the treats that don't satisfy it.
# Which logical variable seems to have the strongest effect on winpercent?


candy_convert <- candy_rankings
candy_convert[2:10] <- lapply(candy_convert[2:10],as.numeric)
str(candy_convert)                                          

new <- candy_rankings[2:10] %>% colnames()
new

diff <- function(arg) {
  candy_select <- candy_convert %>% select(arg, winpercent) %>%
    group_by_(arg)%>% summarize(avg = mean(winpercent)) %>% 
    mutate(Diff = avg - lag(avg, default = avg[1])) 
  candy_select[2,"Diff"]
  return(abs(candy_select[2,"Diff"]))
}

for (i in new) {
  print(paste(i,diff(i)))
}





# ----------------------------------------------
# College admissions dataset

cad <- read.csv("http://vicpena.github.io/admin.csv")

str(cad)
head(cad)
class(cad)

# 1. Find the percentage of men who applied and got in and the percentage
# of women who applied and got in. What do you see?


cad1 <- cad %>% group_by(Admit,Gender) %>% summarize(Total_by_g_a = sum(Freq)) %>%
  group_by(Gender) %>% mutate(Total_by_g = sum(Total_by_g_a)) %>%
  group_by(Gender) %>% mutate(Percentage = 100*(Total_by_g_a/Total_by_g)) %>%
  arrange(desc(Gender)) %>% View()
cad1

# 2. Now, find the percentage of men who applied and got in by department.
# Do the same with women. Compare the results with what you found in part 1.

cad2 <- cad %>% group_by(Admit,Dept,Gender) %>% summarize(Total_g_a_d = sum(Freq)) %>% 
  group_by(Dept,Gender) %>% mutate(Total_g_d = sum(Total_g_a_d)) %>%
  group_by(Gender) %>% mutate(Percentage = 100*(Total_g_a_d/Total_g_d)) %>% 
  arrange(desc(Gender)) %>% filter(Admit=="Admitted") %>% View()
cad2

# 3. Explain what is going on in this dataset.
# Do you see any evidence of gender discrimination?

cad2 %>% ggplot(aes(x=cad2$Dept, y=cad2$Percentage, group = cad2$Gender, colour = Gender)) +
  geom_line() + xlab("Departments") + ylab("Percentage Admits") + 
  ggtitle("Line chart by gender demonstrating percentage admits by departments") 
  


# ----------------------------------------------
# Fandango movie ratings

data(fandango)
str(fandango)
head(fandango)
?fandango

# 1. Identify the Top 5 best rated and Top 5 worst rated movies in the dataset.
# Average over different platforms.

write.csv(fandango, file = "C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\HW 1\\fandango.csv")

# We will sum the rating for 6 platofrms and then divide by 5
# to get the average across all platforms
fandango_rank <- fandango %>% rowwise() %>% 
  mutate(all_platforms_rating_sum = sum(rt_norm,rt_user_norm,metacritic_norm,metacritic_user_nom,imdb_norm)) %>%
  mutate(new_avg_rating = all_platforms_rating_sum/5) %>% select(film,new_avg_rating) %>% arrange(desc(new_avg_rating))
  
head(fandango_rank, n=5)
tail(fandango_rank, n=5)


fandango_rank <- fandango %>% rowwise() %>% 
  mutate(new_avg_rating = mean(rt_norm,rt_user_norm,metacritic_norm,metacritic_user_nom,imdb_norm)) %>%
  select(film,new_avg_rating) %>% arrange(desc(new_avg_rating))


# 2. Visualize the difference between Fandango stars and actual Fandango ratings.
# Comment on what you see.
fandango %>% ggplot(aes(x=fandango_stars, y=fandango_ratingvalue)) + geom_point() +
  xlab("Fandango Stars") + ylab ("Fandango Ratings") + ggtitle ("Scatter plot: Fandango - Stars v/s Ratings") + geom_smooth(method = "lm")

x = fandango$fandango_stars
y = fandango$fandango_ratingvalue

f_corr <- fandango %>% select(fandango_stars, fandango_ratingvalue)
ggcorr(f_corr, label = TRUE)


hist(y, prob=TRUE, ylim=c(0,.06), breaks=10) +
  curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


# 3. Some movies are loved by the critics, but hated by the audience (and
# sometimes, it's the other way around). Given the data you have, create a metric
# to measure discrepancies between user and critic ratings. Create a table that
# contains the Top 5 movies that seem to appeal to critics but not the audience,
# and another table with the Top 5 movies that users seem to like more than critics do.

fandango_rank_comp <- fandango %>% rowwise() %>% 
  mutate(all_critics_rating_sum = sum(rt_norm,metacritic_norm)) %>%
  mutate(new_avg_critic_rating = all_critics_rating_sum/2) %>%
  mutate(all_users_rating_sum = sum(rt_user_norm,metacritic_user_nom)) %>%
  mutate(new_avg_users_rating = all_users_rating_sum/2) %>%
  mutate(critic_user_diff = new_avg_critic_rating - new_avg_users_rating) %>%
  select(film,critic_user_diff) %>% arrange(desc(critic_user_diff))

head(fandango_rank_comp, n=5)
tail(fandango_rank_comp, n=5)


fandango_rank_comp <- fandango %>% rowwise() %>% 
  mutate(new_avg_critic_rating = mean(rt_norm,metacritic_norm)) %>%
  mutate(new_avg_users_rating = mean(rt_user_norm,metacritic_user_nom)) %>%
  mutate(critic_user_diff = new_avg_critic_rating - new_avg_users_rating) %>%
  select(film,critic_user_diff) %>% arrange(desc(critic_user_diff))


# ----------------------------------------------
# Lahman Baseball Dataset

lbd_teams <- read.csv("http://vicpena.github.io/sta9750/Teams1719.csv")

str(lbd_teams)
head(lbd_teams)

# 1. Create a statistic that quantifies "home advantage".
# You'll use this statistic for the next few questions.
# There is more than one reasonable choice here.
# Propose 2 different statistics and justify why you picked the one you'll use from now on.

# statistic = (HomeW/(HomeW + HomeL)) / (AwayW/(AwayW + AwayL)))

# 2. Find home advantage statistics for the American League (AL) and National League (NL) in the 2017-2019 period.
# Comment on the results. Do you see any differences between leagues?
# Do you see any evidence of home advantage at all? What are the years where there seems to be more
# of a home advantage, and those where the effect might not be as strong
# (or doesn't seem to be there)?

lbd_teams <- lbd_teams %>% mutate(statistic_home_adv = (HomeW/(HomeW + HomeL)) / (AwayW/(AwayW + AwayL)))

lbd_teams %>% group_by(League) %>% summarize(home_adv_ratio = mean(statistic_home_adv))
lbd_teams
lbd_teams %>% group_by(League,Year) %>% summarize(home_adv_ratio = mean(statistic_home_adv))


# 3. Find the teams that had the highest and lowest home advantage effect
# by league in 2017,2018, and 2019 separately. Comment on the results.

library(sqldf)

lbd_top <- lbd_teams %>% group_by(League,Team,Year) %>%
  summarize(home_adv_ratio = mean(statistic_home_adv)) %>%
  arrange(desc(home_adv_ratio,Year))

lbd_top %>% View()


sqldf("select * from ( select *, dense_rank() over 
(partition by Year,League order by home_adv_ratio desc)rn from lbd_top) where rn=1")
      
sqldf("select * from ( select *, row_number() over 
(partition by Year, League order by home_adv_ratio asc)rn from lbd_top) where rn=1")



# 4. Which franchise had the highest average home advantage in the 2017-2019 period?
# Which one had the lowest average home advantage effect?

lbd_teams %>% group_by(Team) %>% summarize(home_adv_ratio = mean(statistic_home_adv)) %>%
  arrange(desc(home_adv_ratio)) %>% View ()

lbd_teams %>% group_by(Team) %>% summarize(home_adv_ratio = mean(statistic_home_adv)) %>%
  arrange(desc(home_adv_ratio)) %>% top_n(1)

lbd_teams %>% group_by(Team) %>% summarize(home_adv_ratio = mean(statistic_home_adv)) %>%
  arrange(desc(home_adv_ratio)) %>% top_n(-1)


# 5. After completing these exercises, what did you learn about home advantage effect
# in the MLB? You're welcome to try out a few new queries to illustrate your points.


x <- lbd_teams %>% group_by(statistic_home_adv > 1 ) %>% summarize(total_home_adv = n())





#lbd_teams %>% filter(League == "NL")

lbd_teams %>% ggplot(aes(x=lbd_teams$statistic_home_adv, y=lbd_teams$Team, group = lbd_teams$Year, colour = lbd_teams$Year)) +
  geom_line() + xlab("Departments") + ylab("Percentage Admits") + 
  ggtitle("Line chart by gender demonstrating percentage admits by departments") 





lbd_teams %>% ggplot(aes(x=lbd_teams$statistic_home_adv, y=lbd_teams$Year, group = lbd_teams$League, colour = lbd_teams$League)) +
  geom_line() + xlab("Departments") + ylab("Percentage Admits") + 
  ggtitle("Line chart by gender demonstrating percentage admits by departments") 














# ----------------------------------------------
# Lahman Baseball Dataset

# In this exercise, you'll work with the Lahman Baseball datasets, which you can access
# after installing library(Lahman). After installing the package, you can type in ?Lahman to
# get some information on the structure of the datasets and see what's available. If you
# want to do a class project with baseball data, you're welcome to use this resource.

#install.packages("Lahman")
library(Lahman)
lahman_pitching <- Lahman::Pitching
lahman_people <- Lahman::People

colnames(lahman_pitching)
colnames(lahman_people)

# Aging in pitchers and batters

# 1. Let's consider data from 2018 only and look at the subset of pitchers who pitched
# more than 250 outs. Plot the earned run average (ERA; small values are good
# and big ones are bad) of the pitchers against their age. Do you see any patterns?
# Now, find a table with the average ERAs by age. Do you see any patterns?

lahman_age <- lahman_pitching %>% inner_join(lahman_people, by = "playerID") %>%
  filter(yearID == 2018 & IPouts > 250 ) %>% mutate(Age = yearID - birthYear) 

lahman_age %>% View()

lahman_age %>% ggplot(aes(x=Age, y=ERA)) + geom_point() + geom_smooth()

lahman_age %>% group_by(Age) %>% summarize(Avg_ERA = mean(ERA)) %>% View()




# 2. Again, let's look at pitchers who pitched more than 250 outs in 2018. Identify the
# top 5 best and worst pitchers, in terms of ERA.

lahman_age %>% select(playerID, ERA, nameFirst, nameLast) %>% arrange(ERA) %>% head(n=5)
lahman_age %>% select(playerID, ERA, nameFirst, nameLast) %>% arrange(ERA) %>% tail(n=5)


# 3. Consider the best pitcher (in terms of ERA) that you found in part 2. Find his ERA
# by season throughout his career. Based on this alone, do you think he's already
# "peaked"? If you like baseball, you're welcome to share your opinion here as well.

lahman_pitching %>% filter(playerID=="degroja01") %>% select(playerID, yearID, ERA) %>%
  arrange(desc(yearID))


# 4. Let's do a similar exercise, but now with batting average (BA; more is better).
# Use the battingStats function in Lahman to find BAs. Consider data from 2018
# only and look at players that have more than 200 at bats (AB). Plot BA against
# age. Do you see any patterns? Find a table with average BAs by age. Explain what you see.

lahman_batting_stats <- Lahman::battingStats()
colnames(lahman_batting_stats)
lahman_batting_stats %>% View()

lahman_batting_stats <- lahman_batting_stats %>% inner_join(lahman_people, by = "playerID") %>%
  filter(yearID==2018 & AB > 200) %>% mutate(Age = yearID - birthYear)

lahman_batting_stats %>% ggplot(aes(x=Age, y=BA)) + geom_point() + geom_smooth()

lahman_batting_stats %>% group_by(Age) %>% summarize(Avg_BA = mean(BA)) %>% View()

# 5. Again, let's look at players with more than 200 ABs in 2018. Find the top 5 best
# and worst players in terms of BA.

lahman_batting_stats %>% select(playerID, BA, nameFirst, nameLast) %>% arrange(desc(BA)) %>% head(n=5)
lahman_batting_stats %>% select(playerID, BA, nameFirst, nameLast) %>% arrange(desc(BA)) %>% tail(n=5)



#6. Consider the best player (in terms of BA) that you found in part 5. Find his BA by
# season throughout his career. Based on this alone, do you think he's already
# "peaked"? If you like baseball, you're welcome to share your opinion here as well.

lahman_batting_stats <- lahman_batting_stats %>% inner_join(lahman_people, by = "playerID") %>%
  filter(AB > 200) %>% mutate(Age = yearID - birthYear)

lahman_batting_stats %>% filter(playerID=="bettsmo01") %>% select(playerID, yearID, BA) %>%
  arrange(desc(yearID))
