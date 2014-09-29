# PROBLEM STATEMENT: How does your Major League Soccer (MLS) team's goal scorer 
# compare to historical performances?

# My analysis only includes scorers with >= 5 non-Penalty Kick goals and performance is
# from the MLS 2010 - 2014 seasons only (created as of 9/28/14)

# Data found here - http://www.mlssoccer.com/stats/season
-----------------------------------------------------------------------------------
# Acronym Cheat Sheet
# GP: Games Played, GS: Games Started, G: Goals, MIN: Minutes Played, 
# A: Assists, SHT: Shots, SOG: Shots on Goal,mPKA: Penalty Kick Attempts
# PKG: Penalty Kick Goals, HmG: Home Goals, RdG: Road Goals, G.90min: Goals per 90 min
-----------------------------------------------------------------------------------
  
# Load the data
scorers <- read.csv("mls_goal_scorers_2010-2014.csv", header = T)

# Column headers
names(scorers)

# [1] "Player"         "Club"           "Club_Code"      "POS"            "Is_Forward"    
# [6] "Is_Midfield"    "GP"             "GS"             "MINS"           "G"             
# [11] "A"              "SHTS"           "SOG"            "GWG"            "PKG"           
# [16] "PKA"            "HmG"            "RdG"            "G.90min"        "Scoring_Chance"
# [21] "Year"           "Unique" 

# Add a new column with non-Penalty Kick Goals
scorers <- cbind(non_PKG = scorers$G - scorers$PKG, scorers)

# Create linear model to predict non-Penalty Kick Goals using all criteria
model_all <- lm(non_PKG ~ MINS + A + SHTS + SOG + Scoring_Chance, data = scorers)

# Show the result of the model
summary(model_all)
-----------------------------------------------------------------------------------
# Model Results

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)    -2.823e+00  6.157e-01  -4.586 6.98e-06 ***
#  MINS            8.011e-05  2.564e-04   0.312  0.75497    
#  A              -9.265e-02  3.424e-02  -2.706  0.00725 ** 
#  SHTS            3.942e-02  1.342e-02   2.939  0.00359 ** 
#  SOG             2.132e-01  2.921e-02   7.299 3.36e-12 ***
#  Scoring_Chance  2.076e-01  1.977e-02  10.499  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 1.704 on 266 degrees of freedom
# Multiple R-squared:  0.7328,  Adjusted R-squared:  0.7278 
# F-statistic: 145.9 on 5 and 266 DF,  p-value: < 2.2e-16
-----------------------------------------------------------------------------------

# Interesting that MINS were not significant in this model
# Run updated model with significant variables only
model_sig <- lm(non_PKG ~ A + SHTS + SOG + Scoring_Chance,
                  data = scorers)

# Show the result of the model
summary(model_sig)
-----------------------------------------------------------------------------------
# Model Results
  
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)    -2.71810    0.51486  -5.279 2.69e-07 ***
#  A              -0.08948    0.03265  -2.741  0.00654 ** 
#  SHTS            0.04016    0.01318   3.047  0.00255 ** 
#  SOG             0.21365    0.02912   7.336 2.64e-12 ***
#  Scoring_Chance  0.20670    0.01952  10.587  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 1.701 on 267 degrees of freedom
# Multiple R-squared:  0.7327,  Adjusted R-squared:  0.7287 
# F-statistic:   183 on 4 and 267 DF,  p-value: < 2.2e-16
-----------------------------------------------------------------------------------

# Add a new column with the model's estimated goal count for that player based on
# Assists, Shots, Shots on Goal and Scoring Chance Percent
scorers <- cbind(model_estimated_goals = (-2.71810 + 
                                          (scorers$A * -0.08948) + 
                                          (scorers$SHTS * 0.04016) + 
                                          (scorers$SOG * 0.21365) +
                                          (scorers$Scoring_Chance * 0.20670)), scorers)

# Create a simple dataframe to show the results
scorer_rank <- data.frame(scorers$Unique, 
                          scorers$non_PKG, 
                          scorers$model_estimated_goals)

# Add a new column with Goal Delta between actual non-Penalty Kick Goals 
# and model prediction of estimated goals
scorer_rank <- cbind(goal_delta = scorers$non_PKG - scorers$model_estimated_goals, 
                     scorer_rank)

# Sort the dataframe in descending order
scorer_rank_sorted <- scorer_rank[order(-scorer_rank$goal_delta),]

# Drop the Row Column
scorer_rank_sorted <- data.frame(scorer_rank_sorted$scorers.Unique,
                                 scorer_rank_sorted$goal_delta,
                                 scorer_rank_sorted$scorers.non_PKG,
                                 scorer_rank_sorted$scorers.model_estimated_goals)

# Rename the column headers
names(scorer_rank_sorted)[1] <- c("Player_Team_Year")
names(scorer_rank_sorted)[2] <- c("Goal_Delta")
names(scorer_rank_sorted)[3] <- c("Actual_non_PKG")
names(scorer_rank_sorted)[4] <- c("Model_Estimated_non_PKG")

# View the dataframe
View(scorer_rank_sorted)
