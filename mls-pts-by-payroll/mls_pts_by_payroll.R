# Make sure the ggplot2 package is loaded
# library(ggplot2)

# Enter MLS Club Codes and Point Totals
# Data found here - http://www.mlssoccer.com/standings
club_code <- c("DC", "SKC", "NE", "NYRB", "CLB", "PHI", "TFC", "HOU", "CHI", "MTL", "SEA", 
               "LAG", "RSL", "FCD", "POR", "VAN", "COL", "SJ", "CHV")

# Point Totals Last Updated: 09/26/14 at 1:22 PM
pts <- c(48, 45, 42, 41, 40, 38, 37, 33, 31, 24, 54, 54, 49, 48, 39, 37, 31, 28, 24)

# If the playoffs started today, would the team be in?
# Top Five Clubs in Each Conference Reach Playoffs
playoffs <- c("Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No", "No", "Yes",
              "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No")

# Create a dataframe of the data entered above
club_pts <- data.frame(club_code, pts, playoffs)

# Bring in club payroll data
# All MLS player salary information can be found at 
# http://www.mlsplayers.org/salary_info.html

# Load the data
salary <- read.csv("mls_salary.csv", header = T, as.is = T)

# Subset the salary data to 2014 only
salary_2014 <- subset(salary, salary$Year == "2014")

# Sum each team's payroll
mls_payroll <- aggregate(as.numeric(salary_2014$Base_Salary) ~ salary_2014$Club_Code, 
                         data = salary_2014,
                         FUN = sum,
                         na.rm = T)

# Merge the dataframes based on club code
final <- merge(club_pts, mls_payroll, by.x = "club_code", by.y = "salary_2014$Club_Code")

# Rename to simplify
names(final)[4] <- c("Payroll")

# Create a scatter plot with x-axis of MLS Team Points and y-axis of Team Payroll
graph <- qplot(x = final$pts, 
              y = final$Payroll / 1000000, # y-axis will be in $MM
              data = final,
              main = "MLS Teams by Points and Team Payroll",
              xlab = "Points (as of 9/26/14)",
              ylab = "Team Payroll ($MM)",
              label = final$club_code, 
              geom = "text",
              color = final$playoffs)

# Style the plot
graph + scale_colour_discrete(name = "Clinched playoff spot\nif it started today?",
                            breaks = c("No", "Yes"),
                            labels = c("No", "Yes")) +
        theme(title = element_text(face = "bold", 
                                   color = "black", 
                                   size = 16))
