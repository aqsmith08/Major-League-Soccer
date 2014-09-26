
# Data found here - http://www.mlssoccer.com/standings
club_code <- c("DC", "SKC", "NE", "NYRB", "CLB", "PHI", "TFC", "HOU", "CHI", "MTL", "SEA", 
               "LAG", "RSL", "FCD", "POR", "VAN", "COL", "SJ", "CHV")

# Last Updated: 09/24/14 at 11:44 PM
pts <- c(48, 45, 42, 41, 40, 38, 37, 33, 31, 24, 54, 54, 49, 48, 39, 37, 31, 28, 24)

# If the playoffs started today, would the team be in?
playoffs <- c("Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes",
              "No", "No", "No", "No")

# Create a dataframe
club_pts <- data.frame(club_code, pts, playoffs)

# Bring in club payroll data
# All MLS player salary information can be found at 
# http://www.mlsplayers.org/salary_info.html

salary <- read.csv("mls_salary.csv", header = T, as.is = T)
salary_2014 <- subset(salary, salary$Year == "2014")

# Sum each team's payroll
mls_payroll <- aggregate(as.numeric(salary_2014$Base_Salary) ~ salary_2014$Club_Code, 
                         data = salary_2014,
                         FUN = sum,
                         na.rm = T)

# Merge the dataframes based on club code
final <- merge(club_pts, mls_payroll, by.x = "club_code", by.y = "salary_2014$Club_Code")

names(final)[4] <- c("Payroll")

# Create a scatter plot with x-axis of MLS Team Points and y-axis of Team Payroll
graph <- qplot(x = final$pts, 
              y = final$Payroll / 1000000, # y-axis will be in $MM
              data = final,
              main = "MLS Teams with higher payrolls tend to make the playoffs",
              xlab = "Points (as of 9/24/14)",
              ylab = "Team Payroll (in $MM)",
              label = final$club_code, 
              geom = "text",
              color = final$playoffs)

# Style the visualization
graph + scale_colour_discrete(name = "Clinched playoff spot\nif it started today?",
                            breaks = c("No", "Yes"),
                            labels = c("No", "Yes")) +
        theme(title = element_text(face = "bold", 
                                   color = "black", 
                                   size = 14))
