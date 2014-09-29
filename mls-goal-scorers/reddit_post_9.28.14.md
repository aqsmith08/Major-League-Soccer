---
title: "How your team's goal scorer is over- or under-performing?"
output: html_document
---

Hi r/MLS -

I created a model that predicts how many non-penalty kick goals a player should score based on his Assists, Shots, Shots on Goal and Scoring Chance % -- all statistically significant variables.

The model is:

```
estimated_goals = -2.71810 + (Assists * -0.08948) + (Shots * 0.04016) + (Shots on Goal * 0.21365) + (Scoring Chance % * 0.20670)
```

For example, a player with 0 Assists, 100 Shots, 30 Shots on Goal and 20% Scoring Chance Percent would result in **~12** estimated goals.

By comparing a player's actual goals scored to what the model predicts, we can hypothesize that the player is over-performing or under-performing. (e.g. A player would over-perform the model if he scored 15 goals with the stats mentioned above.)

**Find the full results (for goal scorers with at least 5 goals from 2010 to 2014) below.** As always, I'm open to thoughts and feedback (especially on how I can improve my analysis). You can find my code and data on Github [**here**](https://github.com/aqsmith08/Major-League-Soccer/tree/master/mls-goal-scorers).