results <- read_csv("data/sunlab_prediction.csv")

labels = c("Actual", "Ridge Regression (min)", "Ridge Regression (1se)", "Linear Regression", "SVR", "ANN")
results %>%
  filter(Datetime> as_date("2017-10-10") & Datetime < as_date("2017-10-27")) %>%
  gather(type,value, A_Optimal...Power.DC..W., min, fst, ln, svr, nn) %>%
  ggplot(aes(x=Datetime,y=value, group=type)) + geom_line(aes(linetype=type, color=type), alpha=0.8, size=0.5) + 
  geom_point(aes(color=type), size=0.2) +
  facet_zoom(x = Datetime > as.Date("2017-10-14") & Datetime < as.Date("2017-10-17"), horizontal = FALSE, zoom.size = 0.6)+
  scale_color_manual(name="Models", values=c("black", "red", "green", "blue", "orange", "purple"), labels=labels) +
  scale_linetype_manual(name="Models", values=c("solid", "dotted", "dotted", "dotted", "dotted", "dotted"), labels=labels) +
  labs(x="Date, 2017", y="Power (W)") + theme_bw() + theme(legend.position = "bottom")

library(scoring)
library(scales)

results_scaled <- as.data.frame(lapply(results, rescale))
results_scaled %<>% gather(model, outcome, min, fst, ln, svr, nn)

scores <- brierscore(A_Optimal...Power.DC..W. ~ outcome, group = model, data=results_scaled)
scores$brieravg
scores$rawscores
results_scaled$brierscore <- scores$rawscores

# Plot avg score & standard error
results_scaled %>% group_by(as.factor(model)) %>% 
  ggplot(aes(x=model, group=model)) + geom_boxplot(aes(y=brierscore))
