new.df<-cleaned_data%>%
  drop_na()%>%
  select(-c(10:17))
## 0.1117
model <- glm(shortage_ind ~ mean_age_2016, family = binomial, data = new.df)
summary(model)
## 0.380015
model2 <- glm(shortage_ind ~ total_employ_2016, family = binomial, data = new.df)
summary(model2)
## 0.50574
model3 <- glm(shortage_ind ~ employ_prse_2016, family = binomial, data = new.df)
summary(model3)
## 0.379933
model4 <- glm(shortage_ind ~ jobs_per_1000_2016, family = binomial, data = new.df)
summary(model4)
## 0.361638
model5 <- glm(shortage_ind ~ mean_hourly_2016, family = binomial, data = new.df)
summary(model5)
## 0.41945
model6 <- glm(shortage_ind ~ mean_annual_2016, family = binomial, data = new.df)
summary(model6)
## 0.84139
model7 <- glm(shortage_ind ~ mean_prse_2016, family = binomial, data = new.df)
summary(model7)





#Anova
anova <- aov(shortage_ind ~ mean_age_2016 + unemployed_rate_2016+total_employ_2016 + employ_prse_2016+ jobs_per_1000_2016+ mean_hourly_2016+ mean_annual_2016+mean_prse_2016 , data = new.df)
summary(anova)

# Check the homogeneity of variance assumption
# The residuals versus fits plot can be used to check the homogeneity of variances.
# In the plot below, there is no evident relationships between residuals and fitted values (the mean of each groups), which is good. So, we can assume the homogeneity of variances.
plot(anova, 1)


# Check the normality assumption
# Normality plot of residuals. 
# In the plot below, the quantiles of the residuals are plotted against the quantiles of the normal distribution. A 45-degree reference line is also plotted.
plot(anova, 2)
