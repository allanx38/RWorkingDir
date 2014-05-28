# --- stargazer
library(stargazer)
data(attitude)
head(attitude)
linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical, data=attitude)
linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
attitude$high.rating <- (attitude$rating > 70)
probit.model <- glm(high.rating ~ learning + critical + advance, data=attitude, family = binomial(link = "probit"))
stargazer(linear.1, linear.2, probit.model, title="Regression Results", align=TRUE, dep.var.labels=c("Overall Rating","High Rating"), covariate.labels=c("Handling of Complaints","No Special Privileges", "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"), omit.stat=c("LL","ser","f"), no.space=TRUE)

#prints summary type table
stargazer(attitude)

Dax <- read.csv("D://Allan//DropBox//RWorkingDir//Data//CommonDate//Dax_2000_d.csv")
stargazer(head(Dax))
stargazer(summary(Dax))
