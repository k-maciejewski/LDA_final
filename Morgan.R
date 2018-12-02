data <- read.csv("../final_data/frmgham2.csv") 

data <- janitor::clean_names(data)

attach(data)

library(ggplot2)
library(dplyr)

data$sex <- recode(data$sex, `1` = "male", `2` = "female")
data$cursmoke_char <- recode(data$cursmoke, `1` = "yes", `0` = "no")

ggplot(aes(x = age, y = cigpday), data = data) + geom_point()

ggplot(aes(x = age, y = cigpday), data = data) + geom_point() + 
  facet_grid(~sex)

prop <- round(colSums(is.na(data))/dim(data)[1], 3)

knitr::kable(sort(prop, decreasing = TRUE)[1:9], col.names = "Proportion of NAs")

prob.data <- data %>%
  group_by(period) %>% 
  summarise(sysbp_prob = sum(sysbp, na.rm = TRUE)/n())

##plot these probabilities 
ggplot(prob.data, aes(y=sysbp_prob,
                      x=period)) + 
  geom_line() + 
  labs(color = 'Avg Systolic BP')


prob.data <- data %>%
  group_by(period) %>% 
  summarise(diabp_prob = sum(diabp, na.rm = TRUE)/n())

##plot these probabilities 
ggplot(prob.data, aes(y=diabp_prob,
                      x=period)) + 
  geom_line() + 
  labs(color = 'Avg Diastolic BP')

prob.data <- data %>%
  group_by(period) %>% 
  summarise(totchol_prob = sum(totchol, na.rm = TRUE)/n())

##plot these probabilities 
ggplot(prob.data, aes(y=totchol_prob,
                      x=period)) + 
  geom_line() + 
  labs(color = 'Avg Total Chol BP')


cdplot(factor(cursmoke) ~ sysbp, data=data, 
       ylab = "Current Smoking Status", 
       xlab = "Systolic BP")

cdplot(factor(cursmoke) ~ diabp, data=data, 
       ylab = "Current Smoking Status", 
       xlab = "Diatolic BP")

cdplot(factor(cursmoke) ~ totchol, data=data, 
       ylab = "Current Smoking Status", 
       xlab = "Total Cholesterol")

ggplot(data) + geom_boxplot(aes(x = factor(cursmoke), y = sysbp))
ggplot(data) + geom_boxplot(aes(x = factor(cursmoke), y = diabp))
ggplot(data) + geom_boxplot(aes(x = factor(cursmoke), y = totchol))


glm_sys <- glm(cursmoke ~ sysbp + 
                 sex + age + cigpday + bmi + diabetes + bpmeds + 
                 heartrte + glucose + educ + prevchd + prevap + prevmi +
                 prevstrk + prevhyp + time + hdlc + factor(period) + death + 
                 angina + mi_fchd + anychd + stroke + cvd + hyperten, 
                    family = "binomial", data = data, na.action = na.omit)
summary(glm_sys)

glm_sys <- glm(cursmoke ~ diabp + 
                 sex + age + cigpday + bmi + diabetes + bpmeds + 
                 heartrte + glucose + educ + prevchd + prevap + prevmi +
                 prevstrk + prevhyp + time + hdlc + factor(period) + death + 
                 angina + mi_fchd + anychd + stroke + cvd + hyperten, 
               family = "binomial", data = data, na.action = na.omit)
summary(glm_sys)

glm_sys <- glm(cursmoke ~ totchol + 
                 sex + age + cigpday + bmi + diabetes + bpmeds + 
                 heartrte + glucose + educ + prevchd + prevap + prevmi +
                 prevstrk + prevhyp + time + hdlc + factor(period) + death + 
                 angina + mi_fchd + anychd + stroke + cvd + hyperten, 
               family = "binomial", data = data, na.action = na.omit)
summary(glm_sys)
