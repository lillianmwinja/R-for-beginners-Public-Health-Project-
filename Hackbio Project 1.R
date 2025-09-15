
# URL in use
public_h<-"https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/nhanes.csv"

# Reading data from the URL
install.packages("readr")
library(readr)

health_status<-read_csv(public_h)

#Processing NAs
colSums(is.na(health_status))
health_status[sapply(health_status, is.numeric)] <-
  lapply(health_status[sapply(health_status, is.numeric)],
         function(x) ifelse(is.na(x), 0, x))

health_clean <- health_status[!is.na(health_status$Gender), ]

# Visualizing the distribution of BMI, Weight, Weight in pounds (weight *2.2) and Age with an histogram.
hist (health_clean$Weight, main="Weight Distribution",xlab="Weight", col = "purple")

hist (health_clean$Weight*2.2, main="Weight Distribution in pounds",xlab="Weight", col = "orange")

hist (health_clean$Age, main="Age Distribution",xlab="Age", col = "green")

hist (health_clean$BMI, main="BMI Distribution",xlab="BMI", col = "pink")


#Mean 60-second pulse rate

mean(health_clean$Pulse)

#Range of values for diastolic blood pressure in all participants

max(health_clean$BPDia)-min(health_clean$BPDia)

#Variance and standard deviation for income among all participants
var(health_clean$Income)
sd(health_clean$Income)

#Visualizing the relationship between weight and height and coloring the points by gender, diabetes and smoking status

library(ggplot2)
ggplot(data = health_clean, aes(x =Weight, y = Height, color = Gender)) +
  geom_point() +
  labs(title = "Relationship between Weight and Height by Gender",
       x = "Weight",
       y = "Height",
       color = "Gender") +
  theme_minimal()

ggplot(data = health_clean, aes(x =Weight, y = Height, color = Diabetes)) +
  geom_point() +
  labs(title = "Relationship between Weight and Height and Diabetes",
       x = "Weight",
       y = "Height",
       color = "Diabetes") +
  theme_minimal()

ggplot(data = health_clean, aes(x =Weight, y = Height, color = SmokingStatus)) +
  geom_point() +
  labs(title = "Relationship between Weight and Height by SmokingStatus",
       x = "Weight",
       y = "Height",
       color = "SmokingStatus") +
  theme_minimal()

#  T-test between the following variables;Age and Gender,BMI and Diabetes, Alcohol Year and Relationship Status

t.test(Age~Gender, data = health_clean)
t.test(BMI~Diabetes, data = health_clean)
t.test(AlcoholYear~RelationshipStatus, data = health_clean)


