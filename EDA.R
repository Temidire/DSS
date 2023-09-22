#DATA STRUCTURE


str(draft)
head(draft)
summary(draft)
describe(draft)

#Barplot
barplot(table(draft$Genre))
barplot(table(draft$Age_Rating))
barplot(table(draft$Language))
barplot(table(draft$Year))

#No of Cardinality
n_distinct(draft$Language)
n_distinct(draft$Genre)
n_distinct(draft$Directors)

#Histogram
hist(draft$IMDb.Rating)
hist(draft$Metascore)
hist(draft$Runtime..mins., main = "Runtime Distribution", xlab = "Runtime (minutes)")

#Boxplot
boxplot(draft$Num.Votes, main = "Number of Votes", ylab = "Votes")
boxplot(draft$Num.Votes)

#Relationship between numerical variables
plot(draft$Total.Views, draft$Budget...., main = "Scatter Plot: Total Views vs. Budget", 
     xlab = "Total Views", ylab = "Budget", pch = 16, col = "blue")

plot(draft$IMDb.Rating, draft$Metascore, main = "Scatter Plot: IMDb_Rating vs. Metascore", 
     xlab = "IMDb_Rating)", ylab = "Metascore", pch = 16, col = "blue")

plot(draft$Budget_Normalised, draft$Target, main = "Scatter Plot: Budget vs. Target", 
     xlab = "Budget", ylab = "Target", pch = 16, col = "blue")

plot(draft$Month, draft$Total.Views, main = "Scatter Plot: Month vs. Target", 
     xlab = "Month)", ylab = "Target", pch = 16, col = "blue")

plot(draft$Genre_enc, draft$Target, main = "Scatter Plot: Genre vs. Target", 
     xlab = "Genre)", ylab = "Target", pch = 16, col = "blue")


# Create a bar plot of the number of movies for each year
ggplot(draft, aes(x = Year)) +
  geom_bar() +
  labs(x = "Year", y = "Number of Movies", title = "Number of Movies for Each Year")

# Create a bar plot of the number of movies for each month
ggplot(draft, aes(x = Month)) +
  geom_bar() +
  labs(x = "Month", y = "Number of Movies", title = "Number of Movies for Each Month")








