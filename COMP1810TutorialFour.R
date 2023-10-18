data2 <- read.csv("input.csv", sep = "", header = TRUE)
df <- read.table("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt",
                 header = FALSE,
                 sep = ",")
df
df <- read.delim("https://s3.amazonaws.com/blog_assets/test_delim.txt", sep = "$")
df
data()
library(help="datasets")
rm(list=ls())
install.packages("tidyverse")
install.packages("tidyr")
install.packages("tidyverse")
library("tidyverse")
trees%>%glimpse()
trees%>%
  select(Height, Volume)%>%
  summary()
starwars
library("dplyr")
starwars
glimpse(mtcars)
mtcars %>% select(mpg,cyl)
iris %>%
  subset(Sepal.Length > 5)%>%
  aggregate(.~Species,.,mean)
dim(starwars)
install.packages("magrittr")
starwars%>%
  filter(eye_color=="black")
starwars%>% 
  filter(eye_color!="black")
starwars%>%
  filter(eye_color!="black",
  skin_color=="gold",
  height>100)
starwars%>%
  filter(eye_color!="black")%>%
  #filter(skin_color=="gold")%>%
  filter(height>100)
colnames(starwars)
which(colnames(starwars)=="gender")
starwars%>%
  select(2:7)
starwars%>%
  select(height:birth_year)
starwars%>%
  select(7, 2, 3, 5, 6)
starwars%>%
  select(which(colnames(starwars)=="height"):which(colnames(starwars)=="birth_year"))
temp <- starwars%>%
  select(everything())
starwars%>%
  select(height:birth_year, everything())
starwars%>%
  select(ends_with("_color"))
iris%>%
  select(Species, starts_with("Petal"))
iris%>%
  select(starts_with("Petal"), ends_with("Width"))
starwars%>%
  mutate(bmi=mass/(height/100)^2)%>%
  mutate(bmi=round(bmi,2))%>%
  select(name, height, mass, bmi)
starwars%>%
  select(name, mass)%>%
  mutate(
    mass2 = mass*2,
    mass2_squared=mass2 * mass2
  )
starwars%>%
  select(name, homeworld, species)%>%
  mutate(homeworld = as.factor(homeworld),
         species = as.factor(species),
         )
iris%>%
  select(Species, starts_with("Petal"))%>%
  mutate(Petal.Area = Petal.Length * Petal.Width)
iris%>%
  select(Species, starts_with("Petal"))%>%
  mutate(Petal.Area = Petal.Length * Petal.Width)%>%
  filter(Petal.Area > 4 & Petal.Area > 6)%>%
  select(where(~Petal.Area))
starwars%>%
  arrange(height)
starwars%>%
  arrange(desc(height))
mtcars%>% arrange(cyl, am)
mtcars%>%
  group_by(cyl)%>%
  summarise(mean=mean(disp), total=n())
mtcars%>%
  group_by(carb)%>%
  summarise(n=n())
starwars%>%
  group_by(species)%>%
  summarise(count=n(),
  massAvg=mean(mass, na.rm = TRUE))
starwars%>%
  group_by(species)%>%
  summarise(count=n(),
            mass=mean(mass, na.rm = TRUE)
            )%>%
  filter(
    count > 1,
    mass >50
  )
installed.packages("tidyr")
relig_income
pivot_longer(relig_income, -religion, names_to = "income", values_to = "count")
billboard
write.csv(billboard, "billboard.csv", row.names = FALSE)
billboard%>%
  pivot_longer(
    cols=starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )
billboard_longer%>%
  filter(artist == "2 pac")%>%
  View()
billboard_longer%>%
  group_by(track)%>%
  summarise(highest_rank = max(rank))
install.packages("stringr")
library(stringr)
str_length("abc")
x <- c("why", "video", "cross", "extra", "deal", "authority")
str_length(x)
x <- c("abcdef", "ghifjk")
str_sub(x, 2, -2)
str_sub(x, 3, 3)
x <- c("abc", "defghi")
str_pad(x, 10)
str_pad(x, 10, "both")
x <- c("Short", "This is a long string")
x %>%
  str_trunc(10)%>%
  str_pad(10, "right")
billboard%>%
  mutate(week=substr(week, 3,4),
         week=as.integer(week))#error message
rlang::last_trace()
strings <- c(
  "apple",
  "219 733 8965",
  "329-293-8753",
  "work: 579-499-7527; Home: 543.355.3679"
)
phone <- "([2-9][0-9]{2})[- ,]([0-9]{3})[- ,]([0-9]{4})"
str_detect(strings, phone)
str_subset(strings, phone)
x <- c("wh", "vi", "cr", "ex", "de", "au")
str_detect(x,"[:alpha:]")
str_detect(x,"[:space:]")
str_detect("Hwllo World ","[:space:]")
str_detect(x,"a")
fruits <-c("one apple", "two pears", "three bananas")
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", toupper)
str_replace_all(fruits, "b", NA_character_)
str_replace(fruits, c("a", "e", "i"), "-")
fruits <- c("apple", "banana","pear","pineapple")
str_count(fruit, pattern="a")
fruit3 <- c("papaya", "lime", "apple")
str_locate(fruit3, pattern="p")
str_locate_all(fruit3, pattern="p")
