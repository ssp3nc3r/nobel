# load data ----
# https://www.kaggle.com/nobelfoundation/nobel-laureates/version/1
d <- read.csv("winners.csv", stringsAsFactors = F, encoding = "UTF-8")

# load libraries ----
library(dplyr)
library(ggplot2); library(ggthemes)

# clean and transform data ----
cats <- c("Chemistry", "Economics", "Literature", 
          "Medicine", "Peace", "Physics")

d <- d %>% 
  mutate(Category = factor(Category, 
                           levels = cats, 
                           ordered = T)) %>% 
  mutate(Sex = factor(Sex)) %>% 
  mutate(Birth.Year = as.integer(substring(Birth.Date, 1, 4))) %>% 
  mutate(Age = Year - Birth.Year)

davg <- d %>% 
  group_by(Category) %>% 
  summarise(avg_age = mean(Age, na.rm = T)) %>% 
  ungroup()

# main display of age versus year awarded ----
# grab hex colors from original
cols <- c("#cc5b47", "#488595", "#96c17c", 
          "#decd7c", "#924855", "#e79275")

# plot
ggplot(d, aes(color = Category)) + 
  theme_minimal(base_family = "sans") +
  geom_hline(yintercept = mean(d$Age, na.rm = T), 
             lwd = .2, color = "black", linetype = "dashed") +
  geom_hline(data=davg, mapping = aes(yintercept=(avg_age),color = Category)) +
  geom_line(aes(Year, Age, color = Category), lwd = .2) +
  geom_point(aes(Year, Age, color = Category), size = 1.5, alpha = .5) +
  geom_point(data = filter(d, Sex == "Female"), 
             aes(Year, Age), 
             color = "#da87bd", shape = 21, size = 4) +
  facet_wrap(Category ~ ., nrow=6, strip.position="left") +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = c(1901, 1931, 1961, 1991, 2016),
                     minor_breaks = seq(1911, 2016, by = 10),
                     position = "top") +
  theme(legend.position = "") +
  labs(y = "", x = "")

# bar charts at end ----
# need to get grade level data ... wikipedia:
# https://en.wikipedia.org/wiki/List_of_Nobel_laureates_by_university_affiliation#Other_universities_(51st–)

# estimated numbers from reading original visual display as placeholder until
# find actual data
dedu <- read.table(text = "
Category Doctor Master Bachelor None
Chemistry 98 1 1 0
Economics 95 1 4 0
Literature 20 20 25 35
Medicine 95 4 1 0
Peace 32 25 20 23
Physics 99.1 .3 .3 .3
           ", header = T)

dedu <- reshape2::melt(dedu, 
                       variable.name = "Education", 
                       value.name = "Percent")
dedu <- dedu %>% 
  mutate(Category = factor(Category, 
                           levels = cats, 
                           ordered = T),
         Education = factor(Education, 
                            levels = c("None", "Bachelor", "Master", "Doctor"), 
                            ordered = T))

ggplot(dedu) + 
  facet_wrap(~Category, ncol = 1) +
  geom_bar(aes(Education, Percent, fill = Category), stat = "identity") + 
  coord_flip() +
  scale_fill_manual(values = cols) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank())


# bottom stacked bar of birth city ----
cities <- c("Chicago, IL", "Washington, DC", "New York, NY", "Boston, MA", 
            "London", "Paris", "Munich", "Berlin", "Vienna", "Budapest", "Moscow")

# aggregate totals by era, birth city, and category of prize
d3 <- d %>% 
  filter(Birth.City %in% cities) %>%
  mutate(Birth.City = factor(Birth.City, 
                             levels = cities[11:1], ordered = T)) %>%
  mutate(era = case_when(Year < 1931 ~ 1901,
                         Year < 1961 ~ 1931,
                         Year < 1991 ~ 1961,
                         TRUE ~ 1991)) %>%
  group_by(era, Birth.City, Category) %>%
  summarise(n = n()) %>% 
  ungroup()

# aggregate totals by era and birth city
d4 <- d3 %>%
  group_by(era, Birth.City) %>%
  summarise(Total = sum(n)) %>%
  ungroup()

# plot 
ggplot(d3) + 
  facet_wrap(era~., nrow = 1) +
  geom_bar(aes(Birth.City, n, fill = Category), 
           stat = 'identity', width = 0.2) + 
  geom_text(data = d4,
            mapping = aes(Birth.City, Total + 2, label = Total),
            size = 2.5) +
  coord_flip() +
  scale_fill_manual(values = cols) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank())


# ribbon chart on far right ----
universities <- c("Harvard", "MIT", "Stanford", "Caltech", 
                  "Columbia", "Cambridge", "Berkeley")

# clean data for universities
# note: a bit messy because original data had non utf-8 encoding
d5 <- d %>%
  mutate(Organization.Name = ifelse(grepl("Berkeley", Organization.City, perl = T),
                             "Berkeley", Organization.Name)) %>%
  mutate(University = NA) %>%
  mutate(University = ifelse(grepl("Harvard", Organization.Name, perl = T, useBytes = T),
                             "Harvard", University)) %>%
  mutate(University = ifelse(grepl("MIT", Organization.Name, perl = T, useBytes = T),
                             "MIT", University)) %>%
  mutate(University = ifelse(grepl("Stanford", Organization.Name, perl = T, useBytes = T),
                             "Stanford", University)) %>%
  mutate(University = ifelse(grepl("Caltech", Organization.Name, perl = T, useBytes = T),
                             "Caltech", University)) %>%
  mutate(University = ifelse(grepl("Columbia", Organization.Name, perl = T, useBytes = T),
                             "Columbia", University)) %>%
  mutate(University = ifelse(grepl("Cambridge", Organization.Name, perl = T, useBytes = T),
                             "Cambridge", University)) %>%
  mutate(University = ifelse(grepl("Berkeley", Organization.Name, perl = T, useBytes = T),
                             "Berkeley", University))

# aggregate prizes for universities per category
d5 <- d5 %>% 
  filter(University %in% universities) %>%
  group_by(Category, University, .drop = F) %>% 
  summarise(n = n())

# setup data for plot
library(ggforce)
data <- gather_set_data(d5, 1:2)

# hack to add litrature back in (there are zero 
# literature prize winners at schools of interest
data <- data %>% 
  mutate(University = ifelse(Category == "Literature", "Harvard", University)) %>%
  mutate(y = ifelse(Category == "Literature" & x == "University", "Harvard", y)) %>%
  mutate(y = ifelse(Category == "Literature" & x == "Category", "Literature", y))

# plot
ggplot(data, aes(x, id=id, split = y, value = n)) +
  geom_parallel_sets(aes(fill = Category), alpha = 0.6, axis.width = 0.05, sep = .1) +
  geom_parallel_sets_axes(axis.width = 0.01, fill = "gray80", sep = .1) + 
  geom_parallel_sets_labels(size = 2, angle = 0, 
                            position = position_nudge(x = c(rep(-.1, 6), rep(.1, 7)) ),
                            sep = .1) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "")



