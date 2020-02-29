library(dplyr)
library(tibble)
library(ggplot2)

extract <- function (filename) {
  timestamp <- strsplit(filename, '.csv')[[1]]

  raw <- read.csv(paste(data.directory, filename, sep = ''))
  raw <- raw %>% mutate(date = as.Date(timestamp, '%m-%d-%Y'))
  return(raw)
}

dat <- tibble()
data.directory <- './COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/'
files <- list.files(data.directory)

for (name in files) {
  if (name != 'README.md') {
    dat <- rbind(dat, extract(name))
  }
}

dat[is.na(dat)] <- 0

# Confirmed Cases

total.confirmed.cases <- dat %>%
  group_by(date) %>%
  summarise(Confirmed = sum(Confirmed)) %>%
  ggplot() +
  aes(x = date, y = Confirmed) +
  geom_col() +
  theme_minimal() +
  labs(y = "Total Confirmed Cases", x = "Date", title = "Worldwide Total Confirmed Cases")

newly.confirmed.cases <- dat %>%
  group_by(date) %>%
  summarise(Confirmed = sum(Confirmed)) %>%
  mutate(Confirmed = Confirmed - lag(Confirmed)) %>%
  ggplot() +
  aes(x = date, y = Confirmed) +
  geom_col() +
  theme_minimal() +
  labs(y = "Newly Confirmed Cases", x = "Date", title = "Worldwide Newly Confirmed Cases")

# Deaths

total.deaths <- dat %>%
  group_by(date) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ggplot() +
  aes(x = date, y = Deaths) +
  geom_col() +
  theme_minimal() +
  labs(y = "Total Confirmed Cases", x = "Date", title = "Worldwide Total Deaths")


new.deaths <- dat %>%
  group_by(date) %>%
  summarise(Deaths = sum(Deaths)) %>%
  mutate(Deaths = Deaths - lag(Deaths)) %>%
  ggplot() +
  aes(x = date, y = Deaths) +
  geom_col() +
  theme_minimal() +
  labs(y = "Newly Confirmed Cases", x = "Date", title = "Worldwide New Deaths")

arranged <- gridExtra::grid.arrange(total.confirmed.cases, newly.confirmed.cases, total.deaths, new.deaths, ncol = 2)
ggsave('new-and-total-cases.png', arranged, width = 8, height = 5)

# BuT iTs AlL iN ChInA

dat %>%
  mutate(china = ifelse(Country.Region == 'Mainland China', 'China', 'Not China')) %>%
  ggplot() +
  aes(x = date, y = Confirmed, fill = china) +
  geom_col() +
  theme_minimal() +
  labs(y = "Total Confirmed Cases", x = "Date", title = "Worldwide Total Confirmed Cases, outside of China")

outside.china <- dat %>%
  filter(Country.Region != 'Mainland China') %>%
  ggplot() +
  aes(x = date, y = Confirmed) +
  geom_col() +
  theme_minimal() +
  labs(y = "Total Confirmed Cases", x = "Date", title = "Worldwide Total Confirmed Cases, outside of China")
