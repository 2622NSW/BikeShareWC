# http://www.storybench.org/exploring-bike-rental-behavior-using-r/
library(tidyverse)
install.packages("rsample")
library(rsample)
install.packages("randomForest")
library(randomForest)
install.packages("ranger")
library(ranger)
library(caret)
library(ggthemes)
install.packages("scales")
library(scales)
install.packages("wesanderson")
library(wesanderson)
install.packages("styler")
library(styler)
df <- readr::read_csv("Day.csv")
df %>%
  mutate(
  weekday_chr =
  case_when(
  weekday == 0 ~ "Sunday",
  weekday == 1 ~ "Monday",
  weekday == 2 ~ "Tuesday",
  weekday == 3 ~ "Wednesday",
  weekday == 4 ~ "Thursday",
  weekday == 5 ~ "Friday",
  weekday == 6 ~ "Saturday",
  TRUE ~ "other")) %>% 
  dplyr::count(weekday, weekday_chr) %>%
  tidyr::spread(weekday, n)
df <- df %>%
  mutate(
    weekday_chr =
      case_when(
        weekday == 0 ~ "Sunday",
        weekday == 1 ~ "Monday",
        weekday == 2 ~ "Tuesday",
        weekday == 3 ~ "Wednesday",
        weekday == 4 ~ "Thursday",
        weekday == 5 ~ "Friday",
        weekday == 6 ~ "Saturday",
        TRUE ~ "other"))
df %>% 
  dplyr::count(weekday, weekday_chr) %>% 
  tidyr::spread(weekday, n)
df %>%
  mutate(
    weekday_fct = factor(x = weekday,
                         levels = c(0,1,2,3,4,5,6),
                         labels = c("Sunday",
                                    "Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday"))) %>%
  dplyr::count(weekday, weekday_fct) %>%
  tidyr::spread(weekday, n)
df <- df %>%
  mutate(
    weekday_fct = factor(x = weekday,
                         levels = c(0,1,2,3,4,5,6),
                         labels = c("Sunday",
                                    "Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday")))
df %>% 
  dplyr::count(weekday, weekday_fct) %>% 
  tidyr::spread(weekday, n)
df %>%
  mutate(holiday_chr =
           case_when(
             holiday == 0 ~ "Non-Holiday",
             holiday == 1 ~ "Holiday")) %>% 
  dplyr::count(holiday, holiday_chr) %>%
  tidyr::spread(holiday, n)
df <- df %>%
  mutate(holiday_chr =
           case_when(
             holiday == 0 ~ "Non-Holiday",
             holiday == 1 ~ "Holiday"))
df %>%
  dplyr::count(holiday_chr, holiday_fct) %>%
  tidyr::spread(holiday_chr, n)
df %>%
  mutate(
    workingday_chr =
      case_when(
        workingday == 0 ~ "Non-Working Day",
        workingday == 1 ~ "Working Day",
        TRUE ~ "other")) %>% 
  dplyr::count(workingday, workingday_chr) %>%
  tidyr::spread(workingday, n)
df <- df %>%
  mutate(
    workingday_fct = factor(x = workingday,
                            levels = c(0,1),
                            labels = c("Non-Working Day",
                                       "Working Day")))
df %>%
  dplyr::count(workingday_chr, workingday_fct) %>%
  tidyr::spread(workingday_chr, n)
df <- df %>%
  mutate(
    season_chr =
      case_when(
        season == 1 ~ "Spring",
        season == 2 ~ "Summer",
        season == 3 ~ "Fall",
        season == 4 ~ "Winter",
        TRUE ~ "other"
      ))
df %>%
  mutate(
    season_fct = factor(x = season,
                        levels = c(1, 2, 3, 4),
                        labels = c("Spring",
                                   "Summer",
                                   "Fall",
                                   "Winter"))) %>%
  dplyr::count(season_chr, season_fct) %>%
  tidyr::spread(season_chr, n)
df <- df %>%
  mutate(
    season_fct = factor(x = season,
                        levels = c(1, 2, 3, 4),
                        labels = c("Spring",
                                   "Summer",
                                   "Fall",
                                   "Winter"))) 
df %>%
  dplyr::count(season_chr, season_fct) %>%
  tidyr::spread(season_chr, n)
df %>%
  mutate(
    weathersit_chr =
      case_when(
        weathersit == 1 ~ "Good",
        weathersit == 2 ~ "Clouds/Mist",
        weathersit == 3 ~ "Rain/Snow/Storm",
        TRUE ~ "other")) %>% 
  dplyr::count(weathersit, weathersit_chr) %>%
  tidyr::spread(weathersit, n)
df <- df %>%
  mutate(
    weathersit_chr =
      case_when(
        weathersit == 1 ~ "Good",
        weathersit == 2 ~ "Clouds/Mist",
        weathersit == 3 ~ "Rain/Snow/Storm"))
df %>% 
  dplyr::count(weathersit, weathersit_chr) %>%
  tidyr::spread(weathersit, n)
df %>%
  mutate(
    weathersit_fct = factor(x = weathersit,
                            levels = c(1, 2, 3),
                            labels = c("Good",
                                       "Clouds/Mist",
                                       "Rain/Snow/Storm"))) %>%
  dplyr::count(weathersit, weathersit_fct) %>%
  tidyr::spread(weathersit, n)
df<- df %>%
  mutate(
    weathersit_fct = factor(x = weathersit,
                            levels = c(1, 2, 3),
                            labels = c("Good",
                                       "Clouds/Mist",
                                       "Rain/Snow/Storm")))
df %>%
  dplyr::count(weathersit_chr, weathersit_fct) %>%
  tidyr::spread(weathersit_chr, n)
df %>% 
  mutate(month_ord = 
           lubridate::month(mnth, label = TRUE)) %>% 
  dplyr::count(month_ord, mnth) %>% 
  tidyr::spread(month_ord, n)
df <- df %>% 
  mutate(month_ord = 
           lubridate::month(mnth, label = TRUE))
df %>% 
  dplyr::count(month_ord, mnth) %>% 
  tidyr::spread(month_ord, n)
df <- df %>% 
  mutate(month_ord = 
           lubridate::month(mnth, label = TRUE))
df %>% 
  dplyr::count(month_ord, mnth) %>% 
  tidyr::spread(month_ord, n)
df %>%
  mutate(
    month_fct = factor(x = mnth,
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       labels = c("January", "February", "March", "April", "May",
                                  "June", "July", "August", "September", "October",
                                  "November", "December"))) %>%
  dplyr::count(mnth, month_fct) %>%
  tidyr::spread(month_fct, n)
df <- df %>%
  mutate(
    month_fct = factor(x = mnth,
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       labels = c("January", "February", "March", "April", "May",
                                  "June", "July", "August", "September", "October",
                                  "November", "December")))
df %>% 
  dplyr::count(month_chr, month_fct) %>%
  tidyr::spread(month_fct, n)
df %>%
  mutate(
    yr_chr =
      case_when(
        yr == 0 ~ "2011",
        yr == 1 ~ "2012",
        TRUE ~ "other")) %>% 
  dplyr::count(yr, yr_chr) %>%
  tidyr::spread(yr, n)
df <- df %>%
  mutate(
    yr_chr =
      case_when(
        yr == 0 ~ "2011",
        yr == 1 ~ "2012"))
df %>%
  dplyr::count(yr, yr_chr) %>%
  tidyr::spread(yr, n)
df %>%
  mutate(
    yr_fct = factor(x = yr,
                    levels = c(0, 1),
                    labels = c("2011",
                               "2012"))) %>%
  dplyr::count(yr, yr_fct) %>%
  tidyr::spread(yr, n)
df <- df %>%
  mutate(
    yr_fct = factor(x = yr,
                    levels = c(0, 1),
                    labels = c("2011",
                               "2012")))
df %>%
  dplyr::count(yr_chr, yr_fct) %>%
  tidyr::spread(yr_chr, n)
df <- df %>%
  mutate(temp = as.integer(temp * (39 - (-8)) + (-8)))
df <- df %>%
  mutate(atemp = atemp * (50 - (16)) + (16))
df <- df %>%
  mutate(windspeed = as.integer(67 * df$windspeed))
df <- df %>%
  mutate(hum = as.integer(100 * df$hum))
df <- df %>%
  mutate(dteday = as.Date(dteday))
df %>% dplyr::glimpse(78)
df1 <- df
df1 <- df1 %>% 
  dplyr::select(
    dplyr::starts_with("week"),
    dplyr::starts_with("holi"),
    dplyr::starts_with("seas"),
    dplyr::starts_with("work"),
    dplyr::starts_with("month"),
    dplyr::starts_with("yr"),
    dplyr::starts_with("weath"),
    dplyr::everything())
dplyr::glimpse(df1)
BikeDplyrSummary <- df1 %>%
  select(temp, atemp, hum, windspeed, casual, registered, cnt) %>%
  summarise_each(list(
    min = ~min,
    q25 = ~quantile(., 0.25),
    median = ~median,
    q75 = ~quantile(., 0.75),
    max = ~max,
    mean = ~mean,
    sd = ~sd
  )) %>%
  gather(stat, val) %>%
  separate(stat, 
           into = c("var", "stat"), 
           sep = "_") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd)
knitr::kable(BikeDplyrSummary)
BikeSkimrSummary <- df %>%
  skimr::skim_to_wide() %>%
  dplyr::select(type,
                variable,
                missing,
                complete,
                min,
                max,
                mean,
                sd,
                median = p50,
                hist)
knitr::kable(BikeSkimrSummary)
ggRentalsByTemp <- df1 %>% 
  ggplot(aes(y = cnt, 
             x = temp, 
             color = weekday_fct)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(se = FALSE,
              show.legend = FALSE) +
  facet_grid(~weekday_fct) +
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Bike Rentals") +
  xlab("Temperature (°C)") +
  ggtitle("Bike Rental Volume By Temperature")
ggRentalsByTemp                                       
ggRentalVolByWindSpeed <- ggplot(df) +
  geom_point(aes(y = cnt, 
                 x = windspeed, 
                 color = weekday_fct),
             show.legend = FALSE) +
  facet_grid(~weekday_fct) +
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Bike Rentals") +
  xlab("Windspeed") +
  ggtitle("Rental Volume By Windspeed")
ggRentalVolByWindSpeed
ggRentalVolByHoliday <- ggplot(df1) +
  geom_density(aes(x = cnt,
                   fill = holiday_chr), 
               alpha = 0.2) +
  scale_fill_brewer(palette = "Paired") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(title = "Bike Rental Density By Holiday",
       fill = "Holiday",
       x = "Average Bike Rentals",
       y = "Density")
ggRentalVolByHoliday

  