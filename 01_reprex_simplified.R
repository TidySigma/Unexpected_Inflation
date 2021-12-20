library(tidyverse)
suppressPackageStartupMessages(library(tidyquant))
library(timetk)
library(scales)

# sources:
  ### My response to a post by Jonathan Regenstein
  ### original github code: https://gist.github.com/jkr216/0c6463e99dc54f323ed4976392ce3c57
  ### twitter post: https://twitter.com/jkregenstein/status/1467962068863275011?s=20



# Section 1: Simplified Reproduction of jkr216's github gist ----

# 5-Year Breakeven Inflation Rate (T5YIE)
T5YIE <- tq_get("T5YIE", get= "economic.data", from = "2003-01-01") %>%
  mutate(Percent = price/100) %>%
  summarise_by_time(
    .date_var = date,
    .by = "month",
    expected_five_year = mean(Percent, na.rm = T)
  )
head(T5YIE)

# Consumer Price Index for All Urban Consumers: All Items in U.S. City Average
CPIAUCSL <- tq_get("CPIAUCSL", get= "economic.data", from = "1998-01-01") %>%
  select(date, cpi = price) %>%
  mutate(
    cpi_5_year_change = cpi/lag(cpi, 60) - 1,
    cpi_five_year_avg = cpi_5_year_change/5
  ) %>%
  filter(date >= "2003-01-01")
head(CPIAUCSL)

# join the two data sets
df <- left_join(T5YIE, CPIAUCSL, by = "date") %>%
  select(date, expected_five_year, cpi_five_year_avg) %>%
  mutate(
    actual_five_year = lead(cpi_five_year_avg, 60),
    unexpected_five_year = actual_five_year - expected_five_year) %>%
  drop_na() %>%
  select(-contains("cpi")) %>%
  pivot_longer(-date) %>%
  mutate(name = str_replace(name, "_five_year", " inflation") %>% str_to_title())
head(df)

# create chart
chart <- df %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_y_continuous(labels = percent, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y") +
  theme_minimal() +
  labs(x = "", y = "", color = "",
       title = "Actual, Expected and Unexpected Inflation: Next 5 Years",
       caption = "Source: Fred data. Based on CPI and 5-Year Breakeven Rates") +
  theme(legend.position = "top",
        plot.title = element_text(hjust = .5))
chart

# End of Section 1-----



# end
