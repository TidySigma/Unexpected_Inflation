# My approach to visualize the inflation data
library(tidyverse)
suppressPackageStartupMessages(library(tidyquant))
library(timetk)
library(scales)
library(plotly)
library(ggrepel)

## functions -------
# trailing 3 months avg
T3M_avg <- function(x){
  (x + lag(x,1) + lag(x,2))/3
}
# Change vs 12 months ago
YY <- function(x){
  (x - lag(x,12))/lag(x,12)
}
## functions -------


# Section 1: get & prepare data

# T5YIE: 5-Year Breakeven Inflation Rate
T5YIE_monthly <- tq_get("T5YIE", get= "economic.data", from = "2018-01-01") %>%
  mutate(Percent = price/100) %>%
  summarise_by_time(
    .date_var = date,
    .by = "month",
    expected_five_year = mean(Percent, na.rm = T)) %>%
  rename(`Expected Inflation` = expected_five_year)
head(T5YIE_monthly)

T5YIE_chart <- T5YIE_monthly %>%
  ggplot(aes(x=date, y=`Expected Inflation`)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%Y-%b") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
T5YIE_chart

T5YIE_plotly <- ggplotly(T5YIE_chart)
T5YIE_plotly

# CPI Consumer Price Index for All Urban Consumers: All Items in U.S. City Average
# I am using year-over-year change of trailing 3 month average as my measure for actual inflation
CPIAUCSL_3m <- tq_get("CPIAUCSL", get= "economic.data", from = "2018-01-01") %>%
  summarise_by_time(
    .date_var = date,
    .by = "month",
    CPI_allitems_monthly = mean(price, na.rm = T)) %>%
  mutate(CPI_T3M_avg = T3M_avg(CPI_allitems_monthly)) %>%
  mutate(CPI_YY_T3M_avg = YY(CPI_T3M_avg)) %>%
  rename(`Actual Inflation` = CPI_YY_T3M_avg) %>%
  na.omit()
head(CPIAUCSL_3m)

CPIAUCSL_chart <- CPIAUCSL_3m %>%
  ggplot(aes(x=date, y=`Actual Inflation`)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent, breaks = pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%Y-%b") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
CPIAUCSL_chart

CPIAUCSL_plotly <- ggplotly(CPIAUCSL_chart)
CPIAUCSL_plotly

# combine data

df_1 <- left_join(T5YIE_monthly, CPIAUCSL_3m, by="date") %>%
  select(date, `Expected Inflation`, `Actual Inflation`) %>%
  mutate(`Unexpected Inflation` =  `Actual Inflation` - `Expected Inflation`) %>%
  filter(date > "2019-03-01") %>%
  mutate(across(`Expected Inflation`:`Unexpected Inflation`, ~round(.x,3)))
head(df_1)
tail(df_1)

# nrow(df_1)

# Section 2: Visualize

chart_plotly_1 <- df_1 %>%
  plot_ly(x=~date, y=~`Actual Inflation`, type='scatter', mode='lines+markers', name = "Actual") %>%
  add_trace(x=~date, y=~`Expected Inflation`, type='scatter', mode='lines+markers', name = "Expected") %>%
  add_trace(x=~date, y=~`Unexpected Inflation`, type = 'bar', name = "Unexpected",
            opacity=0.25) %>%
  layout(title = 'Inflation: Actual, Expected & Unexpected', plot_bgcolor = "#e5ecf6",
         yaxis = list(title = 'Inflation Percent (%)', tickformat = ".1%"),
         xaxis = list(title = 'Month', type = 'date',
                      tickformat = "%b-%Y", tickangle=-45,
                      range = list("2019-03-01", "2022-01-01"),
                      showgrid = T, dtick="M1"))
chart_plotly_1

