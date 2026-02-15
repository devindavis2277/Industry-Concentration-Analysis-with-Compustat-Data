library(dplyr)
library(haven)
library(ggplot2)
library(caret)
library(pROC)
library(randomForest)
library(statar)
library(scales) 

## Read in data 
cmpst <- read_sas("~/Desktop/compst7023.sas7bdat")
cmpst <- as.data.frame(cmpst)

## Replace some negative values with missing 
cmpst <- cmpst %>%
  mutate(
    PERMNO = permno,
    CA = replace(CA, CA <= 0, NA),
    INTAN = replace(INTAN, INTAN <= 0, NA),
    INVT = replace(INVT, INVT <= 0, NA),
    SLS = replace(SLS, SLS <= 0, NA),
    CASH = replace(CASH, CASH <= 0, NA),
    INTXP = replace(INTXP, INTXP <= 0, NA),
    DIV = replace(DIV, DIV <= 0, NA),
    RD = replace(RD, RD <= 0, 0),
    CAPEX = replace(CAPEX, CAPEX <= 0, NA),
    BVA = replace(BVA, BVA <= 0, NA),
    BVE = replace(BVE, BVE <= 0, NA),
    TLIAB = replace(TLIAB, TLIAB <= 0, NA)
  )

## Check balance sheet consistency and construct ratios
cmpst <- cmpst %>%
  arrange(PERMNO, year) %>%
  mutate(
    CHECK = BVA - TLIAB - BVE
  ) %>%
  filter(CHECK > -0.1 & CHECK < 0.1) %>%
  mutate(
    ROA = NI / BVA,
    RD = if_else(is.na(RD), 0, RD),
    RDA = RD / BVA,
    RDIND = sign(RD),
    SIC2CHAR = substr(SIC, 1, 2),
    SIC2 = as.numeric(SIC2CHAR)
  ) %>%
  select(PERMNO, cname, year, SIC, SIC2, FYR, ROA, NI, BVA, SLS, RD, RDA, RDIND)

## Winsorize ratios to limit outliers
cmpst <- cmpst %>%
  mutate(
    ROA = winsorize(ROA, probs = c(0.01, 0.99)),
    RDA = winsorize(RDA, probs = c(0.01, 0.99))
  )

head(cmpst)
summary(cmpst)

## Calculate Industry Concentration (IC)
IC_df <- cmpst2 %>%
  filter(!is.na(SIC2) & !is.na(year)) %>%
  mutate(SLS_num = as.numeric(SLS)) %>%
  filter(!is.na(SLS_num) & SLS_num > 0) %>%
  group_by(SIC2, year) %>%
  arrange(desc(SLS_num)) %>%
  mutate(rank_in_ind = row_number()) %>%
  summarise(
    n_firms = n(),
    total_sales = sum(SLS_num, na.rm = TRUE),
    top5_sales = sum(ifelse(rank_in_ind <= 5, SLS_num, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IC = case_when(
      n_firms < 5 ~ 1,             # industries with <5 firms are fully concentrated
      total_sales == 0 ~ NA_real_,
      TRUE ~ top5_sales / total_sales
    )
  ) %>%
  rename(YEAR = year) %>%
  arrange(SIC2, YEAR)

head(IC_df)


## Average IC per year (A)
avg_IC_by_year <- IC_df %>%
  group_by(YEAR) %>%
  summarise(
    mean_IC = mean(IC, na.rm = TRUE),
    median_IC = median(IC, na.rm = TRUE),
    n_industries = n(),
    .groups = "drop"
  )

## Plot the average IC trend over time
ggplot(avg_IC_by_year, aes(x = YEAR, y = mean_IC)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "darkred") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Average Industry Concentration (IC) by Year",
    subtitle = "IC = (Top-5 sales) / (Total industry sales) — SIC2 level",
    x = "Year",
    y = "Average IC (percent)",
    caption = "Industries with fewer than 5 firms set to IC = 1"
  ) +
  theme_minimal()

print(avg_IC_by_year)


## Identify 3 Most Competitive Industries (B)
latest_year <- max(IC_df$YEAR, na.rm = TRUE)
first_of_last10 <- latest_year - 9

IC_last10 <- IC_df %>%
  filter(YEAR >= first_of_last10 & !is.na(IC))

avg_IC_last10 <- IC_last10 %>%
  group_by(SIC2) %>%
  summarise(
    mean_IC_last10 = mean(IC, na.rm = TRUE),
    median_IC_last10 = median(IC, na.rm = TRUE),
    years_observed = n(),
    avg_nfirms = mean(n_firms, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_IC_last10)

top3_competitive <- head(avg_IC_last10, 3)
print(top3_competitive)

