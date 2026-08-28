install.packages("tidyverse")
install.packages("gtsummary")
install.packages("dplyr")
library(tidyverse)
library(gtsummary)
library(dplyr)


options(repos = c(CRAN = "https://cran.rstudio.com"))
install.packages(c("gtsummary", "here"))

#convert character to numerical#
unique(mmsa_icu_beds$total_percent_at_risk)
view(mmsa_icu_beds)
mmsa_icu_beds <- icu_beds %>%
  mutate(total_percent_at_risk = readr::parse_number(total_percent_at_risk))
str(icu_beds$total_percent_at_risk)
drop_na(high_risk_per_hospital, total_at_risk)

ny_mmsas <- mmsa_icu_beds %>%
  filter(str_detect(MMSA, "NY"))


#tablesummary#
tbl_summary(mmsa_icu_beds,
            by = MMSA,
            include = c(icu_beds, high_risk_per_ICU_bed, hospitals,
                        high_risk_per_hospital, total_at_risk, total_percent_at_risk),
            missing = "no",
            statistic = all_continuous() ~ "{mean}",
            label = list(total_percent_at_risk ~ "Individuals at high risk (%)",
                         high_risk_per_ICU_bed ~ "High risk individuals per ICU bed",
                         high_risk_per_hospital ~ "High risk individuals per hospital",
                         icu_beds ~ "ICU beds in area",
                         hospitals ~ "Hospitals in area",
                         total_at_risk ~ "Total number of high risk individuals")) |>
  add_overall(
    col_label = "**Total**"
  ) |>
  bold_labels() |>
  remove_footnote_header() |>
  modify_header(
    label = "**Variable**"
  ) |>
  modify_caption("MMSA characteristics and healthcare capacity"
  )

#regression; What is the association between the number of high-risk individuals and the number of hospitals across MMSAs?
linear <- lm(hospitals ~  total_at_risk, data = mmsa_icu_beds)
tbl_regression(linear,intercept = TRUE,
               label = list(total_at_risk ~ "Total high risk individuals"))

#load scatterplot package
library(ggplot2)
library(tidyverse)
ggplot(mmsa_icu_beds, aes(x = total_at_risk, y = hospitals)) +
  geom_point() +
  labs(
    title = "Hospitals vs. Total High-Risk Individuals",
    x = "Total high-risk individuals",
    y = "Number of hospitals")

#making a new variable#
mmsa_icu_beds$hospitals_per_10k_at_risk <- 
  (mmsa_icu_beds$hospitals / mmsa_icu_beds$total_at_risk) / 10000