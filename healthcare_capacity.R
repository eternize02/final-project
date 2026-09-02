library(here)
library(tidyverse)
library(gtsummary)
library(dplyr)
library(here)

mmsa_icu_beds <- read_csv(here::here( "mmsa-icu-beds.csv"))
ny_mmsas <- mmsa_icu_beds %>%
  filter(str_detect(MMSA, "NY"))

tbl_summary(ny_mmsas,
            by = MMSA,
            include = c(hospitals,high_risk_per_hospital, total_at_risk),
            missing = "no",
            type = list(hospitals ~ "continuous",
                        high_risk_per_hospital ~ "continuous",
                        total_at_risk ~ "continuous"),
            statistic = all_continuous() ~ "{mean}",
            label = list(high_risk_per_hospital ~ "High risk individuals per hospital",
                         hospitals ~ "Hospitals in area",
                         total_at_risk ~ "Total number of high risk individuals")) |>
  add_overall(col_label = "**Mean**") |>
  bold_labels() |>
  remove_footnote_header() |>
  modify_header(label = "**Variable**") |>
  modify_caption("Healthcare Capacity across metropolitain cities in New York") 

table1 <- tbl_summary(ny_mmsas,
                      by = MMSA,
                      include = c(hospitals,high_risk_per_hospital, total_at_risk),
                      missing = "no",
                      type = list(hospitals ~ "continuous",
                                  high_risk_per_hospital ~ "continuous",
                                  total_at_risk ~ "continuous"),
                      statistic = all_continuous() ~ "{mean}",
                      label = list(high_risk_per_hospital ~ "High risk individuals per hospital",
                                   hospitals ~ "Hospitals in area",
                                   total_at_risk ~ "Total number of high risk individuals")) |>
  add_overall(col_label = "**Mean**") |>
  bold_labels() |>
  remove_footnote_header() |>
  modify_header(label = "**Variable**") |>
  modify_caption("Healthcare Capacity across metropolitain cities in New York") 

table1$table_body

linear <- lm(hospitals ~  total_at_risk, data = ny_mmsas)
tbl_regression(linear,intercept = TRUE,
               label = list(total_at_risk ~ "Total high risk individuals"))

library(ggplot2) 
library(tidyverse) 
ggplot(mmsa_icu_beds, aes(x = total_at_risk, y = hospitals)) +   
  geom_point(na.rm = TRUE, color = "royalblue3") +   
  labs(     title = "Hospitals vs. Total High-Risk Individuals",     
            x = "Total high-risk individuals",     
            y = "Number of hospitals")

#making new variable to then apply my function to find ratio per 100,000 people#
ny_mmsas$hospitals_per_risk_individual <- (ny_mmsas$hospitals/ ny_mmsas$total_at_risk)

hospital_ratio <- function (x)
{n <- (x*100000)
result <- (n)
return(result)
}
x<-ny_mmsas$hospitals_per_risk_individual
hospital_ratio(x)
ny_mmsas$ratio <- hospital_ratio(x)

#view and compare to verify function is correct
ny_mmsas$hospitals_per_100k_at_risk <- 
  (ny_mmsas$hospitals / ny_mmsas$total_at_risk) * 100000
view (ny_mmsas[, c("hospitals_per_100k_at_risk", "ratio")])


ggplot(ny_mmsas, aes(x =MMSA, y = ratio)) +
  geom_bar(stat = "identity", fill = "royalblue1") +
  labs(title = "Hospital Ratio for every 100k high risk individuals", x = "Metropolitain Area", y = "Ratio") +
  theme_minimal()


##making statustic table
ny_ratio_table <- (ny_mmsas[, c("MMSA", "ratio")])
view(ny_ratio_table)
statistic <-list(
  mean=round(mean(ny_ratio_table$ratio), digits = 2),
  max=round(max(ny_ratio_table$ratio), digits = 2))
