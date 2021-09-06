## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(yowie)
library(ggplot2)
library(forcats)
library(janitor)
library(kableExtra)
library(dplyr)

## -----------------------------------------------------------------------------
# subset demog_nlsy79 who only included in wages
wages_hs_demog <- filter(demog_nlsy79, id %in% wages$id)

# create cross tabulations from demographic data  
gender_race_table <- wages_hs_demog %>%
  tabyl(gender, race) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(position = "front") %>%
  mutate(gender = ifelse(gender == "MALE", "Male",
                         ifelse(gender == "FEMALE", "Female", "Total")))

# styling the table 
kable(gender_race_table,
      caption = "Gender and Race Distribution of the NLSY79 Samples",
      col.names = c("Gender", "Hispanic", "Black", "Non-Black, Non-Hispanic", "Total")) %>%
  kable_styling(latex_options = "striped") %>%
  add_header_above(c(" " = 1, "Race" = 3, " " = 1))

## -----------------------------------------------------------------------------
# count the number of rounds that every id taken
wages_hs_df <- as_tibble(wages) %>%
  group_by(id) %>%
  count()

# create the bar plot 
wages_hs_df %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  xlab("number of rounds") +
  ggtitle("Distribution of survey taken by the cohort")

## ---- message=FALSE-----------------------------------------------------------
ggplot(wages, aes(x = year, y = log(wage), color = gender)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "black") +
  facet_wrap(~gender) +
  scale_color_manual(values = c("#04B4AE", "#BF00FF")) +
  theme(legend.position = "none") +
  ggtitle("Wages based on gender, overlaid by GAM smoother") +
  ylab("log of mean hourly wage")

## ---- message=FALSE-----------------------------------------------------------
ggplot(wages, aes(x = year, y = log(wage), color = race)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "black", se = TRUE) +
  facet_wrap(~race) +
  scale_color_manual(values = c("#FACC2E", "#04B45F", "#58ACFA")) +
  theme(legend.position = "none") +
  ggtitle("Wages based on race, overlaid by GAM smoother") +
  ylab("log of mean hourly wage")

## ---- message = FALSE---------------------------------------------------------
# collapse the hgc into new levels
wages_hgc_new <- wages %>%
  mutate(hgc_regroup = fct_collapse(hgc, `ungraded` = "UNGRADED",
                                    `8TH grade or less` = c("1ST GRADE", "3RD GRADE",
                                                                 "4TH GRADE", "5TH GRADE",
                                                                 "6TH GRADE", "7TH GRADE",
                                                                 "8TH GRADE"),
                                    `9TH to 11TH grade` = c("9TH GRADE",
                                                            "10TH GRADE",
                                                            "11TH GRADE"),
                                    `12TH grade` = "12TH GRADE")) %>%
  filter(hgc_regroup != "ungraded") %>%
  mutate(hgc_regroup = factor(hgc_regroup, levels = c("ungraded", "8TH grade or less", "9TH to 11TH grade", "12TH grade")))

# create the plot of wages vs year facet by education
ggplot(wages_hgc_new, aes(x = year, y = log(wage), color = hgc_regroup)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "black", se = TRUE) +
  scale_colour_brewer("", palette="Dark2") +
  facet_wrap(~hgc_regroup) +
  theme(legend.position = "none") +
  ggtitle("Wages based on education, overlaid by GAM smoother") +
  ylab("log of mean hourly wage")

