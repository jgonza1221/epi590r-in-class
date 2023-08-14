#Exercises Here package

#install.packages("here")
#here::here()
#getwd()
#setwd("data")




library(tidyverse)

#My code worked
# column names
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

# read in raw data, replacing missing values with NA
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
				 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
				 skip = 1, col_names = nlsy_cols)

# create factors for categorical variables
nlsy_cats <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

# check to make sure coding is correct
count(nlsy_cats, eyesight, eyesight_cat)
count(nlsy_cats, region_cat)

# remove observations with any missing data
nlsy_cc <- na.omit(nlsy_cats)

# check to make sure it worked
count(nlsy_cc, eyesight_cat)

# create data/clean folder if it doesn't already exist
if (!dir.exists(here::here("data", "clean"))) {
	dir.create(here::here("data", "clean"))
}

# save the complete-case data
write_rds(nlsy_cc, here::here("data", "clean", "nlsy-complete-cases.rds"))


library(gtsummary)

#gtsummary::tbl_summary()

tbl_summary(
	nlsy_cats,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))

#You can also refer to variables using helper functions

tbl_summary(
	nlsy_cats,
	by = sex_cat,
	include = c(ends_with("cat"), glasses, age_bir))


#We probably want to name the variables

tbl_summary(
	nlsy_cats,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")



#Do a million other things
#not in curly text is on the table

tbl_summary(
	nlsy_cats,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

################################################################

tbl_summary(
	nlsy_cats,
	by = sex_cat,
	include = c(sex_cat, region_cat, race_eth_cat, income,
							sleep_wkdy, sleep_wknd),
	label = list(
		sex_cat ~ "Sex",
		region_cat ~ "Region",
		race_eth_cat ~ "Race/ethnicity",
		income ~ "Income",
		sleep_wkdy ~ "SleepWeekday",
		sleep_wknd ~ "SleepWeekend"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

