library(ffscrapr)
library(nflverse)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(lubridate)
library(piggyback)


options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

#For testing
waiver_date = "2023-09-13"

#waiver_date = today()


all_transactions <- read_csv("https://github.com/mohanpatrick/sfb_13/releases/download/data_mfl/sfb_all_transactions.csv")

# Grab just this weeks waiver run and summarize at the player level
current_waivers <- all_transactions|>
  filter(tran_date == waiver_date & type == "BBID_WAIVER") |>
  group_by(cross_mfl_sleep_id,mfl_id, name,pos,headshot)|>
  summarise(
    adds = sum(ifelse(type_desc == "added",1,0)),
    drops = sum(ifelse(type_desc == "dropped",1,0)),
    total_bbid = sum(bbid_spent, na.rm = TRUE),
    mean_bbid = mean (bbid_spent, na.rm = TRUE),
    total = adds + drops
  )|>
  mutate(player_id = cross_mfl_sleep_id)|>
  arrange(desc(adds))

write_csv(current_waivers, "current_waivers.csv")


# Create gt() tables and images 

footer_text = paste("Run Date: ",waiver_date, " Data from @ffscrapr", sep=" ")


# Most added table
top_adds_table <- current_waivers|>
  ungroup() |>
  select(name, pos, headshot,adds,drops, mean_bbid)|>
  slice_head(n=20)|>
  gt() |>
  cols_label(
    name = "Name",
    pos = "Pos",
    headshot = "",
    adds = "Adds",
    drops = "Drops",
    mean_bbid = "Avg. Bid"
  ) |>
  tab_header(
    title = md("#SFB13 Top 20 Waiver Adds"),
    subtitle = md("Includes MFL and Sleeper Waiver Runs. Not free agent transactions")
  ) |>
  #gt_hulk_col_numeric(num_over) |>
  tab_source_note(source_note = footer_text) |>
  gt_img_rows(columns = headshot, img_source = "web", height = 30)|>
  tab_options(data_row.padding = px(1))|>
  fmt_currency(columns = c(mean_bbid), currency = "USD", decimals = 0)|>
  gt_theme_nytimes() 

top_adds_table |> gtsave("top-adds.png", expand = 20)
top_adds_table |> gtsave("top-adds.html", inline_css = TRUE)

# Most dropped table
top_drops_table <- current_waivers|>
  ungroup() |>
  arrange(desc(drops))|>
  select(name, pos, headshot,adds,drops)|>
  slice_head(n=20)|>
  gt() |>
  cols_label(
    name = "Name",
    pos = "Pos",
    headshot = "",
    adds = "Adds",
    drops = "Drops"
  ) |>
  tab_header(
    title = md("#SFB13 Top 20 Waiver Drops"),
    subtitle = md("Includes MFL and Sleeper Waiver Runs. Not free agent transactions")
  ) |>
  #gt_hulk_col_numeric(num_over) |>
  tab_source_note(source_note = footer_text) |>
  gt_img_rows(columns = headshot, img_source = "web", height = 30)|>
  tab_options(data_row.padding = px(1))|>
  gt_theme_nytimes() 

top_drops_table |> gtsave("top-drops.png", expand = 20)
top_drops_table |> gtsave("top-drops.html", inline_css = TRUE)



top_spend_table <- current_waivers|>
  ungroup() |>
  arrange(desc(mean_bbid))|>
  select(name, pos, headshot,adds,drops, mean_bbid)|>
  slice_head(n=15)|>
  gt() |>
  cols_label(
    name = "Name",
    pos = "Pos",
    headshot = "",
    adds = "Adds",
    drops = "Drops",
    mean_bbid = "Avg. Bid"
  ) |>
  tab_header(
    title = md("#SFB13Waivers Top 15 Highest Avg $ Spend"),
    subtitle = md("Includes MFL and Sleeper Waiver Runs. Not free agent transactions")
  ) |>
  #gt_hulk_col_numeric(num_over) |>
  gt_img_rows(columns = headshot, img_source = "web", height = 30)|>
  tab_source_note(source_note = footer_text) |>
  tab_options(data_row.padding = px(1))|>
  fmt_currency(columns = c(mean_bbid), currency = "USD", decimals = 0)|>
  gt_theme_nytimes() 

top_spend_table  |> gtsave("top-spend.png", expand = 20)
top_spend_table  |> gtsave("top-spend.html", inline_css = TRUE)





pb_upload("current_waivers.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded waivers to Git")


pb_upload("top-adds.png",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded top adds PNG to Git")


pb_upload("top-adds.html",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded top adds HTML to Git")


pb_upload("top-drops.png",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded top drops PNG to Git")


pb_upload("top-drops.html",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded top drops HTML to Git")


pb_upload("top-spend.png",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded top spend PNG to Git")


pb_upload("top-spend.html",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded top spend HTML to Git")




