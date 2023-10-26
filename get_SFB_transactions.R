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



search_year = "2023"
sfb_sleeper_string = "\\#SFB13"
rundate = today()

get_mfl_transactions <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(2)
  conn <- mfl_connect(search_year, league_id, user_agent = "MFLRCLIENT", rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_transactions(conn)
}

get_sleeper_transactions <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  Sys.sleep(1)
  conn <- sleeper_connect(search_year , league_id, rate_limit = F)
  suppressWarnings(ff_transactions(conn))
}

get_mfl_rosters <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(2)
  conn <- mfl_connect(search_year, league_id, user_agent = "MFLRCLIENT", rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_rosters(conn)
}

get_sleeper_rosters <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  Sys.sleep(1)
  conn <- sleeper_connect(search_year , league_id, rate_limit = F)
  suppressWarnings(ff_rosters(conn))
}

mfl_client <- Sys.getenv(c("MFL_CLIENT"))
cli::cli_alert("Client ID: {mfl_client}")

cli::cli_alert("Getting MFL League Ids")
mfl_leagues <- mfl_getendpoint(mfl_connect(search_year),"leagueSearch", user_agent=mfl_client, SEARCH="#SFB13") |>
  purrr::pluck("content","leagues","league") |>
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  select( league_name = name, league_id = id,league_home = homeURL) |>
  # Going to need some stricter filtering patterns, but for now we take out obvious not real ones
  filter(!grepl("Mock|Template|Sattelite|Mirror|BEST", league_name))|>
  filter(league_id != "57652")



#### DELETE AFTER TESTING ########
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")

cli::cli_alert("Getting Sleeper League Ids")
sleeper_leagues <- sleeper_userleagues("ScottFishBowl", search_year) |>
  #select(league_id, league_name) |>
  filter(str_detect(league_name,sfb_sleeper_string)) |>
  mutate(league_id = as.character(league_id)) |>
  filter(!grepl("Satellite|Mirror", league_name))

#### DELETE AFTER TESTING: This grabs 20 leagues from each so run will be shorter and less likely to be aborted by MFL

#sleeper_leagues <- sleeper_leagues |>
#  slice_head(n=20)
#mfl_leagues <- mfl_leagues |>
#  slice_head(n=20)

write_csv(sleeper_leagues, "sleeper_sfb_leagues.csv")
write_csv(mfl_leagues, "mfl_sfb_leagues.csv")

# Get all transactions 
cli::cli_alert("Getting sleeper transactions")

sleeper_transactions <- sleeper_leagues |>
  select(league_id, league_name) |>
  mutate(transactions  = map(league_id, possibly(get_sleeper_transactions, otherwise = tibble()))) |>
  unnest(transactions ) 

cli::cli_alert("Getting MFL transactions")
mfl_transactions <- mfl_leagues |>
  mutate(transactions = map(league_id, possibly(get_mfl_transactions, otherwise = tibble()))) |>
  unnest(transactions)

# Load player ids, we do this to find ff ids and allow merge of Sleeper/MFL
master_player_ids <- dp_playerids() |>
  select(mfl_id, sleeper_id, name, merge_name, position, team) |>
  filter(position %in% c("QB","WR", "TE", "RB", "K", "PK", "FB")) |>
  mutate(
    cross_mfl_sleep_id = paste0(mfl_id, "-", sleeper_id),
    sleeper_id = sleeper_id,
    mfl_id = mfl_id,
    clean_team = team
  )|>
  filter(mfl_id != 	11600)

# Get display names and headshot URLs for players
players <- load_players()|>
  filter( status != "RET" & position %in% c("QB","WR", "TE", "RB", "K", "PK", "FB"))|>
  select(display_name, first_name, last_name, gsis_id, position, headshot)|>
  mutate(
    merge_name = dp_cleannames(display_name,
                               convert_lastfirst = TRUE,
                               use_name_database = TRUE,
                               lowercase = TRUE
    )
  )
master_player_ids <- master_player_ids|>
  left_join(players, by=c("merge_name" = "merge_name", "position" = "position"))

# Clean up transaction datasets so we can merge them

sleep_trans_sum <- sleeper_transactions |>
  filter(type != "waiver_failed")|> # Removes unsuccessful bids
  mutate(tran_date = as.Date(timestamp),
         pos = ifelse(pos == "K", "PK", pos),
         bbid_spent = bbid_amount,
         type =  ifelse(type=="free_agent", "FREE_AGENT", "BBID_WAIVER") #Align with MFL transaction types
  )  |>
  left_join(master_player_ids, by=c("player_id" = "sleeper_id", "pos" = "position"))|>
  select(cross_mfl_sleep_id, name,tran_date, pos, type_desc, bbid_spent, headshot, type,mfl_id)


mfl_trans_sum <- mfl_transactions |>
  mutate(tran_date = as.Date(timestamp))  |>
  #filter(type == "BBID_WAIVER")|>
  left_join(master_player_ids, by=c("player_id" = "mfl_id"))|>
  select(cross_mfl_sleep_id,player_id, name, tran_date, pos, type_desc, bbid_spent, headshot, type)|>
  mutate(mfl_id = player_id)|>
  select(-player_id)

# Merge transaction sets
all_transactions <- bind_rows(mfl_trans_sum, sleep_trans_sum)|>
mutate(bbid_spent =ifelse(!is.na(bbid_spent), bbid_spent,0))

write_csv(all_transactions, "sfb_all_transactions.csv")
write_csv(as_tibble(rundate), "date_run.csv")


pb_upload("sfb_all_transactions.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded transactions to Git")

pb_upload("date_run.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded run date to Git")

