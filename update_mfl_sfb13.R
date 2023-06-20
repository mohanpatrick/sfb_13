library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(lubridate)

options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

#### DELETE AFTER TESTING ########
GITHUB_PAT <- Sys.setenv("REPO_PAT")

 mfl_leagues <- mfl_getendpoint(mfl_connect(2022),"leagueSearch", user_agent="MFLRCLIENT", SEARCH="#SFB12") |>
   purrr::pluck("content","leagues","league") |>
   tibble::tibble() |>
   tidyr::unnest_wider(1) |>
   select( league_name = name, league_id = id,league_home = homeURL) |>
   # Going to need some stricter filtering patterns, but for now we take out obvious not real ones
   filter(!grepl("[Mirror]|[Mock]|[Template]", league_name))


 fwrite(mfl_leagues,"mfl_league_ids.csv",quote = TRUE)

mfl_leagues <- fread("mfl_league_ids.csv")

get_mfl_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(1)
  conn <- mfl_connect(2022, league_id, user_agent = "MFLRCLIENT", rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_draft(conn)
}


# For Testing, subset leagues

mfl_leagues <- mfl_leagues |>
  slice_head(n=25)



mfl_drafts <- mfl_leagues |>
  mutate(drafts = map(league_id, possibly(get_mfl_draft, otherwise = tibble()))) |>
  unnest(drafts)

# We add divisions here which is probably the unit of focus for draft dashboard for MFL. Sleeper seems to lack divisionsm but maybe an
# artifact of all being live last year?
divisions <- mfl_drafts |>
  select(league_id, division_name) |>
  distinct()



#pb_upload("draft_picks_mfl.csv", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")
#pb_upload("timestamp.txt", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")

# Add interval between picks
mfl_drafts <- mfl_drafts |>
  mutate(
    player_id = as.character(player_id)
  ) |>
  group_by(league_id, division) |>
  mutate(
        timestamp = as.POSIXct(timestamp, origin= "1970-01-01"),
        time_to_pick_int = interval(lag(timestamp), timestamp),
        time_to_pick = seconds(time_to_pick_int)
  )

# May have to group first? or Do it separately unless we can group within lag()

# For testing only

mfl_drafts <- mfl_drafts |>
  mutate(
      player_name = ifelse(overall > 48, NA, player_name),
      player_id = ifelse(overall > 48, NA, player_id),
      timestamp = ifelse(overall > 48, NA, timestamp)
  )

mfl_drafts |>
  filter(time_to_pick < 0 )


league_url_base = "https://www43.myfantasyleague.com/2022/options?L="
league_url_mid = "&O=17&DISPLAY=DIVISION"



mfl_current_picks <- mfl_drafts |>
  filter(!is.na(player_name)) |>
  group_by(league_id, division, division_name) |>
  summarize(
    last_pick = max(overall),
    last_pick_ts = max(timestamp),
    mean_time_to_pick = mean(time_to_pick, na.rm = TRUE)
  ) |>
  mutate(otc = last_pick +1,
         last_pick_ts = with_tz(last_pick_ts, tz = "America/New_York"  ),
         time_otc = interval(last_pick_ts,now(tzone = "America/New_York")),
         draft_url = paste0(league_url_base, league_id, league_url_mid, division)
  )

# Get name of franchise OTC
mfl_current_picks <- mfl_current_picks |>
  left_join(mfl_drafts|>select(league_id, division, division_name, overall, franchise_name), by=c("league_id" ="league_id","division" = "division", "otc" = "overall", "division_name" = "division_name") )

# What else do we need for the league summary?
# Pace of picks?

# Add common player_ids to get combined ADP
master_player_ids <- dp_playerids() |>
  select(mfl_id, sleeper_id, name, merge_name) |>
  mutate(
    cross_mfl_sleep_id = paste0(mfl_id, "-", sleeper_id)
  )

mfl_drafts <- mfl_drafts |>
  left_join(master_player_ids, by=c("player_id" = "mfl_id"))

#https://www43.myfantasyleague.com/2022/options?L=16404&O=17&DISPLAY=DIVISION03

league_url_base = "https://www43.myfantasyleague.com/2022/options?L="
league_url_mid = "&O=17&DISPLAY=DIVISION"


draft_picks_mfl <- read_csv("draft_picks_mfl.csv")







fwrite(divisions, "divisions_mfl.csv", quote = TRUE)
fwrite(mfl_drafts,"draft_picks_mfl.csv",quote = TRUE)
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")




mfl_adp <- mfl_drafts |>
  filter(!is.na(player_id)) |>
  group_by(league_id,division,pos) |>
  mutate(pos_rank = rank(overall)) |>
  group_by(player_id, player_name, pos, team) |>
  summarise(
    n = n(),
    overall_avg = mean(overall, na.rm = TRUE) |> round(2),
    overall_sd = sd(overall, na.rm = TRUE) |> round(2),
    pos_avg = mean(pos_rank, na.rm = TRUE) |> round(2),
    pos_sd = sd(pos_rank, na.rm = TRUE) |> round(2),
    overall_min = min(overall, na.rm = TRUE),
    overall_max = max(overall, na.rm = TRUE),
    pos_min = min(pos_rank, na.rm = TRUE),
    pos_max = max(pos_rank, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(overall_avg,-n)

fwrite(adp,"adp_mfl.csv")
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")
pb_upload("adp_mfl.csv", repo = "mohanpatrick/sfb_13", tag = "data_mfl")
pb_upload("timestamp.txt", repo = "mohanpatrick/sfb_13", tag = "data_mfl")

cli::cli_alert_success("Successfully got all picks and ADP!")



