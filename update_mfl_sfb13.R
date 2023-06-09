library(ffscrapr)
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

options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

#### DELETE AFTER TESTING ########
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")
#Sys.setenv("MFL_CLIENT")
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
cli::cli_alert("Client ID: {mfl_client}")

 mfl_leagues <- mfl_getendpoint(mfl_connect(2023),"leagueSearch", user_agent="MFLRCLIENT", SEARCH="#SFB13") |>
   purrr::pluck("content","leagues","league") |>
   tibble::tibble() |>
   tidyr::unnest_wider(1) |>
   select( league_name = name, league_id = id,league_home = homeURL) |>
   # Going to need some stricter filtering patterns, but for now we take out obvious not real ones
    filter(!grepl("Mock|Template|Sattelite|Mirror", league_name))|>
    filter(league_id != "57652")
# Add filter for 57652 the stray miller liter


prior_completed_leagues <- read_csv("https://github.com/mohanpatrick/sfb_13/releases/download/data_mfl/completed_leagues.csv", col_names = c("league_name","league_id", "league_home"))|>
  mutate(league_id = as.character(league_id))|>
  select(league_id)



mfl_leagues <- mfl_leagues |>
anti_join(prior_completed_leagues)


 fwrite(mfl_leagues,"mfl_league_ids.csv",quote = TRUE)


get_mfl_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(3)
  conn <- mfl_connect(2023, league_id, user_agent = "MFLRCLIENT", rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_draft(conn)
}


# For Testing, subset leagues


#28764 is London live. Only MFL presumably until Monday



mfl_drafts <- mfl_leagues |>
  mutate(drafts = map(league_id, possibly(get_mfl_draft, otherwise = tibble()))) |>
  unnest(drafts)

# We add divisions here which is probably the unit of focus for draft dashboard for MFL. Sleeper seems to lack divisionsm but maybe an
# artifact of all being live last year?
# ok, no divisions yet?
#divisions <- mfl_drafts |>
#  select(league_id, division_name) |>
#  distinct()



#pb_upload("draft_picks_mfl.csv", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")
#pb_upload("timestamp.txt", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")












# Add interval between picks
mfl_drafts <- mfl_drafts |>
  mutate(
    player_id = as.character(player_id)
  ) |>
  group_by(league_id) |> # Note removed division here
  mutate(
        timestamp = as.POSIXct(timestamp, origin= "1970-01-01"),
        time_to_pick_int = interval(lag(timestamp), timestamp),
        time_to_pick = seconds(time_to_pick_int)
  )

# May have to group first? or Do it separately unless we can group within lag()

# Check for completed

completed <- mfl_drafts |>
  filter(!is.na(timestamp))|>
  group_by(league_name,league_id, league_home) |>
  summarise(last_pick = max(overall, na.rm=TRUE))|>
  filter(last_pick == 264)|>
  select(-last_pick, -league_name)

write_csv(completed, "completed_leagues.csv", append = TRUE)

completed_drafts <- completed |>
  left_join(mfl_drafts)

write_csv(completed_drafts, "completed_mfl_drafts.csv", append = TRUE)

#fwrite(divisions, "divisions_mfl.csv", quote = TRUE)
fwrite(mfl_drafts,"draft_picks_mfl.csv",quote = TRUE)
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")

pb_upload("draft_picks_mfl.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")


pb_upload("mfl_league_ids.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")


pb_upload("completed_leagues.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")

pb_upload("completed_mfl_drafts.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")

cli::cli_alert_success("Successfully got all picks and ADP!")



