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
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")

 mfl_leagues <- mfl_getendpoint(mfl_connect(2023),"leagueSearch", user_agent="MFLRCLIENT", SEARCH="#SFB13") |>
   purrr::pluck("content","leagues","league") |>
   tibble::tibble() |>
   tidyr::unnest_wider(1) |>
   select( league_name = name, league_id = id,league_home = homeURL) |>
   # Going to need some stricter filtering patterns, but for now we take out obvious not real ones
   filter(!grepl("[Mirror]|[Mock]|[Template]", league_name))


 fwrite(mfl_leagues,"mfl_league_ids.csv",quote = TRUE)


get_mfl_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(1)
  conn <- mfl_connect(2023, league_id, user_agent = "MFLRCLIENT", rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_draft(conn)
}


# For Testing, subset leagues





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

# For testing only





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


#pb_upload("divisions_mfl.csv",
#          repo = "mohanpatrick/sfb_13",
#          tag = "data_mfl")
#cli::cli_alert_success("Successfully uploaded to Git")


cli::cli_alert_success("Successfully got all picks and ADP!")



