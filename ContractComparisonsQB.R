

# Load in player snap counts to get rolling snaps in the past 3 years (will apply to players who have played 4 seasons and are FAs for a new deal)
player_snap_counts <- load_snap_counts(2012:2024) |> filter(game_type == "REG" & season > 2015) |> #
  group_by(pfr_player_id, season) |> 
  summarize(player, rolling_snaps = sum(offense_snaps)) |> unique() |> 
  ungroup() |> 
  group_by(pfr_player_id) |> 
  mutate(
    rolling_3_snaps = rolling_snaps + lag(rolling_snaps, n = 1) + lag(rolling_snaps, n = 2)) |> 
  ungroup() |> mutate(player = clean_player_names(player))


# Load in the contracts from OTC
contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))
rosters2018_2024 <- load_rosters(2017:2024)

# Table for the start dates of free agency (will be useful later)
free_agency_start_dates <- nfl_free_agency <- tibble(
  Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  Date = as.Date(c(
    "2018-03-14",
    "2019-03-13",
    "2020-03-18",
    "2021-03-17",
    "2022-03-16",
    "2023-03-15",
    "2024-03-13",
    "2025-03-12"
  ), format = "%Y-%m-%d")
) 

# Load in the PFF Data
passing_summary_2024 <- read.csv("passing_summary.csv") |> mutate(season = 2024)
passing_summary_2023 <- read.csv("passing_summary (1).csv") |> mutate(season = 2023)
passing_summary_2022 <- read.csv("passing_summary (2).csv") |> mutate(season = 2022)
passing_summary_2021 <- read.csv("passing_summary (3).csv") |> mutate(season = 2021)
passing_summary_2020 <- read.csv("passing_summary (4).csv") |> mutate(season = 2020)
passing_summary_2019 <- read.csv("passing_summary (5).csv") |> mutate(season = 2019)
passing_summary_2018 <- read.csv("passing_summary (6).csv") |> mutate(season = 2018)
passing_summary_2017 <- read.csv("passing_summary (7).csv") |> mutate(season = 2017)

passing_summary_2017_2024 <- rbind(passing_summary_2024, passing_summary_2023, passing_summary_2022,
                                     passing_summary_2021, passing_summary_2020, passing_summary_2019,
                                     passing_summary_2019, passing_summary_2018, passing_summary_2017)

rushing_summary_2024 <- read.csv("rushing_summary.csv") |> mutate(season = 2024)
rushing_summary_2023 <- read.csv("rushing_summary (1).csv") |> mutate(season = 2023)
rushing_summary_2022 <- read.csv("rushing_summary (2).csv") |> mutate(season = 2022)
rushing_summary_2021 <- read.csv("rushing_summary (3).csv") |> mutate(season = 2021)
rushing_summary_2020 <- read.csv("rushing_summary (4).csv") |> mutate(season = 2020)
rushing_summary_2019 <- read.csv("rushing_summary (5).csv") |> mutate(season = 2019)
rushing_summary_2018 <- read.csv("rushing_summary (6).csv") |> mutate(season = 2018)
rushing_summary_2017 <- read.csv("rushing_summary (7).csv") |> mutate(season = 2017)

rushing_summary_2017_2024 <- rbind(rushing_summary_2024, rushing_summary_2023, rushing_summary_2022,
                                   rushing_summary_2021, rushing_summary_2020, rushing_summary_2019,
                                   rushing_summary_2019, rushing_summary_2018, rushing_summary_2017)


# Important stats from PFF for the position
QB_Stats <- passing_summary_2017_2024 |> left_join(rushing_summary_2017_2024 |> select(player_id, player, season, position, team_name, run_plays, touchdowns, yards), 
                                                     by = c("player_id", "season", "position", "team_name", "player")) |> 
  summarize(player, player_id, position, season, team_name, 
            grades_offense, grades_run,
            attempts, sacks, touchdowns = touchdowns.x + touchdowns.y, turnover_worthy_plays,
            passing_yards = yards.x, rushing_yards = yards.y,
            snaps = passing_snaps + run_plays) |> 
  filter(position %in% c("QB")) |> unique()



# Use nflverse rosters to filter position
rosters2018_2024_QB <- rosters2018_2024 |> ungroup() |> filter(position %in% c("QB"))



rosters_QB_Contract <- rosters2018_2024_QB |> 
  summarize(birth_date = as.Date(birth_date, format = "%Y-%m-%d"), full_name, gsis_id) |>  # Get their birth date as date
  left_join(contracts, by = "gsis_id") |>  # Merge with the contract data
  # Important data manipulation to find things like contract number
  mutate(rookie_contract = ifelse(draft_year == year_signed, 1, 0)) |> 
  dplyr::select(-cols) |> 
  unique() |> 
  summarize(full_name, gsis_id, position, team, year_signed, draft_year, years, value, apy, birth_date,
            guaranteed, apy_cap_pct, height, weight, draft_round, draft_overall, rookie_contract) |> 
  group_by(gsis_id) |> 
  distinct(gsis_id, year_signed, draft_year, years, value, apy, birth_date, 
           guaranteed, apy_cap_pct, height, weight, draft_round, draft_overall, rookie_contract, .keep_all = TRUE) |>
  arrange(year_signed) |> 
  mutate(contract_number = row_number(),
         contract_number = case_when(
           contract_number == 1 ~ "1",
           contract_number == 2 ~ "2" ,
           contract_number > 2 ~ "3+",
           TRUE ~ as.character(contract_number)
         )) |> 
  ungroup() |> 
  left_join(free_agency_start_dates, by = c("year_signed" = "Year")) |> 
  filter(!is.na(Date)) |> 
  mutate(age_at_signing = round(as.numeric(difftime(as.Date(Date), birth_date, units = "days")) / 365.25, 1)) |>  # Age at signing approximation
  left_join(cap_ref |> summarize(cap, year), by = c("year_signed" = "year")) |> 
  mutate(guaranteed_contract_pct = guaranteed/value, 
         full_name = clean_player_names(full_name),
         ybs = year_signed - 1)

# Merge cleaned data with the PFF Data
QB_contracts <- rosters_QB_Contract |> 
  left_join(QB_Stats |> mutate(player = clean_player_names(player)), 
            by = c("full_name" = "player", "ybs" = "season")) |> 
  filter(!is.na(snaps)) |> 
  unique() |> 
  mutate(FA_2025 = 0) |> # Marker for the training data
  mutate(position = position.y) |> 
  select(-position.x, -position.y)  |>  # added
  left_join(player_snap_counts |> select(player, season, rolling_3_snaps), by = c("full_name" = "player", "ybs" = "season")) # Merge with snap counts

# Similar to the above data manipulation, but we are doing this to eventually merge with the FA data for THIS YEAR
rosters_QB_Contract_2024 <- rosters2018_2024_QB |> 
  summarize(birth_date = as.Date(birth_date, format = "%Y-%m-%d"), full_name, gsis_id, season) |> 
  left_join(contracts, by = c("gsis_id")) |>
  mutate(rookie_contract = ifelse(draft_year == year_signed, 1, 0)) |> 
  unique() |> 
  summarize(full_name, gsis_id, position, team, year_signed, draft_year, years, value, apy, birth_date,
            guaranteed, apy_cap_pct, height, weight, draft_round, draft_overall, rookie_contract, season) |> 
  group_by(gsis_id) |> 
  distinct(gsis_id, year_signed, draft_year, years, value, apy, birth_date, 
           guaranteed, apy_cap_pct, height, weight, draft_round, draft_overall, rookie_contract, .keep_all = TRUE) |>
  arrange(year_signed) |> 
  mutate(contract_number = row_number(),
         contract_number = case_when(
           contract_number == 1 ~ "1",
           contract_number == 2 ~ "2" ,
           contract_number > 2 ~ "3+",
           TRUE ~ as.character(contract_number)
         )) |> 
  slice_tail(n = 1) |> 
  ungroup() |> 
  dplyr::select(!c(draft_year, year_signed, years, value, apy, guaranteed, apy_cap_pct)) |> 
  mutate(age_at_signing = round(as.numeric(difftime(as.Date("2025-03-12"), birth_date, units = "days")) / 365.25, 1)) |> 
  mutate(full_name = clean_player_names(full_name))   |> 
  left_join(player_snap_counts |> select(player, season, rolling_3_snaps) |> filter(season == 2024) |> select(-season), by = c("full_name" = "player")) # added


# Load in the free agent data and combine with measurables and such (no contracts included because they haven't signed yet)
free_agents_QB_2024 <- read.csv("FreeAgents2025.csv") |> 
  filter(sortable.3 %in% c("SFA", "UFA", "Void")) |> 
  filter(sortable.2 %in% c("QB")) |> 
  summarize(full_name = clean_player_names(sortable)) |>
  add_row(full_name = "Aaron Rodgers") |> 
  left_join(rosters_QB_Contract_2024, by = "full_name") |> 
  mutate(contract_number = case_when(
    contract_number == "1" ~ "2",
    contract_number == "2" ~ "3+",
    TRUE ~ contract_number
  )) |> 
  select(-season) 


# Merge with the PFF Data and label as the training data
QB_contracts_2024 <- free_agents_QB_2024 |> 
  left_join(QB_Stats |> filter(season == 2024) |> filter(position %in% c("QB")) |> mutate(player = clean_player_names(player)), 
            by = c("full_name" = "player")) |> 
  filter(!is.na(snaps)) |> 
  unique() |> 
  mutate(FA_2025 = 1) |> 
  mutate(position = position.y) |> 
  dplyr::select(-position.x, -position.y) |> 
  mutate(team_name = clean_team_abbrs(team_name)) |> 
  left_join(teams_colors_logos |> summarize(team_abbr, team_nick), by = c("team_name" = "team_abbr"))


# Combine the training and test data
QB_contracts_combined <- bind_rows(QB_contracts, QB_contracts_2024) |> 
  summarize(full_name, gsis_id, team, position, 
            year_signed = case_when(
              is.na(year_signed) ~ 2025,
              TRUE ~ year_signed
            ), value, apy, guaranteed, guaranteed_contract_pct,
            apy_cap_pct, height, weight = as.numeric(weight), draft_round, draft_overall, contract_number, age_at_signing,
            cap = case_when(
              is.na(cap) ~ 279.2,
              TRUE ~ cap
            ), team_name, snaps, 
            grades_offense, 
            #grades_run,
            attempts, sacks, touchdowns, turnover_worthy_plays,
            passing_yards, rushing_yards,
            rolling_3_snaps, #Added
            FA_2025) |> 
  group_by(full_name, year_signed) %>%  
  slice_max(order_by = value, n = 1, with_ties = FALSE) %>%  
  ungroup()

# Select the columns for use and turn non-numeric variables into numeric or dummy
QB_contracts_combined_forknn <- QB_contracts_combined |> 
  summarize(full_name, team, position, year_signed, height, weight, draft_overall, contract_number, age_at_signing, snaps, 
            grades_offense, 
            #grades_run,
            attempts, sacks, touchdowns, turnover_worthy_plays,
            passing_yards, rushing_yards,
            rolling_3_snaps,
            FA_2025) %>%
  separate(height, into = c("Feet", "Inches"), sep = "'", remove = FALSE) %>% 
  mutate(
    Feet = as.numeric(Feet),
    Inches = as.numeric(gsub("\"", "", Inches)), 
    height = (Feet * 12) + Inches  
  ) %>%
  mutate(contract_number = case_when(
    contract_number == "1"  ~ 1,
    contract_number == "2"  ~ 2,
    contract_number == "3+" ~ 3,
    TRUE ~ 0  # Default case (if needed)
  )) |> 
  dplyr::select(-Feet, -Inches) %>%
  mutate(
    draft_overall = ifelse(is.na(draft_overall), 300, draft_overall)  # Large number to separate undrafted players
  ) |>
  dummy_cols(select_columns = c("position"), remove_first_dummy = F) %>%
  dplyr::select(-position) %>%
  #dplyr::select(!c(pressures_allowed, sacks_allowed, player_game_count)) |> 
  unique()


# Normalize for the KNN
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Apply the normalization to the data
QB_contracts_combined_forknn_normalized <- QB_contracts_combined_forknn %>%
  mutate(
    across(where(is.numeric) & !all_of(c("year_signed", "FA_2025")), normalize),
    position_QB = 1) |>
  na.rm()

# Establish the weights for this model
weights <- c(
  height = 0.5, 
  weight = 0.5, 
  draft_overall = 3, 
  contract_number = 25, 
  age_at_signing = 7,
  snaps = 3,
  grades_offense = 3,
  attempts = 2,
  passing_yards = 8,
  touchdowns = 7,
  turnover_worthy_plays = 5,
  rushing_yards = 2,
  rolling_3_snaps = 4, # Added
  sacks = 2,
  position_QB = 1
)



# Split the training and test model
train_data <- QB_contracts_combined_forknn_normalized[,c(1,3, 4:ncol(QB_contracts_combined_forknn_normalized))] %>% filter(FA_2025 == 0) %>% dplyr::select(-FA_2025)%>%
  mutate(across(names(weights), ~ . * weights[cur_column()], .names = "weighted_{col}"))


test_data <- QB_contracts_combined_forknn_normalized[,c(1,3, 4:ncol(QB_contracts_combined_forknn_normalized))] %>% filter(FA_2025 == 1) %>% dplyr::select(-FA_2025)%>%
  mutate(across(names(weights), ~ . * weights[cur_column()], .names = "weighted_{col}"))

# Run the knn with 5 closest neighbors 
k <- 5 # Added
knn_result <- get.knnx(data = train_data[3:ncol(train_data)], query = test_data[3:ncol(test_data)], k = k)


# Take the results from the KNN and make a table
top_matches <- as.data.frame(knn_result$nn.index) 
top_distances <- as.data.frame(knn_result$nn.dist)

similarity_scores <- 1 - (top_distances / max(top_distances))
similarity_scores <- round(similarity_scores * 100, 2)

top_matches_named <- tibble(
  Player_Source = test_data$full_name,
  Match_1 = map_chr(top_matches$V1, ~ train_data$full_name[.x]),
  Match_1_year = map_chr(top_matches$V1, ~ train_data$year_signed[.x]),
  Score_1 = map_dbl(top_distances$V1, ~ similarity_scores$V1[which(top_distances$V1 == .x)]),
  Match_2 = map_chr(top_matches$V2, ~ train_data$full_name[.x]),
  Match_2_year = map_chr(top_matches$V2, ~ train_data$year_signed[.x]),
  Score_2 = map_dbl(top_distances$V2, ~ similarity_scores$V2[which(top_distances$V2 == .x)]),
  Match_3 = map_chr(top_matches$V3, ~ train_data$full_name[.x]),
  Match_3_year = map_chr(top_matches$V3, ~ train_data$year_signed[.x]),
  Score_3 = map_dbl(top_distances$V3, ~ similarity_scores$V3[which(top_distances$V3 == .x)]),
  Match_4 = map_chr(top_matches$V4, ~ train_data$full_name[.x]),
  Match_4_year = map_chr(top_matches$V4, ~ train_data$year_signed[.x]),
  Score_4 = map_dbl(top_distances$V4, ~  similarity_scores$V4[which(top_distances$V4 == .x)]),
  Match_5 = map_chr(top_matches$V5, ~ train_data$full_name[.x]),
  Match_5_year = map_chr(top_matches$V5, ~ train_data$year_signed[.x]),
  Score_5 = map_dbl(top_distances$V5, ~  similarity_scores$V5[which(top_distances$V5 == .x)]),
  #Match_6 = map_chr(top_matches$V6, ~ train_data$full_name[.x]),
  #Match_6_year = map_chr(top_matches$V6, ~ train_data$year_signed[.x]),
  #Score_6 = map_dbl(top_distances$V6, ~  similarity_scores$V6[which(top_distances$V6 == .x)]) # Added
) |> 
  mutate(across(starts_with("Score_"), as.character),
         across(starts_with("Match_") & ends_with("_year"), as.numeric),
         across(starts_with("Match_") & ends_with("_year"), as.character)) |> 
  pivot_longer(
    cols = matches("Match_\\d+|Match_\\d+_year|Score_\\d+"),
    names_to = c("Type", "Match_Num"), 
    names_pattern = "(Match|Match_year|Score)_(\\d+)",
    values_drop_na = TRUE  
  ) |>
  pivot_wider(
    names_from = Type,  # Spread Match, Match_year, and Score into separate columns
    values_from = value,
    values_fn = list
  ) %>%
  mutate(
    Match2 = map_chr(Match, ~ .x[1]),  # Extract first value from list
    Match_year = map_chr(Match, ~ .x[2]),  # Extract first value from list
    Score = as.numeric(Score)  # Convert Score back to numeric
  ) %>%
  summarize(Player_Source, Match_Num, Match2, Match_year = as.numeric(Match_year), Score) 


# Add the matched data to a list of players 
everything_matched_QB <- top_matches_named |> left_join(QB_contracts, 
                                                        by = c("Match2"= "full_name", 
                                                               "Match_year"= "year_signed" 
                                                        )) |> 
  summarize(Player_Source, Match_Num, Match2, Match_year, Score, team, position, height, weight, draft_overall, contract_number, age_at_signing, 
            years, value, apy, apy_cap_pct, inflated_apy = apy_cap_pct*279.2, guaranteed, snaps, 
            grades_offense, 
            #grades_run,
            attempts, sacks, touchdowns, turnover_worthy_plays,
            passing_yards, rushing_yards,
            rolling_3_snaps,
            FA_2025) |>
  dplyr::select(!c(FA_2025)) |> 
  group_by(Match2, Player_Source, Match_Num) |> 
  slice_max(value, with_ties = FALSE) |>
  ungroup() |> unique() |> 
  left_join(teams_colors_logos |> select(team_nick, team_logo_espn), by = c("team" = "team_nick")) |> unique() |> 
  left_join(load_rosters(2018:2024) |> select(full_name, headshot_url, season)|> mutate(full_name = clean_player_names(full_name)), by = c("Match2" = "full_name", "Match_year" = "season")) |>
  mutate(headshot_url = ifelse(Match2 == "AJ Green" & Match_year == 2021, "https://static.www.nfl.com/image/private/f_auto,q_auto/league/ibugzfv4xaeq9y4xurmj", headshot_url)) |> 
  filter(!(Match2 == "David Moore" & is.na(headshot_url))) |> 
  unique()

# Dataframe of names and variables of the free agents matched upon
everything_matched_QB_2024 <- QB_contracts_2024 |> 
  summarize(Player_Source = full_name, year = 2025, team, position, height, weight, draft_overall, contract_number, age_at_signing, 
            snaps, 
            grades_offense, 
            #grades_run,
            attempts, sacks, touchdowns, turnover_worthy_plays,
            passing_yards, rushing_yards,
            rolling_3_snaps,
            FA_2025) |> 
  dplyr::select(!c(FA_2025)) |> 
  left_join(load_rosters(2024) |> select(full_name, headshot_url)|> mutate(full_name = clean_player_names(full_name)), by = c("Player_Source" = "full_name")) |> unique()



write.csv(everything_matched_QB, "QBMatched.csv")
write.csv(everything_matched_QB_2024, "QBID.csv")

