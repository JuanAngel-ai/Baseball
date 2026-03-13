library(Lahman)
library(tidyverse)

PlayerInfo <- People |>
  filter(
    playerID %in% c(
      "ruthba01", "aaronha01", "bondsba01", "rodrial01"
    )
  ) |>
  mutate(
    mlb_birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    ),
    Player = paste(nameFirst, nameLast)
  ) |>
  select(playerID, Player, mlb_birthyear)


HR_data <- Batting |> 
  inner_join(PlayerInfo, by = "playerID") |>
  mutate(Age = yearID - mlb_birthyear) |>
  select(Player, Age, HR) |>
  group_by(Player) |>
  mutate(cHR = cumsum(HR))

ggplot(HR_data, aes(x = Age, y = cHR, color = Player)) +
  geom_line() 
