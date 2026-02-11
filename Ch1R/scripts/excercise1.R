## Install Packages
install.packages("nflfastR")
install.packages("tidyverse")

##Load the packages
library("tidyverse")
library("nflfastR")

##Load play by play data
pbp_r <- load_pbp(2021)

## Filter for passing plays
pbp_r_p <- pbp_r |>
  filter(play_type == 'pass' & !is.na(air_yards))

## Average Depth of Target (aDOT)
adot_summary <- pbp_r_p |>
  group_by(passer_id, passer) |>
  summarize(n= n(), adot = mean(air_yards)) |>
  filter(n >= 100 & !is.na(passer)) |>
  arrange(-adot)

## Preview in console the results
print(adot_summary, n = Inf)

## Print Results into a CSV
write_csv(adot_summary, "outputs/adot_summary2021.csv")


## Get top 20 Passers
top_passers <- adot_summary |>
  slice_max(order_by = adot, n = 20)

## Bar Chart for Top 20 Passers
ggplot(top_passers, aes(x = reorder(passer, adot), y = adot)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Passers by Average Depth of Target (aDOT)",
    x = "Passer",
    y = "aDOT (yards)"
  )

## Save Bar Chart as a PNG
ggsave("outputs/top20_passers_adot.png", width = 6, height = 4)
