# Packages ----
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(gt)
library(webshot)
# data ----

pokemon <- c("Bulbasaur", "Squirtle", "Charmander")
round1 <- c(rnorm(3))
round2 <- c(rnorm(3))
round3 <- c(rnorm(3))
round4 <- c(rnorm(3))
round5 <- c(rnorm(3))
quote <-
  c("Everybody makes a wrong turn once in a while. - Ash Ketchum",
  "You see, sometimes friends have to go away, but a part of them stays behind with you. - Ash Ketchum",
  "A Caterpie may change into a Butterfree, but the heart that beats inside remains the same. - Brock")

df <- data.frame(pokemon, round1, round2, round3, round4, round5, quote)
df$diff <- df$round5 - df$round4
df$checker <- ifelse(df$diff > 0, TRUE, FALSE)

# add symbols into df 
tick <- "&#10004;" #html for a tick
cross <- "&#10008;" #html for a cross

df$symb <-ifelse(df$checker == TRUE, tick, cross)

# make it long ----
df_long <- gather(df, round, scores,
                  round1:round5, factor_key = TRUE)


# make a plot of each measure ----
tibble_plot <- df_long %>%
  group_by(pokemon) %>%
  nest() %>% # nests tibbles inside of tibbles for plotting
  mutate(plot = map(
    data,
    ~ ggplot(., aes(round, scores, group = 1)) +
      geom_line(size = 2) +
      theme_tufte() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      ) +
      geom_point(color = "blue") +
      theme(legend.position = "none")
  )) %>%
  select(-data) %>% # drop the nested tibbles
  mutate(ggplot = NA) # a place holder for the plots


# get the columns ready ----
df_display <- df %>%
  mutate(ggplot = NA) %>% 
  select(pokemon, checker, round5, diff, symb, ggplot, quote) 


# make the gt table ----
gt_output <- tibble(df_display) %>%
  gt(rowname_col = "pokemon" ) %>% # this removes pokemon header and add right border) 
  text_transform(
    locations = cells_body(vars(ggplot)),
    fn = function(x) {
      map(tibble_plot$plot,
          ggplot_image,
          height = px(50),
          aspect_ratio = 3)
      }
    ) %>%
  tab_style(style = list(cell_text(color = "red")),
            locations = cells_body(columns = vars(checker),
                                   rows = diff < 0)
            ) %>%
  tab_style(style = list(cell_text(color = "green")),
            locations = cells_body(columns = vars(checker),
                                   rows =  diff > 0)
            ) %>%
  fmt_passthrough(columns = vars(symb), escape = FALSE) %>% 
  tab_style(style = list(cell_text(color = "red")),
            locations = cells_body(columns = vars(symb),
                                   rows = symb == cross)
            ) %>%
  tab_style(style = list(cell_text(color = "green")),
            locations = cells_body(columns = vars(symb),
                                   rows =  symb == tick)
            ) %>%
  tab_header(title = md("Battle score"),
             subtitle = md("Palette Town Tourney")) %>%
  tab_stubhead(label = "Pokemon") %>%
  tab_row_group(group = "Group 2",
                rows = c("Charmander")) %>%
  tab_row_group(group = "Group 1",
                rows = 1:2) %>% # can add conditions here
  cols_label(
    pokemon = "Pokemon",
    round5 = "Last round" ,
    diff = "Differential" ,
    checker = "Starter",
    symb = "Win", 
    quote = "Quote",
    ggplot = "Trend"
    ) %>% 
  cols_width(vars(quote) ~ px(200))

gt_output

#gtsave(gt_output, "gtSparkline.png")
