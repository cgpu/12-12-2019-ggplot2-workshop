# exercise 1

library(ggplot2)
library(SmarterPoland)
library(tidyimpute)
library(dplyr)
library(devtools)

# `countries` dataset is included in {SmarterPoland}
# redundant but explicit which we like
countries <- SmarterPoland::countries

# If df has missing values, visualise them:
if (nrow(dplyr::filter_all(countries, any_vars(is.na(.)))) > 0) {
  devtools::source_url('https://raw.githubusercontent.com/cgpu/xPanDaR/master/R/prepare_missingness_absence_presence_graph.R')
  prepare_missingness_absence_presence_graph(dplyr::filter_all(countries, any_vars(is.na(.))) ,  ts_id = 'country' ) + coord_flip() + theme(axis.text.x = element_text(angle = 50, hjust = 1))

  # Perform basic imputation and round imputed cols
  countries %>%
    tidyimpute::impute_mean()  %>%
    dplyr::mutate_if(is.numeric, round, digits = 1 ) ->
    countries

}

# Start ggplotting
ggplt <- ggplot(countries, aes(x     = death.rate,
                               y     = birth.rate,
                               label = country,
                               size  = birth.rate,
                               color = birth.rate)) +

  geom_point() +

  scale_color_gradient2(midpoint = mean(countries$birth.rate),
                        low      = "#4974a5",
                        mid      = "#4A637B",
                        high     = "#f35f71",
                        space    = "Lab" ) +

  scale_size_continuous(range = c(0.05,3)) +


  # This is not redundant (cc' theme). See: https://github.com/tidyverse/ggplot2/issues/1859
  geom_text(data          = countries[c(which.min(countries[["birth.rate"]]), which.max(countries[["birth.rate"]])),],
            aes(colour    = birth.rate),
            size          = 14,
            family        = "Helvetica") +

  # Add Poland and Greece
  geom_text(data          = countries[countries[["country"]] %in% c("Greece", "Poland"), ],
            aes(colour    = birth.rate),
            size          = 3,
            family        = "Helvetica") +

  labs(
    title     = "Countries with the highest and lowest birth rate in the world",
    subtitle  = "12.12.19: Statistical inference with missing values + ggplot2 workshops",
    caption   = "bit.ly/waRsaw_meetup",
    x         = "Death rate",
    y         = "Birth rate")   +

  theme(text             = element_text(family = 'Helvetica',color = "#4A637B",face = "bold")
        ,axis.title.x    = element_text(size  = 12)
        ,axis.title.y    = element_text(size  = 12)
        ,axis.text.x     = element_text(color = "#4A637B")
        ,axis.text.y     = element_text(color = "#4A637B")
        ,plot.title      = element_text(size  = 14,hjust = 0 )
        ,plot.subtitle   = element_text(size  =  7,hjust = 1,face = "italic")
        ,plot.caption    = element_text(size  =  7)
        ,legend.position = "none")


plotly::ggplotly(ggplt, tooltip = c("x","y","country"))
