library(ggplot2)
library(dplyr)
library(ggchicklet)

movies <- read.csv("exercises/data/movie_metadata.csv")

movies_actors <- movies%>%
    group_by(aktor=actor_1_name)%>%
    summarise(ilosc=n())%>%
    arrange(desc(ilosc))%>%
    head(15)

#ggplt <- 
            ggplot(movies_actors,aes(x     =  reorder(aktor,ilosc), 
                                     y     =  ilosc,
                                     fill  =  ilosc)) +
    
            geom_chicklet(size   = 0.30,
                          width   = 0.85,
                          radius = unit(0.30, "cm"))  +

            theme(text              = element_text(           color = "#4A637B", face = "bold", family = 'Helvetica')
                  ,plot.caption     = element_text(size =  9, color = "#8d99ae", face = "plain"                     ) 
                  ,plot.title       = element_text(size = 18, color = "#2b2d42", face = "bold", hjust=0.15          )
                  ,axis.text.y      = element_text(size = 10, color = "#8d99ae", face = "bold", hjust=1.1           )
                  ,axis.title.x     = element_text(size = 14 ,                                  hjust = 0.15        )
                  ,axis.text.x      = element_blank()
                  ,axis.title.y     = element_blank()
                  ,axis.ticks.x     = element_blank()
                  ,axis.ticks.y     = element_blank()
                  ,plot.margin      = unit(c(1,1,1,1),"cm")
                  ,panel.background = element_blank()
                  ,legend.position  = "none"
                  ,aspect.ratio     = 0.75) + 

            scale_fill_gradient2(midpoint  = mean(movies_actors$ilosc) - 2,
                                  low      = "#4974a5",
                                  mid      = "#4A637B",
                                  high     = "#f35f71",
                                  space    = "Lab" ) +
                
            scale_size_continuous(range = c(3,5)) +
            
            coord_flip() +
                
            geom_text(aes(label = ilosc, 
                          size  = ilosc),
                      color     = "white",
                      nudge_y   = -2.5,
                      family    = 'Helvetica') +
            
            labs(title   = " Actors with most starring roles\n",
                 caption = "\nsource: Spotkania Entuzjastów R-Warsaw RUG Meetup",
                     y   = "\nNumber of starring roles")



