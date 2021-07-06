
##################################################
## general
##################################################

## set seed for reproducible jitter
set.seed(312) 

## load packages
library(tidyverse) # data wrangling
# library(RColorBrewer) # select colours # no longer required as col hex codes known
library(maps) # outline of UK
library(ggrepel) # repelling labels in ggplot 

##################################################
## get coordinates of institutions
##################################################

## load locations files

## ## option 1:
## ## download file from github to e.g. Desktop and load 
## ## from: https://gist.github.com/deparkes/3b100ddb7068f82c1262b2236a153e01
## locations <- 
  
##   read_csv("./uk_universities_locations.csv") %>%
##   rename(name = Name) # lowercase column header

## option 2:
## load file directly from github
locations <- 
  
  read_csv(file = 
             "https://gist.githubusercontent.com/deparkes/3b100ddb7068f82c1262b2236a153e01/raw/3f4717a25df7f748e4e500a572d94e0e17aebecd/uk_universities_locations.csv") %>%
  rename(name = Name) # lowercase column header

## create object
## this will allow us to select institutions named e.g. University of Bristol
univ <- "University of"

## create list of affiliated institutions
list_institutions <-

    c("Babraham", # FIXME not in locations datafile! added manually below
      "Cardiff University",
      "Keele",
      "King's College London", # added from website
      paste(univ, "Newcastle"),
      "Oxford Brookes",
      "Royal Veterinary",
      "University College London",
      "Aberdeen",
      paste(univ, "Bristol"),
      paste(univ, "Edinburgh"),
      paste(univ, "Glasgow"),
      paste(univ, "Liverpool"),
      paste(univ, "Manchester"), # added 2021-05-24
      "Reading",
      paste(univ, "Sheffield"),
      paste(univ, "Surrey"),
      paste(univ, "Southampton"),
      "Wolverhampton"
      )

## check number of institutions
length(list_institutions)

## create list of local networks
list_networks <- 

    c("Alan Turing", # FIXME not in locations datafile! added manually below
      "Aston",
      "Birkbeck",
      "Bournemouth University",
      "Brunel",
      "Cardiff University",
      "Imperial",
      "Keele", # Added # missing www 
      "King's College London",
      "Kingston",
      "Lancaster",
      "London School of Economics",
      "London School of Hygiene", # Added 2020-10-28
      "Loughborough",
      "Manchester Metropolitan", # Added 2020-10-28
      # paste(univ, "Newcastle"), # Added from old www # not in 2021-05-21 file
      "Northumbria",
      "Oxford Brookes", # Added 2020-10-28
      "Queen Mary", # Added 2021-05-24
      "Queen's", # Added 2020-10-28
      "Royal Holloway",
      "Royal Veterinary", # Added
      "Swansea",
      "University College London", # Added # missing www
      "Aberdeen",
      paste(univ, "Bath"),
      paste(univ, "Birmingham"), # Added 2021-05-24
      paste(univ, "Bristol"), # in 2021-05-21 file
      "Brighton", # Added 2021-05-24
      "Cambridge",
      "Chester",
      "Durham", # missing www
      "East Anglia", # Added 2021-05-24 
      paste(univ, "Edinburgh"),
      "Essex",
      "Exeter",
      paste(univ, "Glasgow"), # Added 2020-10-28
      paste(univ, "Greenwich"), # Added 2020-10-28
      paste(univ, "Hertfordshire"), # Added 2020-10-28
      "Hull",
      "Kent",
      paste(univ, "Leeds"),
      paste(univ, "Leicester"), # Added 2020-10-28
      paste(univ, "Liverpool"),
      paste(univ, "Manchester"),
      paste(univ, "Oxford"),
      paste(univ, "Plymouth"),
      paste(univ, "Portsmouth"),
      "Reading",
      paste(univ, "Sheffield"), # Added
      paste(univ, "Southampton"), # Added 2020-10-28
      paste(univ, "Surrey"), # Added from www
      paste(univ, "Sussex"), # Added 2021-07-06
      "West of England",
      "Westminster",
      "Wolverhampton",
      paste(univ, "York"),
      "Warwick"
      )

## check number of local networks
length(list_networks)

## manual coordinates for Alan Turing Institute
## based on google maps
alan_turing <- 

    tibble(name = "Alan Turing Institute",
           lat = 51.5299658,
           lon = -0.1298621)

## based on google search 2021-05-24
babraham <- 

    tibble(name = "Babraham Institute",
           lat = 52.1325,
           lon = 0.2057)


## terms to be removed from labels
remove_univ_terms <-
  
  c("The University of ", 
    "University of ", 
    " University", 
    "The ")

## create tibble of institutions
institutions <-

    locations %>%
    filter(str_detect(name, paste(list_institutions, collapse = "|"))) %>%
    bind_rows(babraham) %>%
    arrange(name) %>%
    ## clean names for pretty labels # FIXME this is a bit long-winded
    mutate(name = str_replace(name, "University College London", "UCL"),
           name = str_replace(name, "Royal Veterinary College", "RVC"),
           name = str_replace(name, "King's College London", "KCL"),
           name = str_remove(name, "-upon-Tyne"),
           name = str_remove(name, str_c(remove_univ_terms, collapse = "|")),
           name = str_remove(name, " Institute")
           )

## create tibble of networks
networks <-

    locations %>%
    filter(str_detect(name, str_c(list_networks, collapse = "|"))) %>%
    bind_rows(alan_turing) %>%
    ## bind_rows(institutions) %>%
    arrange(name) %>%
    mutate(name = str_replace(name, "University College London", "UCL"),
           name = str_replace(name, "Royal Veterinary College", "RVC"),
           name = str_remove(name, "-upon-Tyne"),
           name = str_remove(name, str_c(remove_univ_terms, collapse = "|"))
           )

## create tibble of networks excluding institutions
## to prevent locations appearing twice as e.g. both network and member institutions
networks_without_institutions <-

    locations %>%
    filter(str_detect(name, paste(list_networks, collapse = "|"))) %>%
    bind_rows(alan_turing) %>%
    bind_rows(babraham) %>%
    arrange(name)    

##################################################
## get colours
##################################################

## colours based on their similarity to the ukrn logo 
## at: https://www.bristol.ac.uk/psychology/research/ukrn/

## colours taken from RColorBrewer
## following: http://www.sthda.com/english/wiki/colors-in-r

## purple colour
# ukrn_pur <- brewer.pal(n = 8, name = "BuPu")[6]
ukrn_pur <- "#4e5087" # from JT 2020-05-21

## green colour
# ukrn_gre <- brewer.pal(n = 8, name = "BuGn")[4]

## pink colour
# ukrn_pink <- brewer.pal(n = 8, name = "Dark2")[4]
ukrn_pink <- "#ec008c" # from JT 2020-05-21

##################################################
## create maps
##################################################

## get outline of UK from 'maps'
UK <-

    map_data("world") %>%
    filter(region == "UK") %>%
    rename(lon = long)

## create base map
uk_base_map <- 

    ggplot() +
    geom_polygon(data = UK,
                 aes(x = lon,
                     y = lat,
                     group = group),
                 ## colour = "grey70", # for outline
                 fill = "grey",
                 alpha = 0.5) +
    theme_void() +
    ylim(50, 58.75) + # remove Shetland
    coord_map()

## draw map of institutions with label and networks without label
map_institutions_networks <- 

    uk_base_map +
    geom_point(data = institutions,
               aes(x = lon,
                   y = lat),
               colour = ukrn_pur,
               shape = 19,
               fill = ukrn_pur,
               size = 10) + 
    geom_point(data = networks_without_institutions,
                aes(x = lon,
                    y = lat),
               colour = ukrn_pink, 
               shape = 19,
               fill = ukrn_pink,
               size = 5) +
    # comment below, and preceding plus symbol, to remove labels 
    geom_label_repel(data = institutions,
               aes(x = lon,
                   y = lat,
                   label = name),
               colour = "black",
               fill = "white",
               label.size = 0,
               size = 5,
               alpha = 0.9,
               nudge_y = -0.25
               # vjust = -0.75
               ) +
    # comment below, and preceding plus symbol, to remove caption with date 
    labs(title = paste(nrow(networks), "local networks", "and", nrow(institutions), "institutions"),
         caption = Sys.Date())

## ## if you want to add UKRN logo: https://patchwork.data-imaginist.com/reference/inset_element.html
## ## save image to folder
## ## load package
## library(patchwork)
## ## load image
## logo <- png::readPNG("./UKRN_LOGO10.png", native = TRUE)
## ## add to top right
## map_institutions_networks <- 

##     map_institutions_networks +
##     inset_element(logo, 0.8, 0.8, 1, 1, align_to = 'full')

## save map with transparent background
map_institutions_networks %>%
    ggsave(filename = paste(Sys.Date(), "map-ukrn-combined-transparent.png", sep = "_"),  
           bg = "transparent")

## draw map of institutions with label
map_institutions <- 

    uk_base_map +
    geom_point(data = institutions,
               aes(x = lon,
                   y = lat),
               colour = ukrn_pur,
               shape = 19,
               fill = ukrn_pur,
               size = 10) +
    geom_label_repel(data = institutions,
               aes(x = lon,
                   y = lat,
                   label = name),
               colour = "black",
               fill = "white",
               label.size = 0,
               size = 8,
               alpha = 0.9,
               ## nudge_y = -0.5
               # vjust = -0.75
               )##  +
    ## # comment below, and preceding plus symbol, to remove caption with date 
    ## labs(title = paste(nrow(institutions), "institutions"),
    ##      caption = Sys.Date())

## save map with transparent background
map_institutions %>%
    ggsave(filename = paste(Sys.Date(), "map-ukrn-institutions-transparent.png", sep = "_"),  
           bg = "transparent")

## draw map of networks without label
map_networks <- 

    uk_base_map +
    geom_point(data = networks,
                aes(x = lon,
                    y = lat),
               colour = ukrn_pink,
               shape = 19,
               fill = ukrn_pink,
               size = 5)##  +
    ## # comment below, and preceding plus symbol, to remove caption with date 
    ## ## labs(title = paste(nrow(networks), "local networks")## ,
    ## ##      ## caption = Sys.Date()
    ## ##      ) ## +
    ## ## ## add label for single network (e.g. Oxford)
    ## geom_label_repel(data =
    ##                      mutate(filter(networks, name == "Oxford"), name = "Oxford"),
    ##            aes(x = lon,
    ##                y = lat,
    ##                label = name),
    ##            colour = "black",
    ##            fill = "white",
    ##            label.size = 0,
    ##            size = 10,
    ##            alpha = 0.9,
    ##            vjust = -0.75
    ##            )

## save map with transparent background
map_networks %>%
    ggsave(filename = paste(Sys.Date(), "map-ukrn-networks-transparent.png", sep = "_"),  
           bg = "transparent")
