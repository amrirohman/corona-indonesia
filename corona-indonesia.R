'https://kawalcorona.com/api/'

library(httr)
library(jsonlite)
library(tidyverse)
library(plotly)
library(janitor)
library(geojsonio)
library(leaflet)

#==============================================
# loading data corona provinsi
base2 <- "https://api.kawalcorona.com/indonesia/provinsi"
prov.corona.df <- 
  GET(base2) %>%
  content(., "text") %>%
  fromJSON(., flatten = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.integer, as.numeric) %>%
  rename_all(., ~str_remove(.x, "attributes\\.")) %>%
  clean_names() %>%
  rename("confirmed" = "kasus_posi",
         "recovered" = "kasus_semb",
         "deaths" = "kasus_meni")
class(prov.corona.df)

# geojson map
prov.map <- geojson_read("C:/Users/User/Documents/coronavirus/provinsi_indonesia.json",  what = "sp")
prov.map <- sf::st_as_sf(prov.map) %>%
  select(NAME_1, geometry) %>%
  rename("provinsi" = NAME_1)
glimpse(prov.map)
class(prov.map)

# data merge
prov.corona.map <- merge(prov.map, prov.corona.df, id = "provinsi")
glimpse(prov.corona.map)
class(prov.corona.map)

# loading data corona global
base3 <- "https://api.kawalcorona.com/"
global.corona.df <- 
  GET(base3) %>%
  content(., "text") %>%
  fromJSON(., flatten = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.integer, as.numeric) %>%
  rename_all(., ~str_remove(.x, "attributes\\.")) %>%
  clean_names()

# data corona indonesia pnpb
base4 <- "https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
pnpb.corona.df <- 
  GET(base4) %>%
  content(., "text") %>%
  fromJSON(., flatten = TRUE) %>%
  .$features %>% 
  na.omit() %>%
  mutate_all(as.numeric) %>%
  rename_all(., ~str_remove(.x, "attributes\\.")) %>%
  clean_names() %>%
  rename("confirmed" = "jumlah_kasus_kumulatif",
         "recovered" = "jumlah_pasien_sembuh",
         "deaths" = "jumlah_pasien_meninggal",
         "confirmed_per_day" = "jumlah_kasus_baru_per_hari",
         "recovered_per_day" = "jumlah_kasus_sembuh_per_hari",
         "deaths_per_day" = "jumlah_kasus_meninggal_per_hari")

sum(pnpb.corona.df$confirmed_per_day)


#========================================================
#ploting timr serie positif
p1 <- pnpb.corona.df %>%
  ggplot() +
  geom_line(aes(x=hari_ke, y=deaths, color="Kematian Kumulatif")) +
  geom_point(aes(x=hari_ke, y=deaths, color="Kematian Kumulatif")) +
  
  geom_line(aes(x=hari_ke, y=confirmed, color="Positif Kumulatif")) +
  geom_point(aes(x=hari_ke, y=confirmed, color="Positif Kumulatif")) +
  
  geom_line(aes(x=hari_ke, y=recovered, color="Sembuh Kumulatif")) +
  geom_point(aes(x=hari_ke, y=recovered, color="Sembuh Kumulatif")) +
  theme(legend.position="bottom")
ggplotly(p1)

#bubble plot

ggplotly(
  prov.corona.df %>%
    ggplot() +
    geom_point(aes(x=deaths, y=confirmed, size=recovered, colour=provinsi)) +
    geom_text(aes(x=deaths, y=confirmed, label= provinsi)) +
    scale_y_log10() +
    scale_x_log10()
)
glimpse(prov.corona.df)

colnames(prov.corona.df)
prov.corona.df %>% glimpse()

pnpb.corona.df %>% glimpse()

pnpb.corona.df %>% 
  select(c("confirmed", "recovered", "deaths", "hari_ke")) %>%
  gather(key = "status", value = "jumlah", -hari_ke) %>%
  ggplot() +  
  geom_line(aes(x=hari_ke, y=jumlah, fill=status))

#==============
 
mybins <- c(1000, 400, 100, 0)
pal <- colorBin("viridis", domain = prov.corona.map$confirmed, bins = mybins)

# Hover Text
mytext <- paste(
  "<strong>", prov.corona.map$provinsi, "</strong>",
  "<br/>", 
  "Positif :", prov.corona.map$confirmed, 
  sep=" ") %>%
  lapply(htmltools::HTML)


map <-
  prov.corona.map %>%
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(confirmed),
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 2,
              opacity = 1,
              color = "black",
              label =  mytext)
map


m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
x <- prov.corona.map$geometry 

prov.corona.map %>%
  ggplot() +
  geom_polygon(aes(fill = confirmed, x = long, y = lat)) +
  theme_void() +
  coord_map()


unique(prov.map$provinsi)
unique(prov.corona.df$provinsi)

prov.corona.df[1, "provinsi"] <- "Jakarta Raya"
prov.corona.df[7, "provinsi"] <- "Yogyakarta"
prov.corona.df[25, "provinsi"] <- "Bangka Belitung"

ggplotly(
  prov.corona.df %>%
  ggplot() +
  geom_histogram(aes(x=confirmed)) + 
  geom_density(aes(x=confirmed)) +
  scale_x_log10()
)

confirmed_color <- "#6b328a"
active_color <- "#1f77b4"
recovered_color <- "#428a32"
death_color <- "#a83b3b"

ggplotly(
  prov.corona.df %>%
  select(c(3:6)) %>%
  gather(key = "status", value = "jumlah_pasien",  -provinsi) %>%
  ggplot(aes(fill=status, x=reorder(provinsi, -jumlah_pasien), y=jumlah_pasien)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual("status", values = c(confirmed = confirmed_color,
                                         recovered = recovered_color,
                                         deaths = death_color))+
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y="Jumlah Pasien", x="Provinsi", caption="Source: Kawal Corona")
)
    
prov.corona.df$provinsi %>% unique()

   

specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
data


#=============================
library(GADMTools)
data("Corsica")
Cantons <- listNames(Corsica, 4)
pop <- floor(runif(length(Cantons), min=15200, max=23500))
DAT <- data.frame(Cantons, pop)

head(DAT)
choropleth(Corsica, DAT,
           adm.join = "Cantons",
           value = "pop",
           breaks = "sd",
           palette="Oranges",
           legend = "Population",
           title="Population Cantons de Corse")

class(Corsica)

#======================================
library(GADMTools)

prov.corona.df[1, "provinsi"] <- "Jakarta Raya"
prov.corona.df[7, "provinsi"] <- "Yogyakarta"
prov.corona.df[28, "provinsi"] <- "Bangka Belitung"

map <- gadm_sf_loadCountries(c("IDN"), level=1, basefile = "./") %>%
  .$sf %>%
  select(NAME_1, geometry) %>%
  rename(provinsi = NAME_1)
data <- prov.corona.df %>%  select(c(3:4))

data$provinsi
map$provinsi %>% sort()
prov.corona.df$provinsi %>% sort()

prov.corona.map <- merge(map, data, id = "provinsi")

prov.corona.map$provinsi

mybins <- c(1500, 400, 100, 0)
pal <- colorBin("viridis", domain = prov.corona.map$confirmed, bins = mybins)

# Hover Text
mytext <- paste(
  "<strong>", prov.corona.map$provinsi, "</strong>",
  "<br/>", 
  "Positif :", prov.corona.map$confirmed, 
  sep=" ") %>%
  lapply(htmltools::HTML)

prov.corona.map %>%
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(confirmed),
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 2,
              opacity = 1,
              color = "black",
              label =  mytext)

prov.map$provinsi %>% sort()

class(prov.map)
prov.corona.df$provinsi

prov.corona.df %>% filter(provinsi == "Sulawesi Utara")

prov.corona.df[1, "provinsi"] <- "Jakarta Raya"
prov.corona.df[7, "provinsi"] <- "Yogyakarta"
prov.corona.df[28, "provinsi"] <- "Bangka Belitung"

