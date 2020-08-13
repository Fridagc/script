##### SESION 1 ####
# COORDENADAS Y PROYECCIONES
# IMPORTACIÓN DE ESRI FILES
# ¿COMO HACER UN MAPA USANDO DIFERENTES PAQUETES
# VAMOS A HACER UN MAPA DE CALOR
# EXPORTAREMOS NUESTRO PROPIO SHP

rm(list = ls())

dir.create("inp/")
dir.create("inp/sesion_1")
dir.create("out/sesion_1")
dir.create("graficas/sesion_1")

##### COORDENADAS Y PROYECCIONES ####
# Un mapa es una representación bidimensional de la tierra 
# En cartografía depende la proyección, la dimensión de los territorios, 
# distancias.
# En R hablaremos de CRS (Sistemas de Referencia de Coordenadas) 
# ¡Necesitas saberlo para lo que vayas a hacer!
## tip: usar en el nombre de tus objetos el sistema de coordenadas 
# Los CRS están compuestos por dos listas ESPG y proj4string 

##### PAQUETES PARA ABRIR DATOS GEO ####
# Past: {sp} + {rgdal} + {rgeos} + {raster}
# Current packages:
#         {sf} : reading and manipulation of vector type data
# {tidyverse} style

# IMPORTACIÓN DE ESRI FILES
install.packages("nombre_del_pqt", dependencies = T) # solo la primera vez
library(sf)
library(tidyverse)
library(raster)
library(spData)
library(viridis)
library(rgdal)
library(tmap)

file <- "inp/sesion_1/gadm36_MEX_shp/gadm36_MEX_1.shp"
mapa <- read_sf(file, quiet = T) # O st_read de sf
# readOGR(dsn = "inp/sesion_1/gadm36_MEX_shp/gadm36_MEX_1.shp") 

class(mapa)
glimpse(mapa)  # str y summary
rm(file)
plot(mapa[,"NAME_1"])
head(mapa)
mapa$geometry[1]

rm(file)

#### SF ####
# read_sf() : para leer
# write_sf() : para escribir
# st_crs() : saber proyeccion
# st_transform() # transformar proyeccion
# st_as_sf() # leer texto
# st_bbox() # saber extension espacial

st_crs(mapa)
st_bbox(mapa)

# mapa de mexico
ggplot(mapa)+
        geom_sf(aes(fill=NAME_1), color="White") + #que se coloren los estados
        guides(fill=F) + # que no aparezcan las etiquetas
        geom_sf_label(aes(label=NAME_1), size=3, fill=NA, line=NA) + #las etiquetas se ven oscuras, se cambia geom_sf_text a geom_sf_label
        coord_sf(datum = NA) + # quita las coordenadas
        theme(line=element_blank(),
              axis.text=element_blank(),
              axis.title=element_blank(),
              panel.background = element_blank()) +
        viridis::scale_fill_viridis(discrete = T)

ggsave("graficas/sesion_1/hola.jpg", height = 10, width = 15)

# mapa de mx por poblacion
poblacion <- read_csv("inp/sesion_1/poblacion_est.csv", 
                      locale = locale(encoding = "Latin1"))
poblacion <- poblacion %>% 
        mutate(Total_1 = str_replace_all(Total, " ", ""),
               Total_1 = as.numeric(Total_1))

options(scipen=999)
joined <- left_join(mapa, poblacion, by= "NAME_1")
sum(is.na(joined$Total))

ggplot(joined)+
        geom_sf(aes(fill=as.numeric(Total_1))) +
        scale_fill_gradient(low = "yellow", high = "red")

joined %>% 
        filter(NAME_1=="México" | NAME_1 == "Baja California") %>% 
        ggplot()+
        geom_sf(data=joined, fill="white") +
        geom_sf(fill="blue")

# mapa de mx por municipio
file2 <- "inp/sesion_1/gadm36_MEX_shp/gadm36_MEX_2.shp"
mapa_mun <- st_read(file2, quiet=T)
rm(file2)

glimpse(mapa_mun)
set.seed(110590)

numero_mun <- tibble(GID_2= mapa_mun$GID_2,
                     n=runif(1854))

numero_mun

joined_mun <- left_join(mapa_mun, numero_mun)
glimpse(joined_mun)
# str(joined_mun)

joined_mun %>% 
        filter(NAME_1 == "Veracruz" | NAME_1 == "Zacatecas") %>% 
        ggplot() +
        geom_sf(aes(fill=n), size=0.5, color="white") +
        geom_sf_label(data = joined %>% 
                              filter(NAME_1 == "Veracruz" | NAME_1 == "Zacatecas"), 
                      aes(label=NAME_1)) + 
        coord_sf(datum = NA) +
        theme(line=element_blank(),
              axis.text=element_blank(),
              axis.title=element_blank(),
              panel.background = element_blank()) + 
        labs(fill = "hola") +
        viridis::scale_fill_viridis(discrete = F, option = "magma")

# mapa del mundo usando datos de SPData
mundo <- world
plot(mundo$lifeExp)

cafe <- coffee_data
cafe_mundo <- left_join(mundo,cafe)
glimpse(cafe_mundo)

ggplot() +
        geom_sf(data=cafe_mundo, aes(fill=coffee_production_2017)) +
        scale_fill_distiller(palette ="Spectral", na.value="white") #le puedo poner, despuès de cafe_mundo " %>%  na.omit(), y aparece todo

rm(cafe, cafe_mundo, joined, joined_mun, mundo, poblacion, mapa, mapa_mun, numero_mun)

#### SP (RGDAL) ####
mapa <- readOGR("inp/sesion_1/gadm36_MEX_shp/gadm36_MEX_1.shp", stringsAsFactors = F)
proj4string(mapa)
mapa <- sp::spTransform(mapa, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

mapa@data$id <- 1:32

mapaF <- fortify(mapa, region = "id") # primero sin region
length(unique(mapaF$id))

casillas <- read_csv("inp/sesion_1/casillas_basicas.csv")
casillas <- casillas %>% filter(long < 0)
casillas <- filter(casillas, !is.na(long))

ggplot() +
        geom_polygon(data = mapaF, aes(x = long, y = lat, group = group), 
                     color = "black", fill = NA) +
        geom_point(casillas, mapping = aes(long, lat, color = as.character(ESTADOID)), 
                   size = 0.8, alpha = 0.4) +
        theme(line=element_blank(),
              axis.text=element_blank(),
              axis.title=element_blank(),
              panel.background = element_blank()) +
        viridis::scale_fill_viridis(discrete = F) +
        labs(title="Mapa de México usando SP",
             subtitle="Casillas extraordinarias")

# ¿cómo guardar un archivo .shp 
casillas_point <- SpatialPointsDataFrame(casillas[,c("long", "lat")],
                                         casillas,
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

writeOGR(casillas_point, ".", paste("out/sesion_1/casillas/int_2"), driver="ESRI Shapefile")

rm(mapa, mapaF, casillas, casillas_point)

#### TM usando tmap ####
mapa <- read_sf("inp/sesion_1/gadm36_MEX_shp/gadm36_MEX_1.shp")
casillas <- read_sf("out/sesion_1/casillas/casillas_point.shp")

st_crs(mapa)
st_crs(casillas)

# ¿que tiene tm?
# tm_shape(): qué dibujar
# tm_symbols()/tm_lines()/tm_polygons()/tm_text(): como dibujar
# tm_scale_bar(), tm_compass(): posición de la info
# tm_layout(): info general

tm_mapa <- tm_shape(mapa) +
        tm_polygons() +
        tm_shape(casillas) +
        tm_dots(size = 0.1, col = "blue") + 
        tm_scale_bar() +
        tm_compass(position = c("left", "bottom")) +
        tm_layout(title = "Mapa hecho con TM",
                  inner.margins = c(0.1, 0.3, 0.1, 0),
                  legend.position = c("left", "center")
        )

tmap_save(tm_mapa, "graficas/sesion_1/hola_soy_tm.jpg")

