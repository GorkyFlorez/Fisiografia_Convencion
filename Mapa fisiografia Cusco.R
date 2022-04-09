
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
library(colorspace)
Fisiogra = st_read("SHP/Fisio_Cusco.shp")  %>% st_as_sf()
Rio = st_read("SHP/RIOS_CUSCO_geogpsperu_SuyoPomalia_931381206.shp")  %>% st_as_sf()
Fisiogr  <- st_transform(Fisiogra ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rioo  <- st_transform(Rio ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Peru      <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
convencion   <- subset(Peru, NAME_2 == "La Convención")

Fisio_cusco = st_intersection(Fisiogr, convencion)
Rio_Conv    = st_intersection(Rioo, convencion)

hcl_palettes(plot = TRUE)
colores1 <- sequential_hcl(23, palette = "YlOrRd", rev = T)
col_palette <-colorspace::sequential_hcl(n = 28, h = c(0, 90), c = c(80, NA, 30), l = c(30, 90), power = c(0.2, 2), register = "Custom-Palette")

Mapa =ggplot()+
  geom_sf(data= Fisio_cusco, aes(fill= SIMBOLO_))+
  geom_sf(data= Rio_Conv ,fill=NA,color="blue", size=0.1)+
  scale_fill_viridis_d()+

  labs( title = "Mapa de la Fisiografia de la Municipalidad ",
        subtitle = "PROVINCIAL LA CONVENCION",
        caption = "Fuente: https://www.geogpsperu.com/2015/10/mapa-fisiografico-del-peru-onern-online.html",
        fill = "Fisiografia \nSimbologia",
        x="Longitud",y="Latitud") +
  theme_bw() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = gray(.4),
                                        linetype = "dashed", size = 0.4),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        legend.position = c(0.1,0.4),
        plot.title = element_text(size = 16, hjust = 0.5, color = "white", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "white", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "white", family="serif", face = "italic"),
        legend.key.size = unit(0.4, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.7,"cm"), #ancho de cuadrados de referencia 
        legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
        text = element_text(size = 9, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold", color="white"),
        legend.text =element_text(size=8, face = "bold"))+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  coord_sf(xlim = c(-74.5,-71.9271 ), ylim = c(-13.45915, -11.21135),expand = FALSE)+
  guides(fill = guide_legend(nrow = 23, ncol=1))+
  annotate(geom = "text", x = -74.3, y = -11.8, hjust = 0, vjust = 1, 
           label = "Ing. Gorky Florez Castillo ",size = 4, family="serif", color = "white",  fontface="italic")





ggsave(plot = Mapa ,"Mapa/Mapa de La Convencion.png", units = "cm", width = 29,height = 29, dpi = 900)# guardar grafico

