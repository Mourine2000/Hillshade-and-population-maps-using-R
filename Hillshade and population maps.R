#libraries
install.packages('BiocManager')
library(pacman)
pacman::p_load(
  terra, elevatr,tidyverse,
  ggnewscale,ggspatial,
  geodata,sf,scales
)

find.package("ggspatial")
#1)Luxembourg boundary download
country_sf <- geodata::gadm(
  country = 'LUX', level=0,
  path= tempdir()
  
) |> sf::st_as_sf()

country_vect<- terra::vect(country_sf)

#2)world 100m population count 2020
pop_100m <- terra::rast("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/LUX/lux_ppp_2020_constrained.tif")
terra::plot(pop_100m)

#3)DEM & hillshade on finer(-30m)grid

dem <- elevatr::get_elev_raster(
  country_sf,
  z=10,clip= 'locations'
)

dem_country <- terra::rast(dem) |>
  terra::crop(country_vect) |>
  terra::mask(country_vect)

#exaggerate the DEM
dem_exaggerated <- dem_country * 1.3

#shaded relief
slope <- terra::terrain(
  dem_exaggerated,
  v='slope', unit='radians'
  )

aspect <- terra::terrain(
  dem_exaggerated,v='aspect',
  unit='radians'
  )

hillshade_raw <- terra::shade(
  slope, aspect, angle= 40, direction=225
)

#resample pop onto the hill-shade grid
#then blank releif where pop exists

pop_on_hillshade <- terra:: resample(
  pop_100m, hillshade_raw, 
  method='bilinear'
)

hillshade_no_pop <- terra::ifel(
  is.na(pop_on_hillshade),hillshade_raw,
  NA
)

#5)data frames for ggplot
#(NA rows dropped automatically)

hillshade_df <- terra::as.data.frame(
  hillshade_no_pop, xy= TRUE,
  na.rm= TRUE
)

head(hillshade_df)

pop_df <- terra::as.data.frame(
  pop_on_hillshade,
  xy=TRUE, na.rm= TRUE
)

tail(pop_df)

pop_df$lux_ppp_2020_constrained[pop_df$lux_ppp_2020_constrained <=0.1]<- NA
summary(pop_df$lux_ppp_2020_constrained)

#6)plot
##legend breaks once so we can reuse them
brks <- c(1,5,20,100)
q <- ggplot() +
  geom_raster(data= hillshade_df,aes(
    x,y,fill=lux_ppp_2020_constrained
    ))+ 
  scale_fill_gradient(
    low='grey70', high='grey10',
    guide='none'
    ) +
  #b)population layer
  ggnewscale:: new_scale_fill() +
  geom_raster(data=pop_df,aes(
    x,y,fill=lux_ppp_2020_constrained
  )) +
  
  scale_fill_viridis_c(
    name= "Population",
    option='plasma',
    alpha=1, begin= .2, end=1,
    trans='log10',breaks= brks,
    labels= scales::comma,
    guide= guide_colourbar(
      title.position= "top",
      barheight= unit(30,"mm"),
      barwidth= unit(5, "mm"),
      ticks.color ="grey10",
      frame.color='grey10'
    )
  )+

  #c)country boundaries
  geom_sf(
    data= country_sf, fill=NA,
    color='black',
    linewidth= .25
  )+
  
  #d)cartographic extras
  ggspatial::annotation_north_arrow(
    location= 'tr', which_north= 'true',
    height= unit(10,'mm'),
    width= unit(10,'mm'),
    style= north_arrow_orienteering
  ) +
  annotation_scale(
    location='bl', pad_y=unit(2, 'mm'),
    height= unit(2,'mm')
  ) +
  
  coord_sf(expand = FALSE) +
  #e)typography and layout
  
  labs(
  title='Luxembourg . Population (2020)',
  subtitle= 'WorldPop 100m constrained grid',
  caption="Data:WorldPop . SRTM via elevtr |Mourine Lagat"
  
#a)hill-shade

  ) +
  theme(
    plot.title= element_text(
      size=16, face='bold',hjust= .02
    ),
    plot.subtitle = element_text(
      size = 14, hjust = 0.02
      ),
    plot.caption= element_text(
      hjust= .5
    ),
    legend.title = element_text(
      size=12
    ),
    legend.text = element_text(
      size=11
    ),
    legend.margin= margin(
      t=0, r=5, b=0, l=3
    ),
    plot.margin= margin(
      t=5, r=5, b=5, l=5
    )
  ) +
  theme_void()

q

#save
ggsave(
  "C:\\Users\\user\\MJ_2024\\R Mapping\\luxembourg_populaion_relief.png",
  width = 8, height= 6,
  dpi= 600,
  bg='white',q
)