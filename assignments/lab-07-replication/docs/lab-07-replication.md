Lab-07 Replication
================
Branson Fox, BA and Christopher Prener, PhD
(March 15, 2021)

## Introduction

This notebook replicates the results for lab-07 - Density of Public
Schools in Missouri.

## Dependencies

This notebook requires the following packages to manipulate our data and
create maps.

``` r
# tidyverse packages
library(dplyr)   # methods for data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2) # making plots/maps

# other packages
library(sf)      # tools for spatial 
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.4, PROJ 6.3.1

``` r
library(here)    # file path management
```

    ## here() starts at /Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication

``` r
library(mapview) # interactive exploratory map
```

    ## GDAL version >= 3.1.0 | setting mapviewOptions(fgb = TRUE)

``` r
library(viridis) # accessible color palettes
```

    ## Loading required package: viridisLite

## Load Data

This notebook requires the K-12 Schools data and the County Population
data:

``` r
schools <- st_read(here("data","MO_EDU_PublicK12","MO_EDU_PublicK12.shp"))
```

    ## Reading layer `MO_EDU_PublicK12' from data source `/Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication/data/MO_EDU_PublicK12/MO_EDU_PublicK12.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 2381 features and 4 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: -95.51797 ymin: 36.04236 xmax: -89.33608 ymax: 40.55136
    ## geographic CRS: GRS 1980(IUGG, 1980)

``` r
counties <- st_read(here("data","MO_DEMOS_CountyPop","MO_DEMOS_CountyPop.shp"))
```

    ## Reading layer `MO_DEMOS_CountyPop' from data source `/Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication/data/MO_DEMOS_CountyPop/MO_DEMOS_CountyPop.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 115 features and 4 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -95.7747 ymin: 35.99568 xmax: -89.09897 ymax: 40.61364
    ## geographic CRS: NAD83

## Part 1: Data Preparation

### Question 1

We’ll begin by checking that our data are in the same coordinate system.

``` r
st_crs(schools)
```

    ## Coordinate Reference System:
    ##   User input: GRS 1980(IUGG, 1980) 
    ##   wkt:
    ## GEOGCRS["GRS 1980(IUGG, 1980)",
    ##     DATUM["D_unknown",
    ##         ELLIPSOID["GRS80",6378137,298.257222101,
    ##             LENGTHUNIT["metre",1,
    ##                 ID["EPSG",9001]]]],
    ##     PRIMEM["Greenwich",0,
    ##         ANGLEUNIT["Degree",0.0174532925199433]],
    ##     CS[ellipsoidal,2],
    ##         AXIS["longitude",east,
    ##             ORDER[1],
    ##             ANGLEUNIT["Degree",0.0174532925199433]],
    ##         AXIS["latitude",north,
    ##             ORDER[2],
    ##             ANGLEUNIT["Degree",0.0174532925199433]]]

``` r
st_crs(counties)
```

    ## Coordinate Reference System:
    ##   User input: NAD83 
    ##   wkt:
    ## GEOGCRS["NAD83",
    ##     DATUM["North American Datum 1983",
    ##         ELLIPSOID["GRS 1980",6378137,298.257222101,
    ##             LENGTHUNIT["metre",1]]],
    ##     PRIMEM["Greenwich",0,
    ##         ANGLEUNIT["degree",0.0174532925199433]],
    ##     CS[ellipsoidal,2],
    ##         AXIS["latitude",north,
    ##             ORDER[1],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##         AXIS["longitude",east,
    ##             ORDER[2],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##     ID["EPSG",4269]]

Our data are in the same coordinate system, but they need to be
projected to something appropriate for statewide Missouri data. We’ll
use UTM 15N, epsg 26915, because it captures most of the extent of
Missouri.

``` r
schools <- st_transform(schools, crs = 26915)
counties <- st_transform(counties, crs = 26915)
```

Next, we need to create a spatial join that identifies the county each
school is located in. We pipe into a select function in order to remove
the extra county variables.

``` r
schools_county <- st_intersection(schools, counties) %>%
  select(id, school, address, city, NAMELSAD)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

Next, we want to create a subset of schools in the City of Saint Louis.

``` r
stlSchools <- filter(schools_county, NAMELSAD == "St. Louis city")
```

Before exporting this we’ll preview the data we subset.

``` r
mapview(stlSchools)
```

![](lab-07-replication_files/figure-gfm/preview%20stl-1.png)<!-- -->

And since the data look good, we’ll save this to a new shapefile.

``` r
st_write(stlSchools, dsn = here("data","STL_EDU_PublicK12","STL_EDU_PublicK12.shp"), delete_dsn = TRUE)
```

    ## Deleting source `/Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication/data/STL_EDU_PublicK12/STL_EDU_PublicK12.shp' using driver `ESRI Shapefile'
    ## Writing layer `STL_EDU_PublicK12' to data source `/Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication/data/STL_EDU_PublicK12/STL_EDU_PublicK12.shp' using driver `ESRI Shapefile'
    ## Writing 113 features with 5 fields and geometry type Point.

We’ll do another spatial join, aggregating schools in all counties in
Missouri and combine them with county data.

``` r
# aggregate
schools_county %>%
  group_by(NAMELSAD) %>%
  summarise(schools = n()) -> schoolsByCounty

# remove geometry
st_geometry(schoolsByCounty) <- NULL

# join data
schoolsByCounty <- left_join(counties, schoolsByCounty, by = "NAMELSAD")
```

And now it’s ready to be written to a shapefile.

``` r
st_write(schoolsByCounty, dsn = here("data", "MO_EDU_County", "MO_EDU_County.shp"), delete_dsn = TRUE)
```

    ## Deleting source `/Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication/data/MO_EDU_County/MO_EDU_County.shp' using driver `ESRI Shapefile'
    ## Writing layer `MO_EDU_County' to data source `/Users/chris/GitHub/slu-soc5650/content/module-3-intersect/assignments/lab-07-replication/data/MO_EDU_County/MO_EDU_County.shp' using driver `ESRI Shapefile'
    ## Writing 115 features with 5 fields and geometry type Polygon.

## Part 2: Mapping the Density of Schools in Missouri

### Question 2

We’ll map the density of schools per square kilometer in each county.
First we need to calculate this number.

``` r
schoolsByCounty <- mutate(schoolsByCounty, school_density = schools/SQKM)
```

Then to create a choropleth map:

``` r
ggplot() +
  geom_sf(data = schoolsByCounty, mapping = aes(fill = school_density)) +
  theme_minimal() +
  labs(
    title = "Density of Public Schools By Missouri County",
    caption = "Map by Branson Fox, BA and Christopher Prener, PhD"
  ) +
  scale_fill_viridis(direction = -1, name = "Schools per\nSquare Kilometer") -> gg_density_km

gg_density_km
```

![](lab-07-replication_files/figure-gfm/ggplot1-1.png)<!-- -->

And to save it:

``` r
ggsave(here("results", "schoolsDensityKM.png"), plot = gg_density_km, dpi = 500)
```

    ## Saving 7 x 5 in image

### Question 3

We’ll map the density of schools per 1,000 residents in each county.
First we need to calculate this number.

``` r
schoolsByCounty <- mutate(schoolsByCounty, school_per_1k = schools/TOTALPOP * 1000)
```

Then to create a choropleth map:

``` r
ggplot() +
  geom_sf(data = schoolsByCounty, mapping = aes(fill = school_per_1k)) +
  theme_minimal() +
  labs(
    title = "Density of Schools By Missouri County",
    caption = "Map by Branson Fox, BA and Christopher Prener, PhD"
  ) +
  scale_fill_viridis(direction = -1, name = "Schools per\n1,000 Residents") -> gg_density_1k

gg_density_1k
```

![](lab-07-replication_files/figure-gfm/ggplot2-1.png)<!-- -->

And to save it:

``` r
ggsave(here("results", "schoolsDensity1k.png"), plot = gg_density_1k, dpi = 500)
```

    ## Saving 7 x 5 in image
