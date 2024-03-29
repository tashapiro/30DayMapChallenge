# 30 Day Map Challenge  

## :world_map: &nbsp; Challenge Summary 

List of map themes by day, taken from the 30DayMapChallenge [home page](https://github.com/tjukanovt/30DayMapChallenge), and content produced per challenge. 

| Day | Theme         | Map                                                            | Data                                                                                                                           |
|:---:|:--------------|:---------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------|
| 1   | Points        | [Manhattan Adventures](manhattan-adventures)                   | [OpenStreetMap](https://www.openstreetmap.org/)                                                                                                                                                                                                |
| 3   | Polygons      | [Virginia Gubernatorial Election](va-governor-election)        | [VA Department of   Elections](https://www.elections.virginia.gov/resultsreports/election-results/)                            |
| 4   | Hexagons      | [Mars Candy Map](candy-hex-map)                                | [Mars Wrigley &   Shipt](https://www.candyindustry.com/articles/89963-whats-your-states-favorite-mars-wrigley-halloween-candy) |
| 5   | OpenStreetMap | [Basel, Switzerland](basel-openstreetmap)                      | [OpenStreetMap](https://www.openstreetmap.org/)                                                                                |
| 6   | Red           | [Beef Consumption](red-beef-map)                               | [OurWorldInData.org](https://ourworldindata.org/grapher/beef-and-buffalo-meat-consumption-per-person)                              |
| 7   | Green         | California Dispensaries                                        | [Department of Cannabis Control CA](https://cannabis.ca.gov/)                                                                  |
| 8   | Blue          | [RGB Flags Around The World](flag-rgb-map)                     | [flagpediat.net](https://flagpedia.net/)                                                                                       |                                                                 |
| 10  | Raster        | [Such Great Heights (Altitude Map of Nepal)](raster-nepal-map) | [SRTM](https://srtm.csi.cgiar.org/)                                                                                            |
| 11  | 3D        | [Washington State Rainfall](3D-washington-map) | [NOAA](https://www.ncdc.noaa.gov/cag/county/mapping)                                                                                           |
| 12  | Population | [Population Growth Rate](population-map) | [OurWorldInData.org](https://ourworldindata.org)                                                                                           |
| 13  | Natural Earth | [Travel Options](natural-earth) | [Natural Earth](https://www.naturalearthdata.com/)                                                                                 |
| 16  | Urban/Rural | [Queens, NY (Year of Construction)](urban-nyc) | [NYC Planning](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page)    
| 19  | Islands | [Staten Island Pizzerias](staten_island) | [OpenStreetMap](https://www.openstreetmap.org/) |
| 25, 26  | Interactive & Choropleth | [USA - Coronary Disease COD](interactive) | [CDC Wonder API](https://https://wonder.cdc.gov/) 

## **Day 1 & 2 - Points & Lines | [Manhattan Adventures](manhattan-adventures)**
This map is a love letter to my better half, Kristen. We spent two years living together in New York City, and over those two years we created a lot of great memories. This map outlines some of our favorite date spots and places. Map created using ggplot and OpenStreetMap (osmdata) library.

![plot](./manhattan-adventures/manhattan_adventures_dark_light.png)


## **Day 3 - Polygons | [Virginia Gubernatorial Election 2017](va-governor-election)**
Throwback to the last Virginia Gubernatorial Election. Election results by county. Data set from the Virgina Department of Elections. Map created using ggplot and R maps library (county map shapes).

![plot](./va-governor-election/va-governor-election.jpeg)


## **Day 4 - Hexagons | [Mars Candy Favorites 2021](candy-hex-map)**
November 4th is National Candy Day. Dedicating this map to all the Mars candy fans. Snickers is the clear favorite country wide!

![plot](./candy-hex-map/favorite_mars_candy.jpeg)

## **Day 5 - OpenStreetMap | [Basel, Switzerland](basel-openstreetmap)**
Used R osmdata library along with ggplot to render a map of my hometown, Basel, Switzerland. 

![plot](./basel-openstreetmap/basel_map_dark_light.png)


## **Day 6 - Red | [World Beef & Buffalo Consumption by Country (2013)](red-beef-map)**


![plot](./red-beef-map/red_beef_map.jpeg)

## **Day 6, 7, 8 - Red, Green, Blue | [RGB Around The World](flag-rgb-map)**
Map explores different red, green, and blue hex colors of flags around the world. Data set created by scraping data from flagpedia.net and analyzing data using colorfindr.

![plot](./flag-rgb-map/world-flag-map.png)

## **Day 10 - Raster | [Such Great Heights (Nepal Altitude Map)](raster-nepal-map)**
Used altitude data from STRM (raster getData alt) to generate an altitude map of Nepal, home of the tallest mountains.

![plot](./raster-nepal-map/nepal_map.jpeg)

## **Day 11 - 3D | [Washington State Rainfall](3D-washington-map)**
First attempt with 3D mapping using #RStats rayshader library. Precipitation data from [NOAA](https://www.ncdc.noaa.gov/cag/county/mapping), county map data from tigris.

![plot](./3D-washington-map/3D-washington-map.png)

## **Day 12 - Population | [Population Growth Rate](population-map)**
Annual population growth rate over time, spans the last 3 decades (1990-2020). Data from OurWorldInData.org.

![plot](./population-map/population_growth.gif)

## **Day 13 - Natural Earth | [Travel Options](natural-earth)**
Map of different modes of transportation in United Kingdom, Germany, and Italy. Data from Natural Earth, accsessed using #RStats rnaturalearth library.

![plot](./natural-earth/travel-map.png)


## **Day 16 - Urban/Rural | [Queens, NY (Year of Construction)](urban-nyc)**
Map of buildings by year built in Queens, New York. Data from NYC Planning.

![plot](./urban-nyc/nyc_urban.jpeg)


## **Day 19 - Islands | [Staten Island Pizzerias](staten_island)**
Map of Staten Island Pizzerias. Data usese OpenStreetMap data from R osmdata package to render the map. Pizzeria data scraped from Google, addressees later used to create geocoordinatee information to overlay points on map.

![plot](./staten_island/staten-island-pizzeria.png)


## **Day 25, 26 - Interactive Choropleth | [USA Coronary Disease COD](interactive)**
Interactive choropleth map exploring death rate in the United States attributed to Coronary Disease. Data provided by the CDC Wonder API. Map by state, ability to drill down to county level for each state. Interactive map available [here](https://rpubs.com/tshapiro/coronarydisease).


<img src="/interactive/coronary_map.gif" width="100%"/>
