
### Part 1 : creating plots with rBokeh

# rBokeh command reference: https://rdrr.io/cran/rbokeh/man/

# install rBokeh package
#install.packages("rbokeh")

library(rbokeh)

# use the built-in "iris" dataset to create a simple interactive plot
iris_plot <- figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))

# display the plot
iris_plot

# save the plot as HTML file in /home/[CNet ID]/public_html
library(htmlwidgets)

filename = "/home/jcarlsen/public_html/bokeh/bokeh_iris.html"
saveWidget(iris_plot, file=filename)

###

# we can also use the "cars" dataset to add in a linear regression,
# and a "lowess" regression (this is a "locally weighted regression")

lin_reg <- lm(dist ~ speed, data = cars)

cars_plot <- figure(width = 600, height = 600) %>%
  ly_points(cars, hover = cars) %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(lin_reg, type = 2, legend = "lm")

cars_plot

###

# now let's make a histogram with a density curve (non-interactive)
# based on the "eruptions" built-in dataset (eruptions of the geyser Old Faithful)

eruptions_hist <- figure(width = 600, height = 400) %>%
  ly_hist(eruptions, data = faithful, breaks = 40, freq = FALSE) %>%
  ly_density(eruptions, data = faithful, hover = faithful$eruptions)
  
eruptions_hist

###

# how about an interactive Periodic Table of the Elements?  rBokeh can do it!

# prepare data
elements <- subset(elements, !is.na(group))
elements$group <- as.character(elements$group)
elements$period <- as.character(elements$period)

# add colors for groups
metals <- c("alkali metal", "alkaline earth metal", "halogen",
            "metal", "metalloid", "noble gas", "nonmetal", "transition metal")
colors <- c("#a6cee3", "#1f78b4", "#fdbf6f", "#b2df8a", "#33a02c",
            "#bbbb88", "#baa2a6", "#e08e79")
elements$color <- colors[match(elements$metal, metals)]
elements$type <- elements$metal

# make coordinates for labels
elements$symx <- paste(elements$group, ":0.1", sep = "")
elements$numbery <- paste(elements$period, ":0.8", sep = "")
elements$massy <- paste(elements$period, ":0.15", sep = "")
elements$namey <- paste(elements$period, ":0.3", sep = "")

# create figure
elements_plot <- figure(title = "Periodic Table", tools = c("resize", "hover"),
            ylim = as.character(c(7:1)), xlim = as.character(1:18),
            xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
            height = 445, width = 800) %>%
  
  # plot rectangles
  ly_crect(group, period, data = elements, 0.9, 0.9,
           fill_color = color, line_color = color, fill_alpha = 0.6,
           hover = list(name, atomic.number, type, atomic.mass,
                        electronic.configuration)) %>%
  
  # add symbol text
  ly_text(symx, period, text = symbol, data = elements,
          font_style = "bold", font_size = "10pt",
          align = "left", baseline = "middle") %>%
  
  # add atomic number text
  ly_text(symx, numbery, text = atomic.number, data = elements,
          font_size = "6pt", align = "left", baseline = "middle") %>%
  
  # add name text
  ly_text(symx, namey, text = name, data = elements,
          font_size = "4pt", align = "left", baseline = "middle") %>%
  
  # add atomic mass text
  ly_text(symx, massy, text = atomic.mass, data = elements,
          font_size = "4pt", align = "left", baseline = "middle")

elements_plot

###

# Time for cartography! Let's make a simple interactive world map with all the capital cities.

library(maps)

data(world.cities)
caps <- subset(world.cities, capital == 1)
caps$population <- prettyNum(caps$pop, big.mark = ",")

caps_map <- figure(width = 800, height = 450, padding_factor = 0) %>%
  ly_map("world", col = "gray") %>%
  ly_points(long, lat, data = caps, size = 5,
            hover = c(name, country.etc, population))

caps_map

###

# If you have a Google Key you can also use Google Maps as our basemap;
# this is an interactive map showing all the Ranger Stations in Oregon as red circles

# set your Google API key (get one at https://console.developers.google.com/ )
options(GMAP_API_KEY="AIzaSyC_giPfJ6QgBVGc5O1hN82gy2gtsXj0s6g")

orstationc <- read.csv("https://pjbartlein.github.io/GeogDataAnalysis/data/csv/orstationc.csv")

orstation_map <- gmap(lat = 44.1, lng = -120.767, zoom = 6, width = 700, height = 600,
                      api_key = "AIzaSyC_giPfJ6QgBVGc5O1hN82gy2gtsXj0s6g") %>%
  ly_points(lon, lat, data = orstationc, alpha = 0.8, col = "red",
            hover = c(station, Name, elev, tann))

orstation_map 

###

# Let's make an interactive time series for airline flights (from "flightfreq" dataset)

timeseries_plot <- figure(width = 800, height = 400) %>%
  ly_lines(date, Freq, data = flightfreq, alpha = 0.3) %>%
  ly_points(date, Freq, data = flightfreq,
            hover = list(date, Freq, dow), size = 5) %>%
  ly_abline(v = as.Date("2001-09-11"))

timeseries_plot

#####
#
# Non-interactive plot examples
#

# How about a topographic map of the Maunga Whau volcano with contour lines?
  
volcano_plot <- figure(title = "Volcano", padding_factor = 0) %>%
  ly_image(volcano) %>%
  ly_contour(volcano)

volcano_plot

###

# Or a simple boxplot (non-interactive):

boxplot_plot <- figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer,
             hover = height)

boxplot_plot

###

# Simple quantile plot with iris data (non-interactive):

iris_quant_plot <- figure(legend_location = "top_left") %>%
  ly_quantile(Sepal.Length, group = Species, data = iris)

iris_quant_plot

###

# and finally, a sorted dot plot (non-interactive)

#install.packages("latticeExtra")

wa_cancer <- droplevels(subset(latticeExtra::USCancerRates, state == "Washington"))

# sorted by male rate
ylim <- levels(with(wa_cancer, reorder(county, rate.male)))

wa_c_plot <- figure(ylim = ylim, width = 700, height = 600, tools = "") %>%
  ly_segments(LCL95.male, county, UCL95.male,
              county, data = wa_cancer, color = NULL, width = 2) %>%
  ly_points(rate.male, county, glyph = 16, data = wa_cancer)

wa_c_plot

#############
