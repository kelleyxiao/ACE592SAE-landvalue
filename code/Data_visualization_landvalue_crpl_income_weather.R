# -----------------#
# Shahadat Hossain
# Final Project ACE 592sae
# May 13, 2022
# This file generates the following using 'df_land_temp_income'+other data
#     1. Maps
#     2. Bar charts
#     3. OLS and Fixed effect estimation
# -----------------#

# Loading packages

library(tidyverse)
library(plotly)
library(processx)
library(rjson)
library(jsonlite)
library(zoo)
library(RColorBrewer)
library(readr)

# Output Directory
fig_outpath = "../Project/figures"

# Ethanol production ----
ethanol_df <- read_csv(file = "../Project/Mydata/ethanol_df.csv")
landvalue_y <- read_csv(file = "../Project/Mydata/landvalue_y.csv")

vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

fig_ethanol <- ethanol_df %>% 
  full_join(., landvalue_y, by = 'year') %>% 
  arrange(year) %>% 
  replace(is.na(.), 0)  %>% 
  mutate(svy = ifelse(year <=2012, "Sample", "Non-sample")) %>% 
  plot_ly(x = ~as.character(year),
          y = ~prod_bgallon,
          text = ~prod_bgallon,
          color=~svy,
          colors = c("Sample" = '#3182bd', "Non-sample" = '#adacac'),
          textposition = 'outside',
          name = "Fuel ethanol (Billion Gallons)",
          type = "bar") %>% 
  layout(shapes = list(vline(15.5, color = 'blue')),
         xaxis = list(title = "Year", tickangle = -90),
         yaxis = list(title = "Fuel ethanol<br>(Billion Gallons)"),
         title = "Annual fuel ethanol production in the US",
         margin = list(b = 100),
         showlegend = FALSE)

# export(fig_ethanol, file.path(fig_outpath, "fig_ethanol.pdf"))

fig_landv_year <- landvalue_y %>% 
  select(year, landvalue_acre_r) %>% 
  mutate(svy = ifelse(year %in% c(1997, 2002, 2007, 2012), 
                      "Sample", "Non-sample"),
         landvalue_acre_r = round(landvalue_acre_r/100, 1)) %>% 
  plot_ly(x = ~as.character(year),
          y = ~landvalue_acre_r,
          text = ~landvalue_acre_r,
          color=~svy,
          colors = c("Sample" = '#3182bd', "Non-sample" = '#adacac'),
          textposition = 'outside',
          name = "Land value ('00 $/acre)",
          type = "bar") %>% 
  layout(shapes = list(vline(15.5, color = 'blue')),
         xaxis = list(title = "Year", tickangle = -90),
         yaxis = list(title = "Land value<br>('00 $/acre)"),
         title = "Average real land value in the US",
         margin = list(b = 100),
         showlegend = FALSE)

fig_landy_ethanol <- subplot(fig_landv_year, fig_ethanol, nrows = 2,
        titleY = TRUE,shareX = TRUE) %>% 
  layout(title = '')



# Data loading 
df_land_temp_income <- read_csv(file = "../Project/Mydata/df_land_temp_income.csv")


library(dvmisc)
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

# Land value Map
map_landvalue_df <- df_land_temp_income %>% 
  select(fips, state_in, county, year, landvalue_acre_r) %>% 
  spread(year, landvalue_acre_r) %>% 
  mutate(lvgrowth = round(100*log(`2012`/`1997`)/15, 2)) %>% 
  select(fips, state_in, county, lvgrowth) %>% 
  filter(!is.na(lvgrowth)) %>%
  mutate(ldgrouth_cat = quant_groups(x = lvgrowth, groups = 5))


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

map_landvalue <- plot_ly() %>% 
  add_trace(type="choropleth",
            geojson=counties,
            locations=map_landvalue_df$fips,
            z=map_landvalue_df$lvgrowth,
            colorscale="PRGn",
            reversescale =T,
            zmin=-10,
            zmax=10,
            marker=list(line=list(width=0))) %>% 
  colorbar(title = "Landvalue<br>growth rate<br>(%)") %>% 
  layout(title = "US Landvalue growth rate by County (1997-2012)") %>% 
  layout(geo = g)

map_landvalue




fig_landvalue <- df_land_temp_income %>% 
  group_by(year, corn_belt) %>% 
  summarise(landv_avg = round(mean(landvalue_acre_r, na.rm = T))) %>% 
  ungroup() %>% 
  rename(group = corn_belt) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year, Region) %>% 
              summarise(landv_avg = round(mean(landvalue_acre_r, na.rm = T))) %>% 
              ungroup() %>% 
              rename(group = Region)) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year) %>% 
              summarise(landv_avg = round(mean(landvalue_acre_r, na.rm = T))) %>% 
              ungroup() %>% 
              mutate(group = "Overall")) %>%
  mutate(landv_avg = round(landv_avg/100, 1)) %>%
  mutate(group = recode_factor(group,
                               "Overall" = "Overall",
                               "Corn-belt" = "Corn-belt",
                               "Non-Corn belt" = "Non-Corn belt",
                               "Northeast" = "Northeast",
                               "Midwest" = "Midwest",
                               "South" = "South",
                               "West" = "West")) %>% 
  arrange(year, group) %>%
  
  group_by(year) %>%
  plot_ly(x =~group, y=~landv_avg, color =~as.character(year),
          text =~landv_avg,
          textposition = 'outside',
          colors = 'BrBG',
          # colors =c('#aadde4', '#85ced8', '#65bac6', '#4aa6b3'),
          opacity = 0.7,
          type= 'bar') %>% 
  layout(barmode = 'group',
         shapes = list(vline(2.5, color = "blue"),
                       vline(0.5, color = "blue")),
         xaxis = list(title = "",
                      zeroline = FALSE),
         yaxis = list(title = "Land value/acre (real '00 $)",
                      zeroline = FALSE),
         legend = list(# x = 0.8, y = 0.9,
                       orientation = 'h',
                       title=list(text='<b> Year </b>')))

# export(fig_landvalue, file.path(fig_outpath, "fig_landvalue.pdf"))


# Crop ---- 

# Cropland  revenue
fig_croprev <- df_land_temp_income %>% 
  group_by(year, corn_belt) %>% 
  summarise(crprev_avg = round(mean(croprev_acreusd_r, na.rm = T))) %>% 
  ungroup() %>% 
  rename(group = corn_belt) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year, Region) %>% 
              summarise(crprev_avg = round(mean(croprev_acreusd_r, na.rm = T))) %>% 
              ungroup() %>% 
              rename(group = Region)) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year) %>% 
              summarise(crprev_avg = round(mean(croprev_acreusd_r, na.rm = T))) %>% 
              ungroup() %>% 
              mutate(group = "Overall")) %>%
  mutate(crprev_avg = round(crprev_avg/100, 2)) %>% 
  mutate(group = recode_factor(group,
                               "Overall" = "Overall",
                               "Corn-belt" = "Corn-belt",
                               "Non-Corn belt" = "Non-Corn belt",
                               "Northeast" = "Northeast",
                               "Midwest" = "Midwest",
                               "South" = "South",
                               "West" = "West")) %>% 
  arrange(year, group) %>%
  
  group_by(year) %>%
  plot_ly(x =~group, y=~crprev_avg, color =~as.character(year),
          text =~crprev_avg,
          textposition = 'outside',
          colors = 'BrBG',
          # colors =c('#aadde4', '#85ced8', '#65bac6', '#4aa6b3'),
          opacity = 0.7,
          type= 'bar',
          showlegend = TRUE) %>% 
  layout(barmode = 'group',
         shapes = list(vline(2.5, color = "blue"),
                       vline(0.5, color = "blue")),
         xaxis = list(title = "",
                      zeroline = FALSE),
         yaxis = list(title = "Crop revenue/acre<br>(real '00 $)",
                      zeroline = FALSE),
         legend = list(# x = 0.8, y = 0.9,
                       orientation = 'h',
                       title=list(text='<b> Year </b>')))

# export(fig_croprev, file.path(fig_outpath, "fig_croprev.pdf"))

# Cropland  
fig_crpl <- df_land_temp_income %>% 
  group_by(year, corn_belt) %>% 
  filter(!is.na(corn_belt)) %>%
  summarise(crpl_avg = round(mean(cropland_acre, na.rm = T))) %>% 
  ungroup() %>% 
  rename(group = corn_belt) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year, Region) %>% 
              summarise(crpl_avg = round(mean(cropland_acre, na.rm = T))) %>% 
              ungroup() %>% 
              rename(group = Region)) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year) %>% 
              summarise(crpl_avg = round(mean(cropland_acre, na.rm = T))) %>% 
              ungroup() %>% 
              mutate(group = "Overall")) %>%
  mutate(crpl_avg = round(crpl_avg/1000, 2)) %>% 
  mutate(group = recode_factor(group,
                               "Overall" = "Overall",
                               "Corn-belt" = "Corn-belt",
                               "Non-Corn belt" = "Non-Corn belt",
                               "Northeast" = "Northeast",
                               "Midwest" = "Midwest",
                               "South" = "South",
                               "West" = "West")) %>% 
  arrange(year, group) %>%
  
  group_by(year) %>%
  plot_ly(x =~group, y=~crpl_avg, color =~as.character(year),
          text =~crpl_avg,
          textposition = 'outside',
          colors = 'BrBG',
          # colors =c('#aadde4', '#85ced8', '#65bac6', '#4aa6b3'),
          opacity = 0.7,
          type= 'bar',
          showlegend = TRUE) %>% 
  layout(barmode = 'group',
         shapes = list(vline(2.5, color = "blue"),
                       vline(0.5, color = "blue")),
         xaxis = list(title = "",
                      zeroline = FALSE),
         yaxis = list(title = "Cropland<br>('000 acre)",
                      zeroline = FALSE),
         legend = list(# x = 0.8, y = 0.9,
           orientation = 'h',
           title=list(text='<b> Year </b>'))
         )

# export(fig_crpl, file.path(fig_outpath, "fig_crpl.pdf"))

fig_crop <- subplot(fig_croprev, fig_crpl, nrows = 2,
        titleY = TRUE) %>% 
  layout(title = 'Countywise average crop revenue and average crop land in US')


# Income and land value ----
fig_pcincome_df <- df_land_temp_income %>%
  select(fips, year, state, state_in, county, Region, 
         landvalue_acre_r, percap_income_r) %>% 
  na.omit() %>%
  mutate(percap_income_r = log(percap_income_r),
         landvalue_acre_r = log(landvalue_acre_r))

fig_pcincome_1997 <- fig_pcincome_df %>% 
  filter(year == 1997) %>% 
  plot_ly(x = ~percap_income_r, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = F,
          opacity = .5) %>%
  layout(xaxis = list(title = "Log Percapita income ($)",
                      zeroline = FALSE,
                      range=c(9.5,12)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13))) %>% 
  add_annotations(x= 10,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 1997<b>")

fig_pcincome_2002 <- fig_pcincome_df %>% 
  filter(year == 2002) %>% 
  plot_ly(x = ~percap_income_r, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = F,
          opacity = .5) %>%
  layout(xaxis = list(title = "Log Percapita income ($)",
                      zeroline = FALSE,
                      range=c(9.5,12)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13))) %>% 
  add_annotations(x= 10,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 2002<b>")

fig_pcincome_2007 <- fig_pcincome_df %>% 
  filter(year == 2007) %>% 
  plot_ly(x = ~percap_income_r, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = F,
          opacity = .5) %>%
  layout(xaxis = list(title = "Log Percapita income ($)",
                      zeroline = FALSE,
                      range=c(9.5,12)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13))) %>% 
  add_annotations(x= 10,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 2007<b>")

fig_pcincome_2012 <- fig_pcincome_df %>% 
  filter(year == 2012) %>% 
  plot_ly(x = ~percap_income_r, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = TRUE,
          opacity = .5) %>% 
  layout(xaxis = list(title = "Log Percapita income ($)",
                      zeroline = FALSE,
                      range=c(9.5,12)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13)),
         legend = list(x = 0.1, y = 1,
                       orientation = 'h',
                       title=list(text='<b> Region </b>'))) %>% 
  add_annotations(x= 10,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 2012<b>")

fig_pcincome <- subplot(fig_pcincome_1997, fig_pcincome_2002,
        fig_pcincome_2007, fig_pcincome_2012, 
        nrows = 2,
        titleY = TRUE,
        shareX = TRUE,
        shareY = TRUE) %>% 
  layout(title = 'Scatterplot of Log per-capita income and Log land value per acre')


# Land value and population density ----

fig_pdensity_df <- df_land_temp_income %>%
  select(fips, year, state, state_in, county, Region, 
         landvalue_acre_r, pdensity_sqm) %>% 
  na.omit() %>%
  mutate(pdensity_sqm = log(pdensity_sqm),
         landvalue_acre_r = log(landvalue_acre_r))

fig_pdensity_1997 <- fig_pdensity_df %>% 
  filter(year == 1997) %>% 
  plot_ly(x = ~pdensity_sqm, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = F,
          opacity = .5) %>%
  layout(xaxis = list(title = "Log Population density (sqm)",
                      zeroline = FALSE,
                      range=c(-2,11)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13))) %>% 
  add_annotations(x= 0,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 1997<b>")

fig_pdensity_2002 <- fig_pdensity_df %>% 
  filter(year == 2002) %>% 
  plot_ly(x = ~pdensity_sqm, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = F,
          opacity = .5) %>%
  layout(xaxis = list(title = "Log Population density (sqm)",
                      zeroline = FALSE,
                      range=c(-2,11)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13))) %>%
  add_annotations(x= 0,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 2002<b>")

fig_pdensity_2007 <- fig_pdensity_df %>% 
  filter(year == 2007) %>% 
  plot_ly(x = ~pdensity_sqm, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = F,
          opacity = .5) %>%
  layout(xaxis = list(title = "Log Population density (sqm)",
                      zeroline = FALSE,
                      range=c(-2,11)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13))) %>%
  add_annotations(x= 0,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 2007<b>")

fig_pdensity_2012 <- fig_pdensity_df %>% 
  filter(year == 2012) %>% 
  plot_ly(x = ~pdensity_sqm, 
          y = ~landvalue_acre_r, 
          color = ~Region, 
          colors = c('#ae5a41', '#5a5255', '#559e83',  '#1b85b8'),
          text = ~paste0(county,",", state_in), 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = TRUE,
          opacity = .5) %>% 
  layout(xaxis = list(title = "Log Population density (sqm)",
                      zeroline = FALSE,
                      range=c(-2,11)),
         yaxis = list(title = "Log Landvalue ($/acre)",
                      zeroline = FALSE,
                      range=c(5,13)),
         legend = list(x = 0.1, y = 1,
                       orientation = 'h',
                       title=list(text='<b> Region </b>'))) %>% 
  add_annotations(x= 0,
                  y= 11,
                  xref = "x",
                  showarrow = F,
                  yref = "y",
                  text = "<b>Year: 2012<b>")

fig_pdensity <- subplot(fig_pdensity_1997, fig_pdensity_2002,
        fig_pdensity_2007, fig_pdensity_2012, 
        nrows = 2,
        titleY = TRUE,
        shareX = TRUE,
        shareY = TRUE) %>% 
  layout(title = 'Scatterplot of Log population density and Log land value per acre')




# Rainfall ----

fig_prec <- df_land_temp_income %>% 
  group_by(year, corn_belt) %>% 
  filter(!is.na(corn_belt)) %>%
  summarise(prec_avg = mean(prec, na.rm = T)) %>% 
  ungroup() %>% 
  rename(group = corn_belt) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year, Region) %>% 
              summarise(prec_avg = mean(prec, na.rm = T)) %>% 
              ungroup() %>% 
              rename(group = Region)) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year) %>% 
              summarise(prec_avg = mean(prec, na.rm = T)) %>% 
              ungroup() %>% 
              mutate(group = "Overall")) %>%
  mutate(prec_avg = round(prec_avg, 2)) %>% 
  mutate(group = recode_factor(group,
                               "Overall" = "Overall",
                               "Corn-belt" = "Corn-belt",
                               "Non-Corn belt" = "Non-Corn belt",
                               "Northeast" = "Northeast",
                               "Midwest" = "Midwest",
                               "South" = "South",
                               "West" = "West")) %>% 
  arrange(year, group) %>%
  
  group_by(year) %>%
  plot_ly(x =~group, y=~prec_avg, color =~as.character(year),
          text =~prec_avg,
          textposition = 'outside',
          colors = 'BrBG',
          # colors =c('#aadde4', '#85ced8', '#65bac6', '#4aa6b3'),
          opacity = 0.7,
          type= 'bar',
          showlegend = FALSE) %>% 
  layout(barmode = 'group',
         shapes = list(vline(2.5, color = "blue"),
                       vline(0.5, color = "blue")),
         xaxis = list(title = "",
                      zeroline = FALSE),
         yaxis = list(title = "Rainfall<br>('00 mm)",
                      zeroline = FALSE),
         legend = list(# x = 0.8, y = 0.9,
           orientation = 'h',
           title=list(text='<b> Year </b>'))
  )

fig_prec

# Temperature
fig_temp <- df_land_temp_income %>% 
  group_by(year, corn_belt) %>% 
  filter(!is.na(corn_belt)) %>%
  summarise(temp_avg = mean(temp, na.rm = T)) %>% 
  ungroup() %>% 
  rename(group = corn_belt) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year, Region) %>% 
              summarise(temp_avg = mean(temp, na.rm = T)) %>% 
              ungroup() %>% 
              rename(group = Region)) %>% 
  bind_rows(df_land_temp_income %>% 
              filter(!is.na(Region)) %>%
              group_by(year) %>% 
              summarise(temp_avg = mean(temp, na.rm = T)) %>% 
              ungroup() %>% 
              mutate(group = "Overall")) %>%
  mutate(temp_avg = round(temp_avg, 2)) %>% 
  mutate(group = recode_factor(group,
                               "Overall" = "Overall",
                               "Corn-belt" = "Corn-belt",
                               "Non-Corn belt" = "Non-Corn belt",
                               "Northeast" = "Northeast",
                               "Midwest" = "Midwest",
                               "South" = "South",
                               "West" = "West")) %>% 
  arrange(year, group) %>%
  
  group_by(year) %>%
  plot_ly(x =~group, y=~temp_avg, color =~as.character(year),
          text =~temp_avg,
          textposition = 'outside',
          colors = 'BrBG',
          # colors =c('#aadde4', '#85ced8', '#65bac6', '#4aa6b3'),
          opacity = 0.7,
          type= 'bar',
          showlegend = TRUE) %>% 
  layout(barmode = 'group',
         shapes = list(vline(2.5, color = "blue"),
                       vline(0.5, color = "blue")),
         xaxis = list(title = "",
                      zeroline = FALSE),
         yaxis = list(title = "Temperature<br>(F)",
                      zeroline = FALSE),
         legend = list(# x = 0.8, y = 0.9,
           orientation = 'h',
           title=list(text='<b> Year </b>'))
  )

fig_weather <- subplot(fig_prec, fig_temp, nrows = 2,
                    titleY = TRUE) %>% 
  layout(title = 'Countywise average rainfall and temperature in US')

# Temperature ----


g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )

map_temp <- df_land_temp_income %>% 
  select(fips, state, year, temp) %>% 
  spread(year, temp) %>% 
  mutate(temp_change = `2012`-`1997`) %>% 
  plot_ly() %>% 
  add_trace(type="choropleth",
            geojson=counties,
            locations=~fips,
            z=~temp_change,
            colorscale="PRGn",
            reversescale =T,
            zmin=-5,
            zmax=5,
            marker=list(line=list(width=0))) %>% 
  colorbar(title = "Temperature change (F)<br>[1997-2012]") %>% 
  layout(title = "US Temperature change by County (1997-2012)") %>% 
  layout(geo = g)

map_temp


# Rainfall ----

map_prec <- df_land_temp_income %>% 
  select(fips, state, year, prec) %>% 
  spread(year, prec) %>% 
  mutate(prec_change = `2012`-`1997`) %>% 
  plot_ly() %>% 
  add_trace(type="choropleth",
            geojson=counties,
            locations=~fips,
            z=~prec_change,
            colorscale="PRGn",
            reversescale =T,
            zmin=-5,
            zmax=5,
            marker=list(line=list(width=0))) %>% 
  colorbar(title = "Rainfall change ('00 mm)<br>[1997-2012]") %>% 
  layout(title = "US Rainfall change by County (1997-2012)") %>% 
  layout(geo = g)

map_prec


# Pollution ----

df_n02 <- read_csv("../Project/Mydata/df_n02_1997_2012.csv")
df_s02 <- read_csv("../Project/Mydata/df_s02_1997_2012.csv")

df_pollution <- df_n02 %>% 
  mutate(fips = sprintf("%05d", fips)) %>% 
  full_join(., 
            df_s02 %>% 
              mutate(fips = sprintf("%05d", fips)),
            by = c("fips", "year")) %>% 
  arrange(fips, year)

# Disaster ----
df_dis <- read_csv("../Project/Mydata/disaster_discrete.csv")
list_dis <- df_dis %>% 
  select(year, fips, disaster, disaster_dct) %>% 
  count(disaster) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  .$disaster

df_disaster <- df_dis %>% 
  select(year, fips, disaster, disaster_dct) %>% 
  filter(disaster %in% list_dis) %>% 
  group_by(fips, year) %>% 
  summarise(disaster_n = sum(disaster_dct)) %>% 
  ungroup()

# Rural-Urban decomposition ----
land_panel_9712 <- read_rds(file = "../Project/Mydata/land_panel_9712.rds")

# Combined data ----


final_df <- df_land_temp_income %>% 
  left_join(., df_pollution,
            by = c('fips', 'year')) %>% 
  left_join(., df_disaster,
            by = c('fips', 'year')) %>% 
  select(fips, state, county, corn_belt, Region,
         year, landvalue_acre_r, croprev_acreusd_r, 
         cropland_acre, percap_income_r, pdensity_sqm, temp, prec,
         n02, s02, disaster_n) %>% 
  mutate(year = as.factor(year),
         croprev_acreusd_r = croprev_acreusd_r+1) %>%
  drop_na()


library(panelr)

mod_cb <- lm_robust(log(landvalue_acre_r)~log(croprev_acreusd_r)+log(cropland_acre)+
               log(percap_income_r)+ log(pdensity_sqm)+temp+prec+n02+s02+
               disaster_n + year + corn_belt,
             data = final_df, se_type = 'stata')

mod_cb_tab <- tidy(mod_cb) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(estimate = as.character(estimate),
         est = ifelse(p.value<.01, paste0(estimate, "***"), 
                      ifelse(p.value<.05, paste0(estimate, " **"), 
                             ifelse(p.value<.1, paste0(estimate, "  *"), 
                                    paste0(as.character(estimate), "   ")))),
         std.error = as.character(round(std.error, 3))) %>%
  select(term, est,  std.error) %>% 
  rename(se = std.error) %>% 
  dplyr::select(term, est, se) %>%
  rbind(., data.frame(term = c("Adj.R square", "Observations"),
                      est = c(round(mod_cb$adj.r.squared, 3),
                              as.character(mod_cb$nobs)),
                      se = c(NA, NA)) %>% 
          as_tibble()) %>% 
  replace(is.na(.), "")



mod_st <- lm_robust(log(landvalue_acre_r)~log(croprev_acreusd_r)+log(cropland_acre)+
               log(percap_income_r)+ log(pdensity_sqm)+temp+prec+n02+s02+
               disaster_n + year + state,
             data = final_df, se_type = 'stata')
mod_st_tab <- tidy(mod_st) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(estimate = as.character(estimate),
         est = ifelse(p.value<.01, paste0(estimate, "***"), 
                      ifelse(p.value<.05, paste0(estimate, " **"), 
                             ifelse(p.value<.1, paste0(estimate, "  *"), 
                                    paste0(as.character(estimate), "   ")))),
         std.error = as.character(round(std.error, 3))) %>%
  select(term, est,  std.error) %>% 
  rename(se = std.error) %>% 
  dplyr::select(term, est, se) %>%
  rbind(., data.frame(term = c("Adj.R square", "Observations"),
                      est = c(round(mod_st$adj.r.squared, 3),
                              as.character(mod_st$nobs)),
                      se = c(NA, NA)) %>% 
          as_tibble()) %>% 
  replace(is.na(.), "") %>% 
  filter(!grepl("state", term)) %>% 
  rbind(data.frame(term = "State (dummy)",
                   est = "Yes", 
                   se = ""))


tab_ols <- mod_cb_tab %>% 
  mutate(se = paste0("(", se, ")")) %>%
  gather(stat, est, -term, na.rm = TRUE) %>%
  filter(!est == "()") %>%
  arrange(term) %>% 
  rename(`OLS(1)` = est) %>% 
  full_join(., mod_st_tab  %>%
              mutate(se = paste0("(", se, ")")) %>%
              gather(stat, est, -term, na.rm = TRUE) %>%
              filter(!est == "()") %>%
              arrange(term) %>% 
              rename(`OLS(2)` = est),
            by = c("term", "stat")) %>% 
  mutate(term = recode_factor(term,
                              `log(croprev_acreusd_r)` = "Log croprevenue (usd/acre)", 
                              `log(cropland_acre)` = "Log cropland (acres)", 
                              `log(percap_income_r)` = "Log per capita income (usd)",
                              `log(pdensity_sqm)` = "Log Population density (sqm)",
                              `temp` = "Temperature (F)",
                              `prec` = "Precipitation ('00 mm)",
                              `n02` = "N02",
                              `s02` = "S02",
                              `disaster_n` = "Freq of top 10 disasters",
                              `year2002` = "2002",
                              `year2007` = "2007",
                              `year2012` = "2012",
                              `corn_beltNon-Corn belt` = "Non-Corn belt",
                              `(Intercept)` = "Constant",
                              `State (dummy)` = "State (dummy)",
                              `Observations` = "Observations",
                              `Adj.R square` = "Adj.R square")) %>% 
  arrange(term, stat)
  


# Fiexed effect model

library(plm)

mod_cb_fe <- plm(log(landvalue_acre_r)~log(croprev_acreusd_r)+log(cropland_acre)+
      log(percap_income_r)+ log(pdensity_sqm)+temp+prec+n02+s02+
      disaster_n,
    data = final_df, index = c("fips", "year"), 
    model="within")


stargazer(mod_cb_fe, type = 'text')

