#*
#*This file plots the figure on WTP and population affected
#*Author: Wei Guo
#*Last edit: 6/18/2024
#*


rm(list=ls())

library(data.table)
library(RODBC) 
library(fst) 
library(utils)
library(dplyr)
library(haven)
library(modelsummary)
library(wru)
library(tidycensus)
library(future)
library(stringr)
library(wordcloud)
library(tm)
library(tidytext)
library(Matrix)
library(maxLik)
library(tictoc)
library(BiocParallel)
library(xtable)
library(fixest)
library(glue)
library(sf)
library(future.apply)
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)
library(grid)
library(ggh4x)
library(ggpattern)
library(ggpubr)

sf_use_s2(FALSE)

path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# load wtp data
setwd(paste0(path,"/Output"))
data <- readxl::read_xlsx("Results_202405.xlsx", sheet = "WTP_figure_data_corrected") %>%
  as.data.table()
names(data)[1] <- "scenario"

# Melt the data frame for ggplot
data_melted <- melt(data, id.vars = c("scenario"), variable.name = "Race", value.name = "Estimate")

# Add a type column for hue differentiation
data_melted <- data_melted %>%
  mutate(Type = ifelse(grepl("agg_WTP", scenario), "WTP", "Other")) %>%
  subset(Type=="WTP") %>%
  mutate(Type = ifelse(grepl("non_coastal", scenario), "Non-Coastal Area (N = 1,039,831)", 
                       "Coastal Area (N = 287,960)")) 
data_melted <- as.data.table(data_melted)
data_melted$Estimate <- -data_melted$Estimate 



# # Create a separate data frame for All.Population WTP estimates
# all_population_wtp <- data %>%
#   filter(grepl("agg_WTP", Scenario)) %>%
#   mutate(Race = "All.Population", Type = "WTP", Estimate = All.Population)

# Adjust Scenario names to match positions for plotting
data_melted$Race  <- factor(data_melted$Race , 
                               levels = unique(data_melted$Race ),
                               labels = c("White", "Black", "Hispanic", "Asian", "Aggregate"))
data_melted[, Scenario := "Current SFHA"] 
data_melted[, Scenario1 := ""]
data_melted[grep("Present",scenario), Scenario := "Present, "] 
data_melted[grep("SLR_3",scenario), Scenario := "High SLR, "] 
data_melted[grep("SLR_2",scenario), Scenario := "Middle SLR, "] 
data_melted[grep("SLR_1",scenario), Scenario := "Low SLR, "] 
data_melted[, scenario := gsub("no_barrier","nobarrier",scenario)] 
data_melted[grep("nobarrier",scenario), Scenario1 := "w/o Barrier"] 
data_melted[grep("_barrier",scenario), Scenario1 := "w/ Barrier"] 
data_melted[, Scenario := paste0(Scenario, Scenario1)]
data_melted[, Scenario := as.factor(Scenario)]
data_melted[, Model := "SFHA"]
data_melted[grep("HadGEM6",scenario), Model := "HadGEM6"] 
data_melted[grep("GFDL6",scenario), Model := "GFDL6"] 
data_melted[grep("CanESM",scenario), Model := "CanESM"] 

data_melted$Type <- factor(data_melted$Type, levels = c("Non-Coastal Area (N = 1,039,831)",
                                                        "Coastal Area (N = 287,960)"))
data_melted <- data_melted %>% subset(!is.na(Scenario))
data_melted <- setorder(data_melted, -Type, Scenario)

# data_melted <- data_melted %>% subset(Type=="Coastal")
data_melted$Estimate <- data_melted$Estimate/1000000

# Plotting
plot<- ggplot(data_melted %>%
         subset(!grepl("Aggregate", Race)) %>%
           subset(Model %in% c("SFHA", "HadGEM6")) %>%
           setorder(-Scenario), 
       aes(x = Scenario, y = Estimate, fill = Race)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8, width = 0.5) +
  # geom_text(data=data_melted %>%
  #             subset(!grepl("Aggregate", Race)) %>%
  #             setorder(-Scenario) %>%
  #             mutate(Estimate = round(Estimate,2)),
  #           aes(label = Estimate, color = Race),
  #           # vjust = -2,  # Adjust the vertical position to place the label above the bar
  #           position = position_dodge(width = 2),
  #           # color = "grey11",  # Text color
  #           size = 3) +
  geom_bar(data = data_melted %>%
             subset(grepl("Aggregate", Race)), 
           aes(x = Scenario, y = Estimate, fill = Race), 
           stat = "identity", position = position_nudge(x = 0), 
           color = "grey33",  alpha = 0.5, width = 0.6, color = "black")+
  geom_text(data=data_melted %>%
                subset(grepl("Aggregate", Race)) %>%
              subset(Model %in% c("SFHA", "HadGEM6")) %>%
              setorder(-Scenario) %>%
                mutate(Estimate = round(Estimate,2)),
              aes(label = Estimate),
              position = position_stack(vjust = 0.5),
              color = "grey5",  # Text color
              size = 4.5) +
  coord_flip() +  # Flip the coordinates to make it horizontal
  facet_wrap(~Type, scales = "free_y", ncol = 1, strip.position = "top") +
  force_panelsizes(rows = c(0.2, 1)) +
  scale_color_manual(name = NULL,
                    values = c(palette.colors(4, palette = "Classic Tableau"), "grey33")) +
  scale_fill_manual(name = NULL,
                    values = c(palette.colors(4, palette = "Classic Tableau"), "grey33")) +
  labs(y = "Welfare Loss (Billion $)", x = NULL) +
  theme_pubr(base_size = 12) +  # Use a minimal theme for a clean look
  theme(
    strip.placement = "outside",  # Place strip labels outside
    strip.background = element_blank(),  # Remove background for strip labels
    strip.text.x.top = element_text(angle = 0, size = 12, face = "bold"),  # Rotate facet labels to horizontal and style
    # axis.text.x = element_text(size = 10),  # Style x-axis text
    # axis.text.y = element_text(size = 10),  # Style y-axis text
    legend.position = c(0.9,0.2), # Move legend to the top
    legend.title = element_text(size = 12),  # Style legend title
    legend.text = element_text(size = 12),  # Style legend text
    # panel.grid.major = element_line(size = 0.5, linetype = 'dotted', color = "grey80"),  # Light grid lines
    # panel.grid.minor = element_blank()  # Remove minor grid lines
    )+
  guides(fill = guide_legend(ncol = 1), color = NULL)

plot
 
setwd(paste0(path, "/Output/Figure"))
ggsave('corrected_wtp_agg_HadGEM6.png',plot,scale =1, width = 8, height = 5) 



data_melted[Scenario=="Current SFHA" &
              Type == "Non-Coastal Area (N = 1,039,831)",
            Type := "Non-Coastal \n (N = 1,039,831)"]
data_melted[Scenario=="Current SFHA" &
              Type == "Coastal Area (N = 287,960)",
            Type := "Coastal \n (N = 287,960)"]
data_melted[, sequence := 1]
data_melted[Type == "Coastal \n (N = 287,960)", sequence := 2]
data_melted[Scenario == "Present, w/o Barrier", sequence := 3]
data_melted[Scenario == "Present, w/ Barrier", sequence := 4]
data_melted[Scenario == "Low SLR, w/o Barrier", sequence := 5]
data_melted[Scenario == "Low SLR, w/ Barrier", sequence := 6]
data_melted[Scenario == "Middle SLR, w/o Barrier", sequence := 7]
data_melted[Scenario == "Middle SLR, w/ Barrier", sequence := 8]
data_melted[Scenario == "High SLR, w/o Barrier", sequence := 9]
data_melted[Scenario == "High SLR, w/ Barrier", sequence := 10]


# Plotting
plot<- ggplot(data_melted %>%
                subset(!grepl("Aggregate", Race)) %>%
                subset(Scenario=="Current SFHA") %>%
                mutate(Type = fct_reorder(Type, -sequence)) , 
              aes(x = Type, y = Estimate, fill = Race)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8, width = 0.5) +
  # geom_text(data=data_melted %>%
  #             subset(!grepl("Aggregate", Race)) %>%
  #             setorder(-Scenario) %>%
  #             mutate(Estimate = round(Estimate,2)),
  #           aes(label = Estimate, color = Race),
  #           # vjust = -2,  # Adjust the vertical position to place the label above the bar
  #           position = position_dodge(width = 2),
  #           # color = "grey11",  # Text color
  #           size = 3) +
  geom_bar(data = data_melted %>%
             subset(grepl("Aggregate", Race)) %>%
             subset(Scenario=="Current SFHA") , 
           aes(x = Type, y = Estimate, fill = Race), 
           stat = "identity", position = position_nudge(x = 0), 
           color = "grey33",  alpha = 0.5, width = 0.6, color = "black")+
  geom_text(data=data_melted %>%
              subset(grepl("Aggregate", Race)) %>%
              subset(Scenario=="Current SFHA") %>%
              setorder(-Scenario) %>%
              mutate(Estimate = round(Estimate,2)),
            aes(label = Estimate),
            position = position_stack(vjust = 0.5),
            color = "grey5",  # Text color
            size = 4.5) +
  coord_flip() +  # Flip the coordinates to make it horizontal
  facet_wrap(~Scenario, scales = "free_x", ncol = 1, strip.position = "top") +
  force_panelsizes(rows = c(0.2, 1)) +
  scale_color_manual(name = NULL,
                     values = c(palette.colors(4, palette = "Classic Tableau"), "grey33")) +
  scale_fill_manual(name = NULL,
                    values = c(palette.colors(4, palette = "Classic Tableau"), "grey33")) +
  labs(y = "Welfare Loss (Billion $)", x = NULL) +
  theme_pubclean(base_size = 12) +  # Use a minimal theme for a clean look
  theme(
    strip.placement = "outside",  # Place strip labels outside
    strip.background = element_blank(),  # Remove background for strip labels
    strip.text.x.top = element_text(angle = 0, size = 12, face = "bold", hjust = 0),  # Rotate facet labels to horizontal and style
    axis.text.x = element_text(size = 10),  # Style x-axis text
    axis.text.y = element_text(size = 10),  # Style y-axis text
    legend.position = "none", # Move legend to the top
    # legend.title = element_text(size = 12),  # Style legend title
    legend.text = element_text(size = 12),  # Style legend text
    # panel.grid.major = element_line(size = 0.5, linetype = 'dotted', color = "grey80"),  # Light grid lines
    # panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  guides(fill = guide_legend(ncol = 1), color = NULL)
plot
setwd(paste0(path, "/Output/Figure"))
ggsave('current_sfha.png',plot,scale =1, width = 8, height = 3) 



# Plotting
plot<- ggplot(data_melted %>%
                subset(!grepl("Aggregate", Race)) %>%
                subset(!Scenario=="Current SFHA") %>%
                mutate(Scenario = fct_reorder(Scenario, -sequence)) , 
              aes(x = Scenario, y = Estimate, fill = Race)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8, width = 0.5) +
  # geom_text(data=data_melted %>%
  #             subset(!grepl("Aggregate", Race)) %>%
  #             setorder(-Scenario) %>%
  #             mutate(Estimate = round(Estimate,2)),
  #           aes(label = Estimate, color = Race),
  #           # vjust = -2,  # Adjust the vertical position to place the label above the bar
  #           position = position_dodge(width = 2),
  #           # color = "grey11",  # Text color
  #           size = 3) +
  geom_bar(data = data_melted %>%
             subset(grepl("Aggregate", Race)) %>%
             subset(!Scenario=="Current SFHA"),
             aes(x = Scenario, y = Estimate, fill = Race), 
           stat = "identity", position = position_nudge(x = 0), 
           color = "grey33",  alpha = 0.5, width = 0.6, color = "black")+
  geom_text(data=data_melted %>%
              subset(grepl("Aggregate", Race)) %>%
              subset(!Scenario=="Current SFHA") %>%
              setorder(-Scenario) %>%
              mutate(Estimate = round(Estimate,2)),
            aes(label = Estimate),
            position = position_stack(vjust = 0.5),
            color = "grey5",  # Text color
            size = 4.5) +
  coord_flip() +  # Flip the coordinates to make it horizontal
  facet_wrap(~Model, scales = "free_x", ncol = 3, strip.position = "top") +
  # force_panelsizes(rows = c(0.2, 1)) +
  scale_color_manual(name = NULL,
                     values = c(palette.colors(4, palette = "Classic Tableau"), "grey33")) +
  scale_fill_manual(name = NULL,
                    values = c(palette.colors(4, palette = "Classic Tableau"), "grey33")) +
  labs(y = "Welfare Loss (Billion $)", x = NULL) +
  theme_pubclean(base_size = 12) +  # Use a minimal theme for a clean look
  theme(
    strip.placement = "outside",  # Place strip labels outside
    strip.background = element_blank(),  # Remove background for strip labels
    strip.text.x.top = element_text(angle = 0, size = 12, face = "bold", hjust = 0),  # Rotate facet labels to horizontal and style
    # axis.text.x = element_text(size = 10),  # Style x-axis text
    # axis.text.y = element_text(size = 10),  # Style y-axis text
    legend.position = "top", # Move legend to the top
    legend.title = element_text(size = 12),  # Style legend title
    legend.text = element_text(size = 10),  # Style legend text
    # panel.grid.major = element_line(size = 0.5, linetype = 'dotted', color = "grey80"),  # Light grid lines
    # panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  guides(fill = guide_legend(ncol = 5), color = NULL)
plot
setwd(paste0(path, "/Output/Figure"))
ggsave('gcms_welfare.png',plot,scale =1, width = 8, height = 8) 



