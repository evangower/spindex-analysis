# Add packages
library(tidyverse)
library(showtext)

# Read data file
data <- read_csv("spindex_historical_data.csv")

# Add custom font
font_add_google("Lato", "font")
showtext_auto()

# Summarise data
summary(data)

# Set image sizing
options(repr.plot.width = 8.5, repr.plot.height = 5.5)

# Plot the change in price of the s&p over time
data %>%
  ggplot(aes(x = year, y = year_close)) +
  geom_line(linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  labs(title = "S&P Index Yearly Closing Price - 1928 to 2022", 
       subtitle = "Adjusted for reinvested dividends, and adjusted for inflation", 
       x = "Year", y = "Yearly Change", 
       caption = "Data: Macrotrends | Viz: Evan Gower") +
  theme_bw() +
  theme(text = element_text(family = "font"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        axis.title = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        plot.title = element_text(size = 21, face = "bold", hjust = 0.5, vjust = 9),
        plot.subtitle = element_text(size = 15, color = "grey50", hjust = 0.5, vjust = 14.25),
        plot.caption = element_text(size = 8.5, vjust = -9),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(30, 20, 20, 20))

# Create a bar plot of year_change by year, color if positive/negative year
data %>%
  ggplot(aes(x = year, y = year_change, fill = year_change > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(values = c("#DD396A", "#17CB92"), guide = "none") +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Annual Real Total Returns - S&P Index - 1928 to 2022", 
       subtitle = "Adjusted for reinvested dividends, and adjusted for inflation", 
       x = "Year", y = "Yearly Change", 
       caption = "Data: Macrotrends | Viz: Evan Gower") +
  theme_bw() +
  theme(text = element_text(family = "font"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        axis.title = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        plot.title = element_text(size = 21, face = "bold", hjust = 0.5, vjust = 9),
        plot.subtitle = element_text(size = 15, color = "grey50", hjust = 0.5, vjust = 14.25),
        plot.caption = element_text(size = 8.5, vjust = -9),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(30, 20, 20, 20))

# Get last number from year
data$last_number <- as.numeric(substring(as.character(data$year), nchar(as.character(data$year)), nchar(as.character(data$year))))

# Create a tile chart of year_change by decade and change_category
data %>%
  ggplot(aes(last_number, decade, fill = year_change)) +
  geom_tile(height = 10, size = 0.4, color = "black") +
  geom_text(data = data, aes(label = scales::percent(year_change)), family = "font", fontface = "bold", size = 3.5) +
  scale_x_continuous(breaks = seq(0, 9, 1)) +
  scale_y_continuous(breaks = seq(1920, 2022, 10)) +
  scale_fill_gradient2(low = "#DD396A", mid = "#fafafa", high = "#17CB92", n.breaks = 10) +
  labs(title = "Annual Real Total Returns - S&P Index - 1928 to 2022", 
       subtitle = "Adjusted for reinvested dividends, and adjusted for inflation", 
       x = "Year", y = "Yearly Change", 
       caption = "Data: Macrotrends | Viz: Evan Gower") +
  theme_bw() +
  theme(text = element_text(family = "font"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", face = "bold", size = 14),
        plot.title = element_text(size = 21, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, color = "grey50", hjust = 0.5, vjust = 2.25),
        plot.caption = element_text(size = 8.5, vjust = -6),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 20, 20, 20),
        legend.position = "none")

# Count by change category
count_category <- data %>%
  count(change_category)

# Add indicators
count_category$indicators <- c(rep(FALSE, 5), rep(TRUE, 5))

# Geom bar count_category
ggplot(count_category, aes(factor(change_category, levels = c("(50%) - (40%)", "(40%) - (30%)", "(30%) - (20%)", "(20%) - (10%)", "(10%) - (0%)", "0% - 10%", "10% - 20%", "20% - 30%", "30% - 40%", "40% - 50%")), y = n, fill = indicators)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c("#DD396A", "#17CB92"), guide = "none") +
  labs(title = "Distrobution of 1 Year Returns - 1928 to 2022", 
       subtitle = "Count of type of year return for the S&P Index", 
       x = "", y = "", 
       caption = "Data: Macrotrends | Viz: Evan Gower") +
  theme_bw() +
  theme(text = element_text(family = "font"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        axis.title = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 4),
        plot.title = element_text(size = 21, face = "bold", hjust = 0.5, vjust = 9),
        plot.subtitle = element_text(size = 15, color = "grey50", hjust = 0.5, vjust = 14.25),
        plot.caption = element_text(size = 8.5, vjust = -9),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(30, 20, 20, 20))

# Filter decades to get 1930-2010
decades <- data %>% filter(decade < 2020 & decade > 1920)

# Filter for first and last row by decade
first_last_decade <- decades %>%
  group_by(decade) %>%
  slice(c(1, n())) %>%
  ungroup()

# Calculate % change in first row year_open versus last row year_closed, group by decade
decade_change <- first_last_decade %>%
  group_by(decade) %>%
  summarize(percent_change = ((last(year_close) - first(year_open)) / first(year_open)))

# Check if percentage change is positive using ifelse statement
decade_change$positive <- ifelse(decade_change$percent_change > 0, TRUE, FALSE)

ggplot(decade_change, aes(x = factor(decade), y = percent_change, fill = positive)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(values = c("#DD396A", "#17CB92"), guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Returns by Decade - S&P Index - 1930 to 2010", 
       subtitle = "% Change from open to close of decade", 
       x = "", y = "", 
       caption = "Note: 1920s and 2020s weren't used as the decade wasn't the full period.\nData: Macrotrends | Viz: Evan Gower") +
  theme_bw() +
  theme(text = element_text(family = "font"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        axis.title = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 4),
        plot.title = element_text(size = 21, face = "bold", hjust = 0.5, vjust = 4),
        plot.subtitle = element_text(size = 15, color = "grey50", hjust = 0.5, vjust = 6.25),
        plot.caption = element_text(size = 8.5, vjust = -9),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(20, 20, 20, 20))
