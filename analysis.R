# A5 Data Report

# Install any necessary packages and load the packages from library,
# suppress any undesired content from the report
# Set your working directory to your source file location
suppressMessages(library("dplyr"))
suppressMessages(library("ggplot2"))
suppressMessages(library("plotly"))

# Load your data, making sure to not interpret strings as factors.
shootings_2018 <- read.csv("data/shootings-2018.csv",
                           stringsAsFactors = FALSE)

# To start, write the code to get some information about the dataframe:
# - How many shootings occurred?
# - How many lives were lost?
# - Which city was most impacted by shootings(make sure to clarify
# how you are measuring "impact")?
# Two other insights of your choice
shooting_num <- nrow(shootings_2018)
total_lives_lost <- shootings_2018 %>%
  select(num_killed) %>%
  summarise(lives_lost = sum(num_killed)) %>%
  pull(lives_lost)

# I will use the sum of num_killed and num_injured to measure the impact
most_impacted_city <- shootings_2018 %>%
  group_by(city) %>%
  summarise(total_casualties = sum(num_killed + num_injured)) %>%
  filter(total_casualties == max(total_casualties)) %>%
  pull(city)

# My choices for the other two insights are:
# - Which state was most impacted by shootings(make sure to clarify
# how you are measuring "impact")?
# - How many people were injured?

# I will use the sum of num_killed and num_injured to measure the impact
most_impacted_state <- shootings_2018 %>%
  group_by(state) %>%
  summarise(total_casualties = sum(num_killed + num_injured)) %>%
  filter(total_casualties == max(total_casualties)) %>%
  pull(state)
total_injured <- shootings_2018 %>%
  select(num_injured) %>%
  summarise(injured_num = sum(num_injured)) %>%
  pull(injured_num)

# Get information for number of people killed and injured for the most
# impacted state.
ca_injury_data <- shootings_2018 %>%
  filter(state == "California") %>%
  summarise(ca_injuried = sum(num_injured)) %>%
  pull(ca_injuried)

ca_death_data <- shootings_2018 %>%
  filter(state == "California") %>%
  summarise(ca_killed = sum(num_killed)) %>%
  pull(ca_killed)

# Create a summary table and sort it in a meaningful way. This should not just
# be the raw data, but instead should an aggregate table of information.
summary_table <- shootings_2018 %>%
  group_by(state) %>%
  summarise(state_death = sum(num_killed),
            percentage_of_state_death =
              round((state_death / total_lives_lost
                     * 100), 1)) %>%
  arrange(-state_death)

# Build an interactive map that shows a marker at the location of each
# shooting, and the map should meet the following criteria:
# when hovered or clicked on, each point should at least 3 pieces of
# information about the incident(with a line break --<br> -- between each
# piece of information) and no irrelevant information.

# set map shape and color for data to display on.

usa_map <- list(scope = "usa", showland = TRUE, landcolor = toRGB("gray80"),
                countrycolor = toRGB("gray88"))
# start creating the map, and the map should meet following requirements:
# - adjust the size of each marker based on a piece of data about the data
# - include hover information showing at least 3 pieces of data about the
# incident - place a line break between each piece of information

interactive_map <- plot_geo(shootings_2018, lat = ~lat, lon = ~long) %>%
  layout(title = "Areas Impacted from the US shootings in 2018",
         geo = usa_map) %>%
  add_markers(text = ~paste("State:", state, "<br />", "City:", city, "<br />",
                            "Total Death:", num_killed, "<br />",
                            "Total Injured:", num_injured),
              colors = c("lightpink2", "indianred1", "indianred"),
              color = ~num_killed + num_injured, symbol = "circle-open",
              hoverinfo = "text",
              marker = list(size = ~num_killed + num_injured)) %>%
  colorbar(title = "Total number of causalties")
  

# Create a plot of your choice

# bar graph
bar_graph <- ggplot(data = shootings_2018) +
  geom_col(mapping = aes(x = state, y = num_injured), width = 0.5) +
  theme(axis.text.x = element_text(size = 10, angle = 40, hjust = 1,
                                   vjust = 1)) +
  labs(title = "Number of Injury in US Shootings in 2018",
       x = "State", y = "Number of Injury")
