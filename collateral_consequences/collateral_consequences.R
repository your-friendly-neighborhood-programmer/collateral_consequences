library(dplyr)
library(tidyr)
library(ggplot2)

# Read in the data for each state (I had to export one by one)
alabama  <- read.csv("./collateral_consequences/docs/alabama.csv") %>% 
    mutate(state = "Alabama")
alaska  <- read.csv("./collateral_consequences/docs/alaska.csv") %>% 
    mutate(state = "Alaska")
arizona  <- read.csv("./collateral_consequences/docs/arizona.csv") %>% 
    mutate(state = "Arizona")
arkansas  <- read.csv("./collateral_consequences/docs/arkansas.csv") %>% 
    mutate(state = "Arkansas")
california  <- read.csv("./collateral_consequences/docs/california.csv") %>% 
    mutate(state = "California")
colorado  <- read.csv("./collateral_consequences/docs/colorado.csv") %>% 
    mutate(state = "Colorado")
connecticut  <- read.csv("./collateral_consequences/docs/connecticut.csv") %>%
    mutate(state = "Connecticut")
dc <- read.csv("./collateral_consequences/docs/dc.csv") %>%
    mutate(state = "DC")
delaware  <- read.csv("./collateral_consequences/docs/delaware.csv") %>%
    mutate(state = "Delaware")
federal <- read.csv("./collateral_consequences/docs/federal.csv") %>%
    mutate(state = "Federal")
florida  <- read.csv("./collateral_consequences/docs/florida.csv") %>%
    mutate(state = "Florida")
georgia  <- read.csv("./collateral_consequences/docs/georgia.csv") %>%  
    mutate(state = "Georgia")
hawaii  <- read.csv("./collateral_consequences/docs/hawaii.csv") %>%
    mutate(state = "Hawaii")
idaho  <- read.csv("./collateral_consequences/docs/idaho.csv") %>%
    mutate(state = "Idaho")
illinois  <- read.csv("./collateral_consequences/docs/illinois.csv") %>%
    mutate(state = "Illinois")
indiana  <- read.csv("./collateral_consequences/docs/indiana.csv") %>%
    mutate(state = "Indiana")
iowa  <- read.csv("./collateral_consequences/docs/iowa.csv") %>%
    mutate(state = "Iowa")
kansas  <- read.csv("./collateral_consequences/docs/kansas.csv") %>%
    mutate(state = "Kansas")
kentucky  <- read.csv("./collateral_consequences/docs/kentucky.csv") %>%
    mutate(state = "Kentucky")
louisiana  <- read.csv("./collateral_consequences/docs/louisiana.csv") %>%
    mutate(state = "Louisiana")
maine  <- read.csv("./collateral_consequences/docs/maine.csv") %>%
    mutate(state = "Maine")
maryland  <- read.csv("./collateral_consequences/docs/maryland.csv") %>%
    mutate(state = "Maryland")
massachusetts  <- read.csv("./collateral_consequences/docs/massachusetts.csv") %>%
    mutate(state = "Massachusetts")
michigan  <- read.csv("./collateral_consequences/docs/michigan.csv") %>%
    mutate(state = "Michigan")
minnesota  <- read.csv("./collateral_consequences/docs/minnesota.csv") %>%
    mutate(state = "Minnesota")
mississippi  <- read.csv("./collateral_consequences/docs/mississippi.csv") %>%
    mutate(state = "Mississippi")
missouri  <- read.csv("./collateral_consequences/docs/missouri.csv") %>%
    mutate(state = "Missouri")
montana  <- read.csv("./collateral_consequences/docs/montana.csv") %>%
    mutate(state = "Montana")
nebraska  <- read.csv("./collateral_consequences/docs/nebraska.csv") %>%
    mutate(state = "Nebraska")
nevada  <- read.csv("./collateral_consequences/docs/nevada.csv") %>%
    mutate(state = "Nevada")
new_hampshire  <- read.csv("./collateral_consequences/docs/newhampshire.csv") %>%
    mutate(state = "New Hampshire")
new_jersey  <- read.csv("./collateral_consequences/docs/newjersey.csv") %>%
    mutate(state = "New Jersey")
new_mexico  <- read.csv("./collateral_consequences/docs/newmexico.csv") %>%
    mutate(state = "New Mexico")
new_york  <- read.csv("./collateral_consequences/docs/newyork.csv") %>%
    mutate(state = "New York")
north_carolina  <- read.csv("./collateral_consequences/docs/northcarolina.csv") %>%
    mutate(state = "North Carolina")
north_dakota  <- read.csv("./collateral_consequences/docs/northdakota.csv") %>%
    mutate(state = "North Dakota")
ohio  <- read.csv("./collateral_consequences/docs/ohio.csv") %>%
    mutate(state = "Ohio")
oklahoma  <- read.csv("./collateral_consequences/docs/oklahoma.csv") %>%
    mutate(state = "Oklahoma")
oregon  <- read.csv("./collateral_consequences/docs/oregon.csv") %>%
    mutate(state = "Oregon")
pennsylvania  <- read.csv("./collateral_consequences/docs/pennsylvania.csv") %>%
    mutate(state = "Pennsylvania")
rhode_island  <- read.csv("./collateral_consequences/docs/rhodeisland.csv") %>%
    mutate(state = "Rhode Island")
south_carolina  <- read.csv("./collateral_consequences/docs/southcarolina.csv") %>%
    mutate(state = "South Carolina")
south_dakota  <- read.csv("./collateral_consequences/docs/southdakota.csv") %>%
    mutate(state = "South Dakota")
tennessee  <- read.csv("./collateral_consequences/docs/tennessee.csv") %>%
    mutate(state = "Tennessee")
texas  <- read.csv("./collateral_consequences/docs/texas.csv") %>%
    mutate(state = "Texas")
utah  <- read.csv("./collateral_consequences/docs/utah.csv") %>%
    mutate(state = "Utah")
vermont  <- read.csv("./collateral_consequences/docs/vermont.csv") %>%
    mutate(state = "Vermont")
virginia  <- read.csv("./collateral_consequences/docs/virginia.csv") %>%
    mutate(state = "Virginia")
washington  <- read.csv("./collateral_consequences/docs/washington.csv") %>%
    mutate(state = "Washington")
west_virginia  <- read.csv("./collateral_consequences/docs/westvirginia.csv") %>%
    mutate(state = "West Virginia")
wisconsin  <- read.csv("./collateral_consequences/docs/wisconsin.csv") %>%
    mutate(state = "Wisconsin")
wyoming  <- read.csv("./collateral_consequences/docs/wyoming.csv") %>%
    mutate(state = "Wyoming")


# Combine all the states into one dataframe
all_states <- rbind(alabama, alaska, arizona, arkansas, california, 
    colorado, connecticut, dc, delaware, federal, florida, georgia, 
    hawaii, idaho, illinois, indiana, iowa, kansas, kentucky, louisiana, 
    maine, maryland, massachusetts, michigan, minnesota, mississippi, 
    missouri, montana, nebraska, nevada, new_hampshire, new_jersey, 
    new_mexico, new_york, north_carolina, north_dakota, ohio, oklahoma, 
    oregon, pennsylvania, rhode_island, south_carolina, south_dakota, 
    tennessee, texas, utah, vermont, virginia, washington, west_virginia, 
    wisconsin, wyoming)

# How many collateral consequences of conviction are there?
num <- na.omit(as.integer(all_states$Number.of.Consequences))
total_consequences <- sum(num)
#41,965

# How many collateral consequences are there in each state?
n_each_state <- all_states %>% group_by(state) %>% 
    summarize(n = sum(na.omit(as.integer(Number.of.Consequences))))

# Visualize the number of collateral consequences by state
n_by_state <- n_each_state %>% ggplot(aes(x = n, y = reorder(state, n), fill = n)) +
    geom_col() +
    geom_label(aes(label = n), fill = "white", color = "black") +
    ggtitle("Number of Collateral Consequences by State") +
    labs(y = "State", x = "Number of Collateral Consequences", caption = "Visualization by Victoria Mitchell. Data Source: National Inventory of Collateral Consequences of Conviction") +
    theme(legend.position = "none", 
    plot.title = element_text(size = 30, hjust = .5), 
    plot.caption = element_text(size = 12, hjust = .4), 
    axis.title = element_text(size = 22), 
    axis.text = element_text(size = 16), 
    plot.background = element_rect(fill = "lightblue", color = NA), 
    panel.background = element_rect(fill = "lightblue", color = NA))



# Save the plot
ggsave("./collateral_consequences/plots/collateral_consequences_by_state.png", plot = n_by_state, width = 16, height = 12)

# How many collateral consequences are there in each category of consequence?
n_each_category <- all_states %>% separate_rows(Consequences, sep = "\\|") %>%
    group_by(Consequences) %>% 
    summarize(n = sum(na.omit(as.integer(Number.of.Consequences)))) %>% 
    filter(n > 1, Consequences != "") %>% arrange(desc(n))

# Visualize the number of collateral consequences by category
category_consequence <- n_each_category %>%
    ggplot(aes(x = n, y = reorder(Consequences, n), fill = n)) +
    geom_col() +
    geom_label(aes(label = n), fill = "white", color = "black") +
    labs(y = "Type of Effect", x = "Number of Collateral Consequences", caption = "Visualization by Victoria Mitchell. Data Source: National Inventory of Collateral Consequences of Conviction") +
    ggtitle("Number of Collateral Consequences by Type of Effect") +
    theme(legend.position = "none", 
    plot.title = element_text(size = 28, hjust = -2),
    plot.caption = element_text(size = 12, hjust = -1),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA))




# Save the plot
ggsave("./collateral_consequences/plots/collateral_consequences_by_category.png", plot = category_consequence, width = 18, height = 12)

# How long do collateral consequences last?
duration <- all_states %>% separate_rows(Duration, sep = "\\|") %>%
    group_by(Duration) %>% 
    summarize(n = sum(na.omit(as.integer(Number.of.Consequences)))) %>% 
    filter(n > 1, Duration != "") %>% 
    arrange(desc(n))

# Visualize the duration of collateral consequences
duration_pie <- duration %>% 
    ggplot(aes(x = "", y = n, fill = Duration)) +
    geom_bar(stat = "identity") +
    coord_polar("y") +
    geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(n/total_consequences*100), "%")), color = "white", size = 5) +
    labs(y = "Number of Collateral Consequences", x = "", fill = "Duration", caption = "Visualization by Victoria Mitchell. Data Source: National Inventory of Collateral Consequences of Conviction") +
    ggtitle("Duration of Collateral Consequences") +
    theme_void() +
    scale_fill_brewer(palette = "Set1") +
    theme(legend.position = "left", legend.text = element_text(size = 20), 
    legend.title = element_text(size = 20), 
    plot.title = element_text(size = 20, hjust = -2), 
    plot.caption = element_text(size = 12, hjust = 1.5), 
    plot.background = element_rect(fill = "lightblue", color = NA), 
    panel.background = element_rect(fill = "lightblue", color = NA)) +
    guides(fill = guide_legend(override.aes = list(label = "")))


# Save the plot
ggsave("./collateral_consequences/plots/collateral_consequences_duration.png", plot = duration_pie, width = 12, height = 9)

# How many collateral consequences are applied by offense type?
offense <- all_states %>% separate_rows(Offense.Type, sep = "\\|") %>%
    group_by(Offense.Type) %>% 
    summarize(n = sum(na.omit(as.integer(Number.of.Consequences)))) %>% 
    filter(n > 1, Offense.Type != "") %>% 
    arrange(desc(n))

# Visualize the number of collateral consequences by offense type
offenses <- offense %>% 
    ggplot(aes(x = n, y = reorder(Offense.Type, n), fill = n)) +
    geom_col() +
    geom_label(aes(label = n), fill = "white", color = "black") +
    labs(y = "Offense Type", x = "Number of Collateral Consequences", caption = "Visualization by Victoria Mitchell. Data Source: National Inventory of Collateral Consequences of Conviction") +
    ggtitle("Number of Collateral Consequences by Offense Type") +
    theme(legend.position = "none", 
    plot.title = element_text(size = 28, hjust = 4),
    plot.caption = element_text(size = 14, hjust = 3.2),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA))


# Save the plot
ggsave("./collateral_consequences/plots/collateral_consequences_by_offense.png", plot = offenses, width = 18, height = 12)
