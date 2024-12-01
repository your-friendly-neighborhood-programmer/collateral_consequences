library(dplyr)
library(ggplot2)
library(httpgd)
library(knitr)
library(pandoc)


# Read csv files
alabama <- read.csv("alabama.csv")
alaska <- read.csv("alaska.csv")
arizona <- read.csv("arizona.csv")
arkansas <- read.csv("arkansas.csv")
california <- read.csv("california.csv")
colorado <- read.csv("colorado.csv")
connecticut <- read.csv("connecticut.csv")
dc <- read.csv("dc.csv")
delaware <- read.csv("delaware.csv")
federal <- read.csv("federal.csv")
florida <- read.csv("florida.csv")
georgia <- read.csv("georgia.csv")
hawaii <- read.csv("hawaii.csv")
idaho <- read.csv("idaho.csv")
illinois <- read.csv("illinois.csv")
indiana <- read.csv("indiana.csv")
iowa <- read.csv("iowa.csv")
kansas <- read.csv("kansas.csv")
kentucky <- read.csv("kentucky.csv")
louisiana <- read.csv("louisiana.csv")
maine <- read.csv("maine.csv")
maryland <- read.csv("maryland.csv")
massachusetts <- read.csv("massachusetts.csv")
michigan <- read.csv("michigan.csv")
minnesota <- read.csv("minnesota.csv")
mississippi <- read.csv("mississippi.csv")
missouri <- read.csv("missouri.csv")
montana <- read.csv("montana.csv")
nebraska <- read.csv("nebraska.csv")
nevada <- read.csv("nevada.csv")
newhampshire <- read.csv("newhampshire.csv")
newjersey <- read.csv("newjersey.csv")
newmexico <- read.csv("newmexico.csv")
newyork <- read.csv("newyork.csv")
northcarolina <- read.csv("northcarolina.csv")
northdakota <- read.csv("northdakota.csv")
ohio <- read.csv("ohio.csv")
oklahoma <- read.csv("oklahoma.csv")
oregon <- read.csv("oregon.csv")
pennsylvania <- read.csv("pennsylvania.csv")
rhodeisland <- read.csv("rhodeisland.csv")
southcarolina <- read.csv("southcarolina.csv")
southdakota <- read.csv("southdakota.csv")
tennessee <- read.csv("tennessee.csv")
texas <- read.csv("texas.csv")
utah <- read.csv("utah.csv")
vermont <- read.csv("vermont.csv")
virginia <- read.csv("virginia.csv")
washington <- read.csv("washington.csv")
westvirginia <- read.csv("westvirginia.csv")
wisconsin <- read.csv("wisconsin.csv")
wyoming <- read.csv("wyoming.csv")

# Add state name column
alabama <- alabama %>% mutate(state = "Alabama")
alaska <- alaska %>% mutate(state = "Alaska")
arizona <- arizona %>% mutate(state = "Arizona")
arkansas <- arkansas %>% mutate(state = "Arkansas")
california <- california %>% mutate(state = "California")
colorado <- colorado %>% mutate(state = "Colorado")
connecticut <- connecticut %>% mutate(state = "Connecticut")
dc <- dc %>% mutate(state = "DC")
delaware <- delaware %>% mutate(state = "Delaware")
federal <- federal %>% mutate(state = "Federal")
florida <- florida %>% mutate(state = "Florida")
georgia <- georgia %>% mutate(state = "Georgia")
hawaii <- hawaii %>% mutate(state = "Hawaii")
idaho <- idaho %>% mutate(state = "Idaho")
illinois <- illinois %>% mutate(state = "Illinois")
indiana <- indiana %>% mutate(state = "Indiana")
iowa <- iowa %>% mutate(state = "Iowa")
kansas <- kansas %>% mutate(state = "Kansas")
kentucky <- kentucky %>% mutate(state = "Kentucky")
louisiana <- louisiana %>% mutate(state = "Louisiana")
maine <- maine %>% mutate(state = "Maine")
maryland <- maryland %>% mutate(state = "Maryland")
massachusetts <- massachusetts %>% mutate(state = "Massachusetts")
michigan <- michigan %>% mutate(state = "Michigan")
minnesota <- minnesota %>% mutate(state = "Minnesota")
mississippi <- mississippi %>% mutate(state = "Mississippi")
missouri <- missouri %>% mutate(state = "Missouri")
montana <- montana %>% mutate(state = "Montana")
nebraska <- nebraska %>% mutate(state = "Nebraska")
nevada <- nevada %>% mutate(state = "Nevada")
newhampshire <- newhampshire %>% mutate(state = "NewHampshire")
newjersey <- newjersey %>% mutate(state = "NewJersey")
newmexico <- newmexico %>% mutate(state = "NewMexico")
newyork <- newyork %>% mutate(state = "NewYork")
northcarolina <- northcarolina %>% mutate(state = "NorthCarolina")
northdakota <- northdakota %>% mutate(state = "NorthDakota")
ohio <- ohio %>% mutate(state = "Ohio")
oklahoma <- oklahoma %>% mutate(state = "Oklahoma")
oregon <- oregon %>% mutate(state = "Oregon")
pennsylvania <- pennsylvania %>% mutate(state = "Pennsylvania")
rhodeisland <- rhodeisland %>% mutate(state = "RhodeIsland")
southcarolina <- southcarolina %>% mutate(state = "SouthCarolina")
southdakota <- southdakota %>% mutate(state = "SouthDakota")
tennessee <- tennessee %>% mutate(state = "Tennessee")
texas <- texas %>% mutate(state = "Texas")
utah <- utah %>% mutate(state = "Utah")
vermont <- vermont %>% mutate(state = "Vermont")
virginia <- virginia %>% mutate(state = "Virginia")
washington <- washington %>% mutate(state = "Washington")
westvirginia <- westvirginia %>% mutate(state = "WestVirginia")
wisconsin <- wisconsin %>% mutate(state = "Wisconsin")
wyoming <- wyoming %>% mutate(state = "Wyoming")

# Combine dataframes
all_states <- rbind(alabama, alaska, arizona, arkansas, california, 
    colorado, connecticut, dc, delaware, federal, florida, georgia, 
    hawaii, idaho, illinois, indiana, iowa, kansas, kentucky, louisiana, 
    maine, maryland, massachusetts, michigan, minnesota, mississippi, 
    missouri, montana, nebraska, nevada, newhampshire, newjersey, 
    newmexico, newyork, northcarolina, northdakota, ohio, oklahoma, 
    oregon, pennsylvania, rhodeisland, southcarolina, southdakota, 
    tennessee, texas, utah, vermont, virginia, washington, westvirginia, 
    wisconsin, wyoming)

# Count number of collateral consequences per jurisdiction/state
n_consequences <- data.frame(all_states %>% group_by(state) %>% summarise(count = n()))

# Create bar chart of total number of collateral consequences in each jurisdiction
n_consequences %>% ggplot(aes(x = reorder(state, count), y = count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Total Number of Collateral Consequences Per Jurisdiction",
         x = "Jurisdiction",
         y = "Number of Collateral Consequences") +
         theme_minimal()

# Create pie chart of the breakdown of time duration of collateral consequences
comb_duration <- all_states %>% group_by(Duration) %>%
  summarise(count = n())
combined_duration <- data.frame(comb_duration)
combined_duration <- combined_duration %>% mutate(percent = round(count/sum(count) * 100))
combined_duration <- combined_duration %>% filter(percent >= 1 & Duration != "")
colors <- c("blue", "red", "orange", "yellow", "green")
pie(combined_duration$count, main = "Breakdown of Time-Duration of Collateral Consequences of Conviction", 
    col = colors, label = paste(combined_duration$Duration, combined_duration$percent, "%"))










