library(tidyverse)
library(janitor)
library(scales)
library(formattable)

df <- clean_names(read_csv("meat_consumption_worldwide.csv"))
head(df)

# Checking for missing values
paste("Empty values: ", toString(any(is.na(df))))

# Checking for duplicated rows
paste("Duplicated rows: ", toString(any(duplicated(df))))

# Dataframe dimensions
paste("Dimension: ", toString(dim(df)))

unique(df$location)

unique(df$time)


# Data only before 2021
df_now <- df %>%
  filter(measure == 'THND_TONNE', 
         time <= 2021,
         !location %in% c('WLD', 'OECD', 'BRICS', 'EU28', 'SSA')) %>%
  mutate(value = round(value))

# Function to set up size of plots
fig <- function(x,y) {
  options(repr.plot.width = x, repr.plot.height = y)
}


df_now %>%
  select(value) %>%
  summary()


# Calculating overall consumption by type
consumption <- df_now %>%
  group_by(subject) %>%
  summarise(total_consumption=sum(value))


# Visualization
fig(12,8)


consumption %>%
  ggplot(mapping=aes(x=subject, y=total_consumption, fill=subject)) + 
  geom_col() +
  geom_text(aes(label=total_consumption), vjust=-0.5, size=7) +
  # Handling y-ticks representation
  scale_y_continuous(labels=label_number(suffix=' M', scale=1e-6),
                     limits=c(0, 3000000)) +    # define ticks manually
  # Labels
  labs(title='Overall Consumption',
       x='Type',
       y='Thousand Ton') +
  # Non-data representation
  theme(plot.title=element_text(face='bold', size=10, hjust=0.5),
        
        axis.title=element_text(face='bold', size=20, hjust=0, color='black'),
        axis.text.x=element_blank(),
        axis.text.y=element_text(face='bold', size=12),
        
        legend.position='top',    # bottom/right/left
        legend.title=element_blank(),
        legend.text=element_text(face='bold', size=16, color='#4e5450'),
        legend.background=element_rect(fill="#e6e8e7"))



### 4.3 Consumption by countries  

# Calculating total by countries
countries <- df_now %>%
  group_by(location) %>%
  summarise(total_consumption=sum(value)) %>%
  arrange(-total_consumption)

# Visualization
fig(12, 7)

countries %>%
  ggplot(mapping=aes(x=reorder(location, desc(total_consumption)), 
                     y=total_consumption, 
                     width=0.5,
                     fill=location)) + 
  geom_col() +
  # Labels
  labs(title='Consumption by Countries',
       x='Countries',
       y='Thousand Ton') +
  # Non-data representation
  theme(plot.title=element_text(face='bold', size=20, hjust=0.5),
        
        legend.position='none',    # delete the legend
        
        axis.title=element_text(face='bold', size=30, hjust=0, color='grey'),
        axis.text.x=element_text(face='bold', angle=90),
        axis.text.y=element_text(face='bold', size=12))



# Top 10 countries
top_10 <- countries %>%
  arrange(desc(total_consumption)) %>%
  slice(1:10)

# Display
top_10 %>%
  formattable(list(location=color_bar("light green"), total_consumption=color_tile("transparent", "Red")), 
              align=c('c', 'c')  # center, right, left
  )




top_10 <- countries %>%
  arrange(desc(total_consumption)) %>%
  slice(1:10)

#visualising top 10 countries
fig(12, 7)

top_10 %>%
  ggplot(mapping=aes(x=reorder(location, desc(total_consumption)), 
                     y=total_consumption, 
                     width=0.5,
                     fill=location)) + 
  geom_col() +
  # Labels
  labs(title='Consumption by Top 10 Countries',
       x='Countries',
       y='Thousand Ton') +
  # Non-data representation
  theme(plot.title=element_text(face='bold', size=10, hjust=0.5),
        
        legend.position='none',    # delete the legend
        axis.title=element_text(face='bold', size=20, hjust=0, color='black'),
        axis.text.x=element_text(face='bold', angle=90),
        axis.text.y=element_text(face='bold', size=12))


# Top 10 countries by type
fig(20, 8)

bp <- df_now %>%
  filter(location %in% top_10$location) %>%
  # Visualization
  ggplot(mapping=aes(x=time, color=location)) + 
  geom_line(mapping=aes(y=value), size=1) + 
  # Divide by meat types
  facet_wrap(~subject) + 
  # Labels
  labs(title='Top 10 Countries by Meat Type',
       x='Year',
       y='Thousand Ton') +
  # Non-data representation
  theme(plot.title=element_text(face='bold', size=10, hjust=0.5),
        axis.title=element_text(face='bold', size=20, hjust=0, color='black'),
        axis.text.x=element_text(face='bold', size=12, angle=90),
        axis.text.y=element_text(face='bold', size=15),
        strip.text.x = element_text(face='bold', color = "darkgreen")
  )
bp 
bp + ylim(0,20000)
