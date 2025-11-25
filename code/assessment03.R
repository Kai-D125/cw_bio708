# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

pacman::p_load(tidyverse,
               patchwork,
               janitor,
               here,
               stringdist,
               MuMIn)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

df_co2 <- tibble(CO2)

# Q2
# Convert column names to lowercase and reassign to `df_co2`

df_co2 <- df_co2 %>%
  rename(
    plant = Plant,
    type = Type,
    treatment = Treatment
  )

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

df_q <- filter(df_co2, type == "Quebec")

df_m <- filter(df_co2, type == "Mississippi")

plot_q <- df_q %>% 
  ggplot(aes(x = conc,
             y = uptake,
             color = treatment)) +
  geom_point()+
  ggtitle('Quebec')

plot_m <- df_m %>% 
  ggplot(aes(x = conc,
             y = uptake,
             color = treatment)) +
  geom_point()+
  ggtitle('Mississipi')

plot_q / plot_m

# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop suitable statistical models to examine:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

lm_m <- lm(uptake ~ conc * treatment, data = df_m)
lm_q <- lm(uptake ~ conc * treatment, data = df_q)

summary(lm_m)
summary(lm_q)

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:
# (no coding required for this question)
# While there was a relationship between all variables in the Mississippi samples,
# the atmospheric co2 was far more significant than the rest.
# This is supported by the Quebec samples, where only co2 concentration is significantly related,
# showing that the main driver of co2 uptake in this experiment was the ambient co2 concentration.
# However, Mississippi samples could be more sensitive to temperature than ones from Quebec,
# perhaps reflecting a genetic tolerance to cold present within populations in Quebec.


# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
 data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`

df_env <- df_env %>%
  rename(
    utm.ew = UTM.EW,
    utm.ns = UTM.NS,
    precipitation = Precipitation,
    elevation = Elevation,
    age.cat = Age.cat,
    geology = Geology,
    habitat = Habitat,
    stream = Stream,
    envhet = EnvHet
  )

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

unique(df_env$utm.ew)
unique(df_env$utm.ns)
unique(df_env$precipitation)
unique(df_env$elevation)
unique(df_env$age.cat)
unique(df_env$geology)
unique(df_env$habitat)
unique(df_env$stream)
unique(df_env$envhet)

df_env_sub <- select(df_env, -c(precipitation, elevation, geology))

# Q8
# Calculate summary statistics for each plot using `df_bci`.
# For each plot, compute:
# - n_sum: total count of all individuals across species
# - n1: count of the most dominant species (maximum count among species)
# - p: proportion of the most abundant species (n1 / n_sum)
# Assign the resulting dataframe to `df_n`.

unique(df_bci$plot)

df_n <- df_bci %>% 
  group_by(plot) %>% 
  mutate(n_sum = sum(count), 
            n1 = max(count, na.rm = TRUE),
         p = n1 / n_sum) 

# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- left_join(x = tibble(df_n),
                  y = tibble(df_env_sub),
                  by = "plot")
  
# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species in each plot. Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# Fit a suitable statistical model to the data.
# Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

var(df_m$p)
mean(df_m$p)

#p_mod <- glmmTMB::glmmTMB(p ~ envhet + stream + habitat,
               #data = df_m,
             #family = beta_family(link="logit"))
#summary(p_mod)
#I have made the above code into a comment, as I am having trouble with the
#syntax of the beta family

#Since the proportion was a continuous variable with an upper bound of 1
#and a lower bound of 0, beta distribution makes the most sense for the model