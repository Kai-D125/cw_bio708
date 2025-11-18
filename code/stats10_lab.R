pacman::p_load(tidyverse,
               patchwork,
               janitor,
               palmerpenguins,
               here)

# data manipulation -------------------------------------------------------

colnames(penguins_raw)

## Clean column names

clean_penguins <- clean_names(penguins_raw)

colnames(clean_penguins)

clean_penguins <- clean_penguins %>% 
  mutate(clutch_completion = ifelse(clutch_completion == "Yes",
                                    1,
                                    0))

#change species name input

clean_penguins <- clean_penguins %>% 
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))

clean_penguins <- clean_penguins %>% 
  drop_na(culmen_length_mm,
          culmen_depth_mm,
          flipper_length_mm,
          body_mass_g,
          sex)

# model selection ---------------------------------------------------------

penguin_glm <- glm(clutch_completion ~ species +
                    culmen_length_mm +
                    culmen_depth_mm +
                    flipper_length_mm +
                    body_mass_g +
                    sex,
                  data = clean_penguins,
                  family = "binomial")

#magic spell
library(MuMIn)
options(na.action = "na.fail")

m_set <- dredge(penguin_glm, rank = "AIC")
subset(m_set, delta < 2)
