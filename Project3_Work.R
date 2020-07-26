#### Earthquake ####
#Load libraries
library(tidyverse)
library(ranger)

#Read Data
earthquake_train_values <- read_csv("Project3/earthquake_train_values.csv")
earthquake_train_labels <- read_csv("~/Documents/ST558/ST558_Project3/Project3/earthquake_train_labels.csv")

#Condense Variables
earthquake_train_values <- earthquake_train_values %>%
  mutate(superstructure = case_when(has_superstructure_adobe_mud ==  1 ~ 'Adobe Mud',
                             has_superstructure_mud_mortar_stone ==  1 ~ 'Mud Mortar Stone',
                             has_superstructure_stone_flag ==  1 ~ 'Stone Flag',
                             has_superstructure_cement_mortar_stone ==  1 ~ 'Cement Mortar Stone',
                             has_superstructure_mud_mortar_brick ==  1 ~ 'Mud Mortar Brick',
                             has_superstructure_cement_mortar_brick ==  1 ~ 'Cement Mortar Brick',
                             has_superstructure_timber == 1 ~ "Timber",
                             has_superstructure_bamboo == 1 ~ "Bamboo",
                             has_superstructure_rc_non_engineered == 1 ~ "RC Non-Engineered",
                             has_superstructure_rc_engineered == 1 ~ "RC Engineered",
                             has_superstructure_other == 1 ~ "Other")) %>%
  select(-(has_superstructure_adobe_mud:has_superstructure_other)) %>%
  mutate(secondary_use = case_when(has_secondary_use == 0 ~ "None",
                                   has_secondary_use_agriculture == 1 ~ "Agriculture",
                                   has_secondary_use_hotel == 1 ~ "Hotel",
                                   has_secondary_use_rental == 1 ~ "Rental",
                                   has_secondary_use_institution == 1 ~ "Institution",
                                   has_secondary_use_school == 1 ~ "School",
                                   has_secondary_use_industry == 1 ~ "Industry",
                                   has_secondary_use_health_post == 1 ~ "Health Post",
                                   has_secondary_use_gov_office == 1 ~ "Government Office",
                                   has_secondary_use_use_police == 1 ~ "Police",
                                   has_secondary_use_other == 1 ~ "Other"
                                   )) %>%
  select(-(has_secondary_use:has_secondary_use_other))

#Merge Data
earthquake <- merge(earthquake_train_values, earthquake_train_labels, by = "building_id")

#Select Small Portion
set.seed(321)
smaller <- sample(1:nrow(earthquake), size = nrow(earthquake)*0.025) 
earthquake <- earthquake[smaller, ] 


#Make Factor
earthquake$damage_grade <- as.factor(earthquake$damage_grade)
earthquake$superstructure <- as.factor(earthquake$superstructure)
earthquake$secondary_use <- as.factor(earthquake$secondary_use)
earthquake$land_surface_condition <- as.factor(earthquake$land_surface_condition)
earthquake$foundation_type <- as.factor(earthquake$foundation_type)
earthquake$roof_type <- as.factor(earthquake$roof_type)
earthquake$ground_floor_type <- as.factor(earthquake$ground_floor_type)
earthquake$other_floor_type <- as.factor(earthquake$other_floor_type)
earthquake$position <- as.factor(earthquake$position)
earthquake$plan_configuration <- as.factor(earthquake$plan_configuration)
earthquake$legal_ownership_status <- as.factor(earthquake$legal_ownership_status)
earthquake$count_families <- as.factor(earthquake$count_families)
earthquake$count_floors_pre_eq <- as.factor(earthquake$count_floors_pre_eq)

#Split data
set.seed(321)
train <- sample(1:nrow(earthquake), size = nrow(earthquake)*0.7) 
test <- setdiff(1:nrow(earthquake), train)

#Subset data
earthquake_train <- earthquake[train, ] 
earthquake_test <- earthquake[test, ]

#### Random Forest ####

#Create Model
rf_ranger <- ranger(formula = damage_grade ~ ., data = earthquake_train, num.trees = 200, 
                    mtry = ncol(earthquake_train)/3)

#Predict
rfPred <- predict(rf_ranger, earthquake_test)

#Create Results
rf_results <- confusionMatrix(data = rfPred$predictions, reference = earthquake_test$damage_grade) 
rf_results

#### PCA ####
numeric_variables <- c("geo_level_1_id", "geo_level_2_id", "geo_level_3_id",
                      "age", "area_percentage", "height_percentage")

factor_variables <- c("count_floors_pre_eq", "land_surface_condition", "foundation_type", "roof_type", 
                      "ground_floor_type", "other_floor_type", "position", "plan_configuration", 
                      "legal_ownership_status", "superstructure", "secondary_use",
                      "damage_grade", "count_families")


earthquake_numeric <- earthquake %>% select(all_of(numeric_variables))

earthquake_factor <- earthquake %>% select(all_of(factor_variables))

PCs <- prcomp(earthquake_numeric, center = TRUE, scale = TRUE)
PCs

summary(PCs)

biplot(PCs, xlabs = rep(".", nrow(earthquake)), cex = 1.2)

#### Plots ####
#Table
table(earthquake$land_surface_condition, earthquake$foundation_type)

#Bar PLot
plot.1 <- ggplot(data = earthquake, aes(x = secondary_use))
plot.1 + geom_bar(aes(fill = damage_grade)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#Scatter
plot.2 <- ggplot(data = earthquake, aes(x = age, y = geo_level_1_id))
plot.2 <- plot.2 + geom_point(aes(color = damage_grade)) +
  geom_smooth() 

ggplotly(plot.2)

#Boxplot
plot.2 <- ggplot(data = earthquake, aes(x = height_percentage))
plot.2 + geom_boxplot(aes(color = damage_grade))

names(select_if(earthquake, is.factor))
