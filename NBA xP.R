library(tidyverse)
library(grid)
library(hexbin)
library(jsonlite)
library(httr)
library(scales)
library(ggplot2)
library(cowplot)
library(prismatic)
library(extrafont)
library(cowplot)
library(ballr)
library(nbastatR)
library(caret)
library(xgboost)
library(RCurl)
library(caTools)
library(car)
library(gt)
library(gtExtras)
library(randomForest)
library(vip)
library(ggthemes)
library(magick)
library(ranger)
teams <- teamcolors::teamcolors
teams <- teams %>% 
  filter(league == 'nba')
options(scipen=999)
prevseasons <- read.csv('prevseasons.csv')
#Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
currentseason <- teams_shots(all_active_teams = T, season_types = "Regular Season", seasons = 2022)
prevseasons$dateGame <- as.character(prevseasons$dateGame)
final <- bind_rows(currentseason,prevseasons)
final %>%
  mutate(made_shot = ifelse(typeEvent == 'Made Shot', 1,0)) -> final
final %>%
  mutate(three_3pa = ifelse(typeShot == '3PT Field Goal', 1,0)) -> final
final %>%
  mutate(two_2pa = ifelse(typeShot == '2PT Field Goal', 1,0)) -> final
final %>%
  mutate(points = case_when(
    typeShot == '3PT Field Goal' ~ 3,
    typeShot == '2PT Field Goal' ~ 2
  )) -> final
final %>% 
  mutate(layup =ifelse(typeAction == 'Layup Shot' | typeAction == 'Running Layup Shot'| typeAction == 'Driving Layup Shot' | typeAction == 'Reverse Layup Shot' | typeAction == 'Driving Reverse Layup Shot' | typeAction == 'Putback Layup Shot' | typeAction == 'Tip Layup Shot' | typeAction == 'Driving Finger Roll Layup Shot' | typeAction == 'Running Finger Roll Layup Shot' | typeAction == 'Alley Oop Layup shot' | typeAction == 'Cutting Layup Shot' | typeAction == 'Cutting Finger Roll Layup Shot' | typeAction == 'Finger Roll Layup Shot' | typeAction == 'Running Alley Oop Layup Shot' | typeAction == 'Running Reverse Layup Shot', 1, 0)) -> final
final %>% 
  mutate(dunk = ifelse(typeAction == 'Alley Oop Dunk Shot' | typeAction == 'Driving Dunk Shot' | typeAction == 'Dunk Shot' | typeAction == 'Running Dunk Shot' | typeAction == 'Cutting Dunk Shot' | typeAction == 'Putback Dunk Shot' | typeAction == 'Running Alley Oop Dunk Shot' | typeAction == 'Tip Dunk Shot' | typeAction == 'Reverse Dunk Shot' | typeAction == 'Running Reverse Dunk Shot' | typeAction == 'Driving Reverse Dunk Shot',1,0)) -> final
final %>% 
  mutate(jumpshot = ifelse(typeAction == 'Driving Floating Jump Shot' | typeAction == 'Jump Shot' | typeAction == 'Pullup Jump shot' | typeAction == 'Step Back Jump shot' | typeAction == 'Floating Jump shot' | typeAction == 'Jump Bank Shot' | typeAction == 'Running Jump Shot' | typeAction == 'Running Pull-Up Jump Shot' | typeAction == 'Driving Floating Bank Jump Shot' | typeAction == 'Turnaround Fadeaway shot' | typeAction == 'Turnaround Jump Shot' | typeAction == 'Turnaround Fadeaway Bank Jump Shot' | typeAction == 'Step Back Bank Jump Shot' | typeAction == 'Pullup Bank shot' | typeAction == 'Turnaround Bank shot' | typeAction == 'Driving Bank shot' | typeAction == 'Fadeaway Bank shot' | typeAction == 'Driving Jump Shot', 1, 0)) -> final
final %>% 
  mutate(hookshot = ifelse(typeAction == 'Turnaround Hook Shot' | typeAction == 'Driving Bank Hook Shot' | typeAction == 'Driving Hook Shot' | typeAction == 'Hook Shot' | typeAction == 'Hook Bank Shot' | typeAction == 'Turnaround Bank Hook Shot' | typeAction == 'Running Hook Shot',1,0)) -> final
final %>% 
  mutate(paint = ifelse(zoneBasic == 'In The Paint (Non-RA)' | zoneBasic == 'Restricted Area',1,0)) -> final
final %>% 
  mutate(backcourt = ifelse(zoneBasic == 'Backcourt',1,0)) -> final
final %>% 
  mutate(corner = ifelse(zoneBasic == 'Left Corner 3' | zoneBasic == 'Right Corner 3',1,0)) -> final
final %>% 
  mutate(midrange = ifelse(zoneBasic == 'Mid-Range',1,0)) -> final
final %>% 
  mutate(three_3pm = ifelse(made_shot == 1 & three_3pa == 1,1,0)) -> final
final$totalSecondsRemaining <- (final$minutesRemaining * 60) + final$secondsRemaining
final %>% 
  mutate(pullup = ifelse(typeAction == "Pullup Jump shot" | typeAction == "Running Pull-Up Jump Shot",1,0)) -> final
final %>% 
  mutate(putback_tipin = ifelse(typeAction == "Tip Layup Shot" | typeAction == "Tip Dunk Shot" | typeAction == "Putback Dunk Shot" | typeAction == "Putback Layup Shot" | typeAction == "Tip Shot" | typeAction == "Running Tip Shot" | typeAction == "Putback Slam Dunk Shot" | typeAction == "Putback Reverse Dunk Shot",1,0)) -> final
final %>% 
  mutate(fadeaway = ifelse(typeAction == "Turnaround Fadeaway shot" | typeAction == "Fadeaway Jump Shot" | typeAction == "Turnaround Fadeaway Bank Jump Shot" | typeAction == "Fadeaway Bank shot",1,0)) -> final
final %>% 
  mutate(stepback = ifelse(typeAction == "Step Back Jump shot" | typeAction == "Step Back Bank Jump Shot",1,0)) -> final
final %>% 
  mutate(reverse = ifelse(typeAction == "Driving Reverse Layup Shot" | typeAction == "Reverse Layup Shot" | typeAction == "Reverse Dunk Shot" | typeAction == "Running Reverse Layup Shot" | typeAction == "Driving Reverse Dunk Shot" | typeAction == "Running Reverse Dunk Shot" | typeAction == 'Reverse Slam Dunk Shot',1,0)) -> final
final %>% 
  mutate(drive = ifelse(typeAction == "Driving Floating Jump Shot"  | typeAction == "Driving Layup Shot" | typeAction == "Driving Floating Bank Jump Shot"  | typeAction == "Driving Finger Roll Layup Shot" | typeAction == "Driving Reverse Layup Shot" | typeAction == "Driving Dunk Shot" | typeAction == 'Driving Hook Shot' | typeAction == "Driving Bank Hook Shot"  | typeAction == "Driving Bank shot" | typeAction == "Driving Jump shot" | typeAction == "Driving Slam Dunk Shot" | typeAction == 'Driving Reverse Dunk Shot',1,0)) -> final
final %>% 
  mutate(turnaround = ifelse(typeAction == "Turnaround Fadeaway shot" | typeAction == "Turnaround Jump Shot" | typeAction == "Turnaround Hook Shot" | typeAction == "Turnaround Fadeaway Bank Jump Shot" | typeAction == "Turnaround Bank Hook Shot" | typeAction == "Turnaround Bank shot" | typeAction == 'Turnaround Bank shot',1,0)) -> final
#Model Data
model_data <- final %>% 
  select(made_shot, locationX, locationY, distanceShot, layup, dunk, jumpshot, paint, totalSecondsRemaining, midrange, backcourt, pullup, putback_tipin, fadeaway, stepback, reverse, drive, turnaround)
shotmake_distance_mod <- glm(made_shot ~ ., data = model_data, family = binomial)
summary(shotmake_distance_mod)
shotmake_distance_mod.probs <- predict(shotmake_distance_mod,type = "response")
model_data$make_prob = predict(shotmake_distance_mod, model_data, type="response")
model_data <- model_data %>% 
  mutate(exp_shotmake = ifelse(make_prob >= 0.5,1,0))
model_data <- model_data %>% 
  mutate(correct_shotmake = ifelse(exp_shotmake == made_shot,1,0))


ggplot(model_data, aes(x=distanceShot, y=make_prob)) + 
  geom_point() +
  geom_smooth() +
  theme_fivethirtyeight() +
  labs(title = 'Made Shot Probability by Distance Using Logit Model',
       subtitle = 'Data From: 2011-2022 Season',
       caption = '@aborelli24 | Data: NBA') + theme(plot.title = element_text(size = 15, lineheight = 1.2, hjust = 0.5, face = "bold"),
                                                    plot.subtitle = element_text(size = 12, lineheight = 1.2, hjust = 0.5))

rf_made <- ranger(made_shot ~ ., data = model_data, num.trees = 100, importance = 'impurity')
vip(rf_made, num_features = 17) + theme_fivethirtyeight()
vip(rf_made, num_features = 17, geom = "point", horizontal = TRUE, 
    aesthetics = list(color = "black", shape = 16, size = 2)) +
  theme(text=element_text(family="Garamond", size=14))
dim(model_data)
rf_grid <- expand.grid(mtry = seq(2,8, by = 1),
                       splitrule = 'variance', min.node.size = 5)
rf_made_tune <- train(made_shot ~., data = model_data, method = 'ranger', num.trees = 20,
                      trControl = trainControl(method = 'cv', number = 5), tuneGrid = rf_grid)
rf_made_tune$bestTune
rf_made_best <- ranger(made_shot ~ ., data = model_data, 
                      num.trees = 100, importance = "impurity",
                      mtry = 3)
rf_preds <- data.frame(predict(rf_made_best, data.frame(model_data))$predictions) 

names(rf_preds)

rf_preds <- rf_preds %>%
  rename(exp_make = predict.rf_made_best..data.frame.model_data...predictions)


ggplot(makes_rf_projs, aes(x=distanceShot, y=exp_make)) + 
  geom_point() +
  geom_smooth() + 
  labs(title = 'Made Shot Probability by Distance Using Random Forest Model',
       subtitle = 'Data From: 2011-2022 Season',
       caption = '@aborelli24 | Data: NBA') + theme_fivethirtyeight() + theme(plot.title = element_text(size = 15, lineheight = 1.2, hjust = 0.5, face = "bold"),
                                                    plot.subtitle = element_text(size = 12, lineheight = 1.2, hjust = 0.5))


# Bind the original dataset and predictions together
makes_rf_projs <- cbind(final, rf_preds)
makes_rf_projs <- makes_rf_projs %>% 
  mutate(exp_shotmake = ifelse(exp_make >= 0.5,1,0))
makes_rf_projs <- makes_rf_projs %>% 
  mutate(correct_shotmake = ifelse(exp_shotmake == made_shot,1,0))
makes_rf_projs <- makes_rf_projs %>%
  mutate(make_over_expected = made_shot - exp_make)
makes_rf_projs <- makes_rf_projs %>%
  mutate(xP = points * exp_make)
makes_rf_projs <- makes_rf_projs %>%
  mutate(result = points * made_shot)
makes_rf_projs <- makes_rf_projs %>% 
  mutate(three_3p_shot_prob = ifelse(three_3pa == 1,exp_make, 0))
season22 <- makes_rf_projs %>% 
  filter(yearSeason == 2022)
season22 <- season22 %>% 
  left_join(teams, by = c('nameTeam' = 'name'))
makes <- season22 %>% 
  filter(made_shot == 1)
misses <- season22 %>% 
  filter(made_shot == 0)
season22 %>% 
  group_by(namePlayer) %>% 
  summarize(
    sumpoints = sum(result), shot_perc = mean(made_shot), totalxP = sum(xP), xShotMakes = sum(exp_make), x3pMakes = sum(three_3p_shot_prob), xShot_perc = mean(exp_make), shots = n(), threePM = sum(three_3pm), shots_made = sum(made_shot), logo = unique(logo)
  ) -> xP
xP <- xP %>% 
  mutate(TPOE = sumpoints - totalxP)
xP <- xP %>% 
  mutate(SPOE = shot_perc - xShot_perc)
xP <- xP %>% 
  mutate(eFG = (shots_made + (0.5*threePM))/shots)
xP <- xP %>% 
  mutate(XeFG = (xShotMakes + (0.5*x3pMakes))/shots)
xP <- xP %>% 
  mutate(eFGOE = eFG -XeFG)
xP <- xP %>% 
  mutate(xPPS = totalxP/shots)
xP <- xP %>% 
  filter(shots > 200)
xP[is.na(xP)] <- "http://content.sportslogos.net/logos/6/236/thumbs/23654622016.gif"
xP <- arrange(xP, -TPOE)
xP$number <- 1:nrow(xP) 
xP %>%
  ungroup() %>% 
  dplyr::select(namePlayer,logo, sumpoints, totalxP, TPOE, XeFG, eFGOE, xPPS) %>%
  dplyr::mutate(TPOE_pct = percent_rank(TPOE)*100) %>%
  dplyr::mutate(XeFG_pct = percent_rank(XeFG)*100) %>%
  dplyr::mutate(eFGOE_pct = percent_rank(eFGOE)*100) %>%
  dplyr::mutate(xPPS_pct = percent_rank(xPPS)*100) %>%
  head(15) %>% 
  gt() %>%
  gt_theme_538() %>% 
  tab_header(title = md("**Top 15 Players in Total Points Over Expected**"), subtitle = '2021-22 Season | As of Jan. 12th | min. 200 FGA') %>%
  cols_label(
    namePlayer = 'Team',
    logo = '',
    sumpoints = 'Total FG Points',
    totalxP = 'Expected Points',
    TPOE = "TPOE",
    XeFG = "XeFG",
    eFGOE = "eFGOE",
    xPPS = 'xPPS',
    TPOE_pct = 'TPOE PCT',
    XeFG_pct = 'XeFG PCT',
    eFGOE_pct = 'eFGOE PCT',
    xPPS_pct = 'xPPS PCT'
  ) %>%
  cols_align(
    align = "center",
    columns = 1:10
  ) %>%
  text_transform(
    locations = cells_body(columns = c('logo')),
    fn = function(x){
      web_image(url = x)
    }
  ) %>% 
  gt_plt_bar_pct(column = TPOE_pct, scaled = TRUE, fill = '#17408B') %>%
  gt_plt_bar_pct(column = XeFG_pct, scaled = TRUE, fill = '#C9082A') %>%
  gt_plt_bar_pct(column = eFGOE_pct, scaled = TRUE, fill = '#178b62') %>%
  gt_plt_bar_pct(column = xPPS_pct, scaled = TRUE, fill = '#177a8b') %>%
  cols_align("center", contains("scale")) %>%
  fmt_percent(
    columns = c(eFGOE,XeFG),
    decimals = 1
  ) %>%
  fmt_number(
    columns = 5,
    decimals = 1,
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = 4,
    decimals = 0
  ) %>%
  fmt_number(
    columns = 8,
    decimals = 2
  ) %>%
  fmt_number(
    columns = 3,
    decimals = 0
  ) %>% 
  tab_footnote(
    footnote = 'Total Points Over Expected',
    locations = cells_column_labels(
      columns = TPOE
    )
  ) %>% 
  tab_footnote(
    footnote = 'Expected Effective FG Percentage',
    locations = cells_column_labels(
      columns = XeFG
    )
  ) %>% 
  tab_footnote(
    footnote = 'Effective FG Percentage Over Expected',
    locations = cells_column_labels(
      columns = eFGOE
    )
  ) %>%
  tab_footnote(
    footnote = 'Expected Points Per Shot',
    locations = cells_column_labels(
      columns = xPPS
    )
  ) %>%
  tab_source_note(
    source_note = "@aborelli24 on Twitter | Data: NBA"
  ) %>% gtsave('playershot2.png')
xP %>%
  ungroup() %>% 
  dplyr::select(namePlayer,logo, sumpoints, totalxP, TPOE, XeFG, eFGOE, xPPS) %>%
  dplyr::mutate(totalxP_pct = percent_rank(totalxP)*100) %>%
  dplyr::mutate(XeFG_pct = percent_rank(XeFG)*100) %>%
  dplyr::mutate(eFGOE_pct = percent_rank(eFGOE)*100) %>%
  dplyr::mutate(xPPS_pct = percent_rank(xPPS)*100) %>%
  head(15) %>% 
  gt() %>%
  gt_theme_538() %>% 
  tab_header(title = md("**Top 15 Players in Total Points Over Expected**"), subtitle = '2021-22 Season | As of Jan. 12th | min. 200 FGA') %>%
  cols_label(
    namePlayer = 'Team',
    logo = '',
    sumpoints = 'Total FG Points',
    totalxP = 'Expected Points',
    TPOE = "TPOE",
    XeFG = "XeFG",
    eFGOE = "eFGOE",
    xPPS = 'xPPS',
    totalxP_pct = 'xP PCT',
    XeFG_pct = 'XeFG PCT',
    eFGOE_pct = 'eFGOE PCT',
    xPPS_pct = 'xPPS PCT'
  ) %>%
  cols_align(
    align = "center",
    columns = 1:10
  ) %>%
  text_transform(
    locations = cells_body(columns = c('logo')),
    fn = function(x){
      web_image(url = x)
    }
  ) %>% 
  gt_plt_bar_pct(column = totalxP_pct, scaled = TRUE, fill = '#17408B') %>%
  gt_plt_bar_pct(column = XeFG_pct, scaled = TRUE, fill = '#C9082A') %>%
  gt_plt_bar_pct(column = eFGOE_pct, scaled = TRUE, fill = '#178b62') %>%
  gt_plt_bar_pct(column = xPPS_pct, scaled = TRUE, fill = '#177a8b') %>%
  cols_align("center", contains("scale")) %>%
  fmt_percent(
    columns = c(eFGOE,XeFG),
    decimals = 1
  ) %>%
  fmt_number(
    columns = 5,
    decimals = 1,
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = 4,
    decimals = 0
  ) %>%
  fmt_number(
    columns = 8,
    decimals = 2
  ) %>%
  fmt_number(
    columns = 3,
    decimals = 0
  ) %>% 
  tab_footnote(
    footnote = 'Total Points Over Expected',
    locations = cells_column_labels(
      columns = TPOE
    )
  ) %>% 
  tab_footnote(
    footnote = 'Expected Effective FG Percentage',
    locations = cells_column_labels(
      columns = XeFG
    )
  ) %>% 
  tab_footnote(
    footnote = 'Effective FG Percentage Over Expected',
    locations = cells_column_labels(
      columns = eFGOE
    )
  ) %>%
  tab_footnote(
    footnote = 'Expected Points Per Shot',
    locations = cells_column_labels(
      columns = xPPS
    )
  ) %>%
  tab_source_note(
    source_note = "@aborelli24 on Twitter | Data: NBA"
  ) %>% gtsave('playershot2.png')
season22 %>% 
  group_by(nameTeam) %>% 
  summarize(
    sumpoints = sum(result), shot_perc = mean(made_shot), totalxP = sum(xP), xShotMakes = sum(exp_make), x3pMakes = sum(three_3p_shot_prob), xShot_perc = mean(exp_make), shots = n(), threePM = sum(three_3pm), shots_made = sum(made_shot), logo = unique(logo)
  ) -> xP2
xP2 <- xP2 %>% 
  mutate(TPOE = sumpoints - totalxP)
xP2 <- xP2 %>% 
  mutate(SPOE = shot_perc - xShot_perc)
xP2 <- xP2 %>% 
  mutate(eFG = (shots_made + (0.5*threePM))/shots)
xP2 <- xP2 %>% 
  mutate(XeFG = (xShotMakes + (0.5*x3pMakes))/shots)
xP2 <- xP2 %>% 
  mutate(eFGOE = eFG -XeFG)
xP2 <- xP2 %>% 
  mutate(xPPS = totalxP/shots)
xP2 <- xP2 %>% 
  mutate(TPOEpr = percent_rank(TPOE))
xP2[is.na(xP2)] <- "http://content.sportslogos.net/logos/6/236/thumbs/23654622016.gif"
xP2 <- arrange(xP2, -TPOE)
xP2$number <- 1:nrow(xP2) 
xP2 %>%
  dplyr::select(nameTeam,logo, sumpoints, totalxP, TPOE, XeFG, eFGOE, xPPS) %>%
  dplyr::mutate(XP_pct = percent_rank(totalxP)*100) %>%
  dplyr::mutate(XeFG_pct = percent_rank(XeFG)*100) %>%
  dplyr::mutate(eFGOE_pct = percent_rank(eFGOE)*100) %>%
  dplyr::mutate(xPPS_pct = percent_rank(xPPS)*100) %>%
  head(15) %>% 
  gt() %>%
  gt_theme_538() %>% 
  tab_header(title = md("**Top 15 NBA Teams in Points Over Expected**"), subtitle = '2021-22 Season | As of Jan. 12th, 2022') %>%
  cols_label(
    nameTeam = 'Team',
    logo = '',
    sumpoints = 'Total FG Points',
    totalxP = 'Expected Points',
    TPOE = "TPOE",
    XeFG = "XeFG",
    eFGOE = "eFGOE",
    xPPS = 'xPPS',
    XP_pct = 'xP PCT',
    XeFG_pct = 'XeFG PCT',
    eFGOE_pct = 'eFGOE PCT',
    xPPS_pct = 'xPPS PCT'
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  text_transform(
    locations = cells_body(columns = c('logo')),
    fn = function(x){
      web_image(url = x)
    }
  ) %>% 
  gt_plt_bar_pct(column = XP_pct, scaled = TRUE, fill = '#17408B') %>%
  gt_plt_bar_pct(column = XeFG_pct, scaled = TRUE, fill = '#C9082A') %>%
  gt_plt_bar_pct(column = eFGOE_pct, scaled = TRUE, fill = '#178b62') %>%
  gt_plt_bar_pct(column = xPPS_pct, scaled = TRUE, fill = '#8b1740') %>%
  cols_align("center", contains("scale")) %>%
  fmt_percent(
    columns = c(eFGOE,XeFG),
    decimals = 1
  ) %>%
  fmt_number(
    columns = 5,
    decimals = 1,
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = 4,
    decimals = 0
  ) %>%
  fmt_number(
    columns = 3,
    decimals = 0
  ) %>% 
  fmt_number(
    columns = 8,
    decimals = 2
  ) %>% 
  tab_footnote(
    footnote = 'Total Points Over Expected',
    locations = cells_column_labels(
      columns = TPOE
    )
  ) %>% 
  tab_footnote(
    footnote = 'Expected Effective FG Percentage',
    locations = cells_column_labels(
      columns = XeFG
    )
  ) %>% 
  tab_footnote(
    footnote = 'Effective FG Percentage Over Expected',
    locations = cells_column_labels(
      columns = eFGOE
    )
  ) %>%
  tab_footnote(
    footnote = 'Expected Points Per Shot Attempt',
    locations = cells_column_labels(
      columns = xPPS
    )
  ) %>% 
  tab_source_note(
    source_note = "@aborelli24 on Twitter | Data: NBA"
  ) %>% gtsave('teamshot.png')
team1 <- image_read("teamshot.png")
team2 <- image_read("teamshot2.png")

img <- c(team1,team2)
image_append(image_scale(img, "x3000"))
#Shot Chart
jokic <- season22 %>% 
  filter(namePlayer == 'Nikola Jokic')
ggplot(season22, aes(x=locationX, y=locationY)) +
  geom_point(aes(colour = make_over_expected))
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
library(jpeg)
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))
ggplot(season22, aes(x=locationX, y=locationY)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = xP)) +
  scale_color_gradient() +
  xlim(-250, 250) +
  ylim(-50, 420) +
  theme_fivethirtyeight() +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(),
        plot.background = element_rect(fill = '#EBEBEB'),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = 'right',
        legend.direction = 'vertical',
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        legend.background = element_rect(fill = '#EBEBEB'),
        plot.title = element_text(size = 15, lineheight = 1.2, hjust = 0.5, face = "bold")) +
  labs(title = 'Expected Points By Location',
       subtitle = '2021-22 Season | As of Jan. 12th, 2022',
       caption = '@aborelli24 | Data: NBA') +
  guides(fill=guide_legend(title="xP"))
#Shot Distance
season22 <- season22 %>% 
  mutate(lessthan5 = ifelse(distanceShot < 5,1,0))
makes_rf_projs %>% 
  group_by(zoneRange) %>% 
  summarize(
    sumpoints = sum(result), shot_perc = mean(made_shot), totalxP = sum(xP), xShotMakes = sum(exp_make), x3pMakes = sum(three_3p_shot_prob), xShot_perc = mean(exp_make), threePM = sum(three_3pm),shots = n(), shots_made = sum(made_shot)
  ) -> xPzone
xPzone <- xPzone %>% 
  mutate(TPOE = sumpoints - totalxP)
xPzone <- xPzone %>% 
  mutate(SPOE = shot_perc - xShot_perc)
xPzone <- xPzone %>% 
  mutate(eFG = (shots_made + (0.5*threePM))/shots)
xPzone <- xPzone %>% 
  mutate(XeFG = (xShotMakes + (0.5*x3pMakes))/shots)
xPzone <- xPzone %>% 
  mutate(eFGOE = eFG -XeFG)
xPzone <- xPzone %>% 
  mutate(xPPS = sumpoints/shots)
x <- c('Less Than 8 ft.', '8-16 ft.', '16-24 ft.', '24+ ft.', 'Back Court Shot')

xPzone %>%
  mutate(zoneRange =  factor(zoneRange, levels = x)) %>%
  arrange(zoneRange) -> xPzone
xPzone %>%
  dplyr::select(zoneRange, totalxP, xPPS, xShot_perc) %>%
  dplyr::mutate(XP_pct = percent_rank(totalxP)*100) %>%
  dplyr::mutate(xPPS_pct = percent_rank(xPPS)*100) %>%
  dplyr::mutate(xShot_perc_pct = percent_rank(xShot_perc)*100) %>%
  gt() %>%
  gt_theme_538() %>% 
  tab_header(title = md("**Expected Points Scored By Range**"), subtitle = 'Seasons 2011-2022 | As of Jan. 6th, 2022') %>%
  cols_label(
    zoneRange = 'Range',
    totalxP = 'Expected Points',
    xPPS = "xPPS",
    xShot_perc = "xFG%",
    XP_pct = 'xP PCT',
    xPPS_pct = 'xPPS PCT',
    xShot_perc_pct = 'xFG% PCT'
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_plt_bar_pct(column = XP_pct, scaled = TRUE, fill = '#17408B') %>%
  gt_plt_bar_pct(column = xPPS_pct, scaled = TRUE, fill = '#C9082A') %>%
  gt_plt_bar_pct(column = xShot_perc_pct, scaled = TRUE, fill = '#178b62') %>%
  cols_align("center", contains("scale")) %>%
  fmt_percent(
    columns = c(xShot_perc),
    decimals = 2
  ) %>%
  fmt_number(
    columns = 2,
    decimals = 0
  ) %>%
  fmt_number(
    columns = 3,
    decimals = 2
  ) %>%
  fmt_number(
    columns = 4,
    decimals = 2
  ) %>% 
  tab_footnote(
    footnote = 'Expected Points Per Shot Attempt',
    locations = cells_column_labels(
      columns = xPPS
    )
  ) %>% 
  tab_footnote(
    footnote = 'Expected Field Goal Percentage',
    locations = cells_column_labels(
      columns = xShot_perc
    )
  ) %>% 
  tab_source_note(
    source_note = "@aborelli24 on Twitter | Data: NBA"
  ) %>% gtsave('shotzone.png')





my.formula <- y ~ x
library(ggpmisc)
#plots of variables
dis <- ggplot(season22, aes(distanceShot, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "shot distance",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
dis

dunk <- ggplot(season22, aes(dunk, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "dunk",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
dunk


jumpshot <- ggplot(season22, aes(jumpshot, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "jumpshot",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
jumpshot

locY <- ggplot(season22, aes(locationY, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "loc Y",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
locY

totsecs <- ggplot(season22, aes(totalSecondsRemaining, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "Secs Rem",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
totsecs

locx <- ggplot(season22, aes(locationX, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "loc x",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
locx


layup <- ggplot(season22, aes(layup, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "layup",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
layup

paint <- ggplot(season22, aes(paint, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "paint",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
paint

midrange <- ggplot(season22, aes(midrange, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "midrange",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
midrange

backcourt <- ggplot(season22, aes(distanceShot, exp_make, fill = "blue")) +
  geom_point(size = 1) +
  geom_point(color = "#B5E48C") +
  geom_smooth(method = "glm",
              colour = 'black',
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(title = "backcourt",
       x = "",
       y = "",
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -3, family = "Karla"),
        axis.text.x = element_text(face = "bold", size = 8, family = "Karla"),
        axis.text.y = element_text(face = "bold", size = 8, family = "Karla"))
backcourt


tot <- grid.arrange(
  dis,
  dunk,
  jumpshot,
  locY,
  totsecs,
  locx,
  layup,
  paint,
  midrange,
  backcourt,
  nrow = 2,
  top = textGrob(
    "Expected Shot Make vs. Variables",
    gp = gpar(fontface = 3, fontsize = 16, fontfamily = "Karla")
  ),
  bottom = textGrob(
    "@aborelli24 on Twitter // NBA",
    gp = gpar(fontface = 3, fontsize = 10, fontfamily = "Karla"),
    hjust = 0.99,
    x = 0.99,
    vjust = 2,
    y = 2,
  )
) %>% ggsave('tot.png')
