library(downloader)
library(tidyverse)
library(htmltab)
library(XML)
library(caret)
library(e1071)
library(doParallel)
library(ggplot2)
library(sjmisc)
library(mclust)
library(ggradar)
library(scales)

install.packages('mclust')

temp_players <- tibble()

locs <- seq(0,29200,200)

for (loc in locs){
  print(loc)
  dest <- paste("https://www.hockey-reference.com/play-index/psl_finder.cgi?c2stat=&c4stat=&c2comp=gt&is_playoffs=N&order_by_asc=&birthyear_max=&birthyear_min=&c1comp=gt&year_min=1980&request=1&franch_id=&is_hof=&birth_country=&match=single&year_max=2018&c3comp=gt&season_end=-1&is_active=&c3stat=&lg_id=NHL&order_by=goals&season_start=1&c1val=&threshhold=5&c3val=&c2val=&am_team_id=&handed=&rookie=N&pos=S&describe_only=&c1stat=&draft=&c4val=&age_min=0&c4comp=gt&age_max=99&offset=",
                loc, sep="")
  
  temp <- htmltab(dest, which=1)
  
  temp_players <- temp_players %>% bind_rows(temp)
  
}

players <- temp_players %>% mutate(Tm = ifelse(Tm=="WIN","WPG", ifelse(Tm=="CBH", "CHI", ifelse(Tm=="MDA","ANA",Tm))), 
                                   Player = str_remove(Player, "\\*"), Pos =as.factor(Pos), Tm = as.factor(Tm), 
                                   Age = as.numeric(Age), Season=as.factor(Season), 
                          G=as.numeric(G), GP=as.numeric(GP), A = as.numeric(`Scoring >> A`), 
                          PTS = as.numeric(`Scoring >> PTS`), PM = as.numeric(`+/-`), TOI = as.numeric(TOI), 
                          S = as.numeric(S), SHG = as.numeric(`Goals >> SH`), PIM = as.numeric(PIM),
                          GWG = as.numeric(`Goals >> GW`)) %>% 
                  select (Player, Tm, Pos, Season, Age, GP, G, A, PTS, PM, PIM) %>% filter(Tm != "TOT")


players <- players %>% mutate(in80s = ifelse(Season %in% levels(players$Season)[1:10],1,0), 
                              in90s = ifelse(Season %in% levels(players$Season)[11:20],1,0),
                              in00s = ifelse(Season %in% levels(players$Season)[21:30],1,0),
                              in10s = ifelse(Season %in% levels(players$Season)[31:38],1,0),
                              decade = as.factor(ifelse(Season %in% levels(players$Season)[1:10],'80s',
                                              ifelse(Season %in% levels(players$Season)[11:20],'90s',
                                                     ifelse(Season %in% levels(players$Season)[21:30],'00s', '10s')))))


#write.csv(players, 'players.csv')

pc<- players %>% filter(GP>=25) %>%mutate(clust = d_clust$classification)

kplayers = kmeans(players, centers = 10)

players %>% group_by(Player) %>% summarise(min(Age), max(Age), career = max(Age) - min(Age))

ggplot(players %>% filter(Age <= 40), aes(Age)) +stat_ecdf(aes(colour=decade), geom='smooth')



yearly_goals <- players %>% group_by(Season) %>% summarise(total_goals = sum(G), mean_goals = mean(G), sd_goals = sd(G))

yearly_points <- players %>% group_by(Season) %>% summarise(total_goals = sum(G), total_assists = sum(A), apg = total_assists/total_goals)

ggplot(players %>% filter(Season == '2017-18' | Season == '1992-93'), aes(y=G, x=Season)) +geom_boxplot(aes(colour=Season))

ggplot(players, aes(GP)) +stat_ecdf(geom='step',aes(colour = Season))

ggplot(yearly_goals, aes(x=Season, y=n)) + geom_bar(stat='identity')

new <- players %>% filter(Season == '2016-17') %>% select(GP) %>% as.vector()
old <- players %>% filter(Season == '1990-91') %>% select(GP) %>% as.vector()

ks.test(new$GP,old$GP)

##KS Test shows that the null hypothesis is rejected - there is a signifcant difference in distributions
  
years <- seq(1980,2018,1)

years <- years[-which(years==2005)]

standings = tibble()
i=1

for (year in years) { 
  
  link <- paste("https://www.hockey-reference.com/leagues/NHL_",year,".html",sep="")
  
  east = htmltab(link, which =1)
  west = htmltab(link, which =2)
  
  league_temp = bind_rows(east,west)
  
  league_temp <- league_temp[which(!is.na(as.numeric(league_temp$GP))),]
  league_temp <- league_temp %>% mutate(Season = as.factor(levels(players$Season)[i]), Team = V1) %>% 
     mutate(inPlayoffs = str_detect(Team, "\\*"), Team = str_remove(Team, "\\*")) %>% select(-V1)

  standings = standings %>% bind_rows(league_temp)
  
  i=i+1
}


standings <- standings %>% mutate (Team = as.factor(Team)) %>% select(-OL, -`RPt%`, -ROW, -`PTS%`, -SRS, -SOS)


abbs <- read.csv('teamabbs.csv', header=F)

abb_key <- abbs %>% separate(V1, sep="\\–", into=c("Tm", "Team")) %>% mutate_all(funs(str_trim))

standings <- standings %>% left_join(abb_key) %>% mutate(PCT = `PTS%`)

#write.csv(standings, 'standings.csv')

#from standings I probably just need wins, points, goals for and goals against, inPlayoffs
#create a share of team goals calculation (goals/GF)
#maybe a share of team scoring (PTS/GF)

standings_to_merge <- standings %>% select(Tm, inPlayoffs, Season, PCT, PTS, GF) %>% mutate_at(.funs=funs(as.numeric), .vars=vars(PCT:GF)) %>%
                                mutate_at(.funs=funs(as.factor), .vars=vars(Tm:Season))

comb <- players %>% left_join(standings_to_merge, by=c('Tm', 'Season')) %>% rename(TeamPTS = PTS.y, PTS = PTS.x)

comb <- comb %>% mutate(Goal_Share = 100*G/GF, Point_Cont = 100*PTS/GF) %>% filter(Tm != "TOT")

data <- comb %>% filter(GP>=25) %>% select(GP, Pos, Age:PIM, Goal_Share, Point_Cont, inPlayoffs, PCT,Tm) %>% select(-PTS) %>%
  mutate(Gp100g = 100* G/GP, Ap100g = 100*A/GP, PMp100g = 100*PM/GP, PIMp100g = 100*PIM/GP) %>% select(-G,-A,-PM,-PIM)

data_w_names <- comb %>% filter(GP>=25) %>% select(Player, GP, Pos, Age:PIM, Goal_Share, Point_Cont, inPlayoffs, PCT,Tm) %>% select(-PTS) %>%
  mutate(Gp100g = 100* G/GP, Ap100g = 100*A/GP, PMp100g = 100*PM/GP, PIMp100g = 100*PIM/GP) %>% select(-G,-A,-PM,-PIM)

data <- data %>% mutate(clust = d_clust$classification)

data2 <- data_w_names %>% mutate(clust=d_clust$classification)

#write.csv(data, 'nhldata.csv')
#write.csv(data2, 'nhldata2names.csv')

clusts <- data2 %>% group_by(clust) %>% summarise(n=n(),age=mean(Age),goals = mean(Gp100g), assists = mean(Ap100g), PM = mean(PMp100g), PIM = mean(PIMp100g))

clusts %>%
  mutate_each(funs(rescale),-clust) %>% filter(goals>0.5 | assists >0.5) -> radar_clust

ggradar(radar_clust)


reg_data <- data %>% select(-inPlayoffs)
class_data <- data %>% select(-PCT)

model1 <- glm(PCT ~ ., data=reg_data, family=binomial())

model2 <- glm(inPlayoffs ~.+I(Age^2), data=class_data, family=binomial)

cluster_data <- data %>% select(-Pos, -inPlayoffs,-Tm)

scaled_data <- scale(cluster_data)

k.max <- 25

wss <- sapply(1:k.max, 
              function(k){kmeans(scaled_data, k,
                                 nstart = 50, iter.max=20)$tot.withinss})

plot(1:k.max, wss, type='b', pch=19, frame=FALSE)

d_clust <- Mclust(as.matrix(scaled_data), G=1:25, modelNames = mclust.options("emModelNames"))
d_clust$BIC

set.seed(321)
training <- sample(1:nrow(data), round(nrow(data)*.7))

reg_data_train <- reg_data[training,]
reg_data_test <- reg_data[-training,]
class_data_train <- class_data[training,]
class_data_test <- class_data[-training,]

kk <- kmeans(class_data_train, 5)

model1 <- lm(W ~ ., data=reg_data_train)

model2 <- glm(inPlayoffs~., data=class_data_train, family=binomial())


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePredictions = TRUE
)

gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.08,
                        n.minobsinnode = 15)

lreg<-train(inPlayoffs~.,data=class_data_train,method="glm",family=binomial(), trControl=fitControl)

cl <- makeCluster(detectCores()/2)
registerDoParallel(cl)

gbmfit1 <- train(inPlayoffs~.,data=class_data_train,method="gbm", trControl=fitControl, 
                 tuneGrid = gbmGrid, preProcess = c("center", "scale"))


model1 <- train(W~., data=reg_data_train, method="bridge")

sum(sapply(class_data_train, is.na))

sum(sapply(class_data_train, is.infinite))

sum(sapply(class_data_train, is.nan))

    
