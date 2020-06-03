library(reshape)
library(reshape2)
library(xgboost)
library(caret)
library(jsonlite)
library(dplyr)
library(Matrix)
library(doParallel)
library(lubridate)


library(readr)
members <- read_csv("~/Documents/hkust/kkbox-music-recommendation-challenge/members.csv")
songs <- read_csv("~/Documents/hkust/kkbox-music-recommendation-challenge/songs.csv")
test <- read_csv("~/Documents/hkust/kkbox-music-recommendation-challenge/test.csv")
train <- read_csv("~/Documents/hkust/kkbox-music-recommendation-challenge/train.csv")

train$target<-as.factor(train$target)
train$source_system_tab<-as.factor(train$source_system_tab)
train$source_type<- as.factor(train$source_type)
train$source_screen_name<- as.factor(train$source_screen_name)

#visualization
ggplot(train, aes(x = source_system_tab , fill = target)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'source_system_tab') +theme_grey()+theme(axis.text.x=element_text(angle=90, hjust=1))4

ggplot(train, aes(x = source_screen_name , fill = target)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'source_screen_name') +theme_grey()+theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(train, aes(x = source_type , fill = target)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'source_type') +theme_grey()+theme(axis.text.x=element_text(angle=90, hjust=1))

train %>%
  
  group_by(target) %>%
  
  summarise(percentage = n()/nrow(train)*100)

train %>%
  
  group_by(source_system_tab) %>%
  
  summarise(percentage = n()/nrow(train)*100)

train %>%
  
  group_by(source_screen_name) %>%
  
  summarise(percentage = n()/nrow(train)*100)

train %>%
  
  group_by(source_type) %>%
  
  summarise(percentage = n()/nrow(train)*100)

p1 <- train %>%
  
  ggplot(aes(target, fill = target)) +
  
  geom_bar() +
  
  theme(legend.position = "none")


p2 <- members %>%
  
  select(gender, msno) %>%
  
  filter(gender != "") %>%
  
  left_join(train, by = "msno") %>%
  
  filter(!is.na(target)) %>%
  
  group_by(gender, target) %>%
  
  count() %>%
  
  spread(target, n) %>%
  
  mutate(frac_churn = `1`/(`1`+`0`)*100) %>%
  
  ggplot(aes(gender, frac_churn, fill = gender)) +
  
  geom_col() +
  
  theme(legend.position = "none") +
  
  labs(x = "Gender", y = "Target [%]")


p3 <- members %>%
  
  select(registered_via, msno) %>%
  
  left_join(train, by = "msno") %>%
  
  filter(!is.na(target)) %>%
  
  group_by(registered_via, target) %>%
  
  count() %>%
  
  spread(target, n) %>%
  
  mutate(frac_churn = `1`/(`1`+`0`)*100) %>%
  
  ggplot(aes(reorder(registered_via, -frac_churn, FUN = max), frac_churn, fill = registered_via)) +
  
  geom_col()+
  
  theme(legend.position = "none") +
  
  labs(x = "Registration method", y = "Target [%]")


p4 <- members %>%
  
  select(bd, msno) %>%
  
  filter(bd > 0 & bd < 100) %>%
  
  left_join(train, by = "msno") %>%
  
  filter(!is.na(target)) %>%
  
  ggplot(aes(bd, fill = target)) +
  
  geom_density(bw = 1, alpha = 0.5) +
  
  labs(x = "Age - bd")

#data preparation
train$key<-paste(train$msno,train$song_id,sep="_")
test$key<-paste(test$msno,test$song_id,sep="_")

train$id<-row.names(train)

test$target<-''

train$type<-'train'
test$type<-'test'

train1<-train[,c('key','type','id','source_system_tab','source_screen_name','source_type','target')]
test1<-test[,c('key','type','id','source_system_tab','source_screen_name','source_type','target')]

master_df<-rbind(train1,test1)

#Creating source system tab based primary variables

master_df$flag_source_system_tab_discover<-ifelse(master_df$source_system_tab=="discover",1,0)
master_df$flag_source_system_tab_explore<-ifelse(master_df$source_system_tab=="explore",1,0)
master_df$flag_source_system_tab_listen_with<-ifelse(master_df$source_system_tab=="listen with",1,0)
master_df$flag_source_system_tab_my_library<-ifelse(master_df$source_system_tab=="my library",1,0)
master_df$flag_source_system_tab_notification<-ifelse(master_df$source_system_tab=="notification",1,0)
master_df$flag_source_system_tab_radio<-ifelse(master_df$source_system_tab=="radio",1,0)
master_df$flag_source_system_tab_search<-ifelse(master_df$source_system_tab=="search",1,0)
master_df$flag_source_system_tab_settings<-ifelse(master_df$source_system_tab=="settings",1,0)



#Creating source type based primary variables

master_df$flag_source_type_song<-ifelse(master_df$source_type=="song",1,0)
master_df$flag_source_type_song_based_playlist<-ifelse(master_df$source_type=="song-based-playlist",1,0)
master_df$flag_source_type_top_hits_for_artist<-ifelse(master_df$source_type=="top-hits-for-artist",1,0)
master_df$flag_source_type_topic_article_playlist<-ifelse(master_df$source_type=="topic-article-playlist",1,0)
master_df$flag_source_type_my_daily_playlist<-ifelse(master_df$source_type=="my-daily-playlist",1,0)
master_df$flag_source_type_online_playlist<-ifelse(master_df$source_type=="online-playlist",1,0)
master_df$flag_source_type_listen_with<-ifelse(master_df$source_type=="listen-with",1,0)
master_df$flag_source_type_local_library<-ifelse(master_df$source_type=="local-library",1,0)
master_df$flag_source_type_local_playlist<-ifelse(master_df$source_type=="local-playlist",1,0)
master_df$flag_source_type_album<-ifelse(master_df$source_type=="album",1,0)
master_df$flag_source_type_artist<-ifelse(master_df$source_type=="artist",1,0)


train_data<-master_df[master_df$type=='train',]
test_data<-master_df[master_df$type=='test',]


#training and test data creation
train_data_xgb1<-master_df[master_df$type=="train",c(8:ncol(master_df))]
test_data_xgb1<-master_df[master_df$type=="test",c(8:ncol(master_df))]


train_data$target <- factor(train_data$target, levels = c(0,1), ordered = TRUE)
ydata <- as.numeric(train_data$target)-1

xdata <- Matrix(as.matrix(train_data_xgb1), sparse = TRUE)

xdata_test_final <- Matrix(as.matrix(test_data_xgb1), sparse = TRUE)


# xgboost parameters
param <- list("objective" = "multi:softprob",   
              "num_class" = 2,    
              "eval_metric" = "merror",  
              "nthread" = 8,  
              "max_depth" = 16,    
              "eta" = 0.3,   
              "gamma" = 0,     
              "subsample" = 1,  
              "colsample_bytree" = 1,  
              "min_child_weight" = 12)

bst.cv <- xgb.cv(param=param, data=xdata, label=ydata, 
                 nfold=2, nrounds=30, prediction=TRUE, verbose=TRUE)


min.merror.idx = which.min(bst.cv$evaluation_log[,test_merror_mean]) 

xgb <- xgboost(param=param, data=xdata, label=ydata,
               nrounds=min.merror.idx, verbose=TRUE)


pred_xgb <- predict(xgb, xdata_test_final, reshape = TRUE)


pred_xgb2 <- as.data.frame(pred_xgb)
names(pred_xgb2) <- c("zero","one")
pred_xgb2$id <- test_data$id

pred_xgb2$target <- ifelse(pred_xgb2$zero>pred_xgb2$one,0,1)

pred_xgb3<-pred_xgb2[,c(3,4)]

write.csv(pred_xgb3, "predictions.csv", row.names = FALSE)

