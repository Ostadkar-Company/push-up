setwd("~/myProjects/210601_ostadkar_r1400_2/feature_selection/")


library(dplyr)
library(ggplot2)
library(viridis)
library(data.table)
library(chron)


setwd("~/myProjects/210601_ostadkar_r1400_2/")

mydata <-
  read.csv("Cleaning_data_extracted-r1400-02_B.csv")

head(mydata)

table(mydata$previous_status)


table(mydata$order_status)


df_status_count <-
  mydata %>%
  group_by(previous_status, order_status )%>%
  summarise(n=n())
df_status_count <-
  as.data.frame(df_status_count)

df_status_count$previous_status <- as.factor(df_status_count$previous_status)
df_status_count$order_status <- as.factor(df_status_count$order_status)

df_status_count$freq <-
  df_status_count$n / (sum(df_status_count$n))
ggplot(df_status_count, aes(x=previous_status, y= order_status)) +
  geom_tile(aes(fill=n), color='gray50') +
  geom_text(aes(label = n)) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_fill_viridis()


ggsave('status_progression_heatmap.png', dpi = 500, width = 6, height = 4)



ggplot(df_status_count, aes(x=previous_status, y= order_status)) +
  geom_tile(aes(fill=freq), color='gray50') +
  geom_text(aes(label = format(freq, digits = 2,scientific = TRUE)),
            color = 'gray50', size =3) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_fill_viridis()

#ggsave('status_progression_heatmap_freq.png', dpi = 500, width = 8, height = 8)

####################################
dim(mydata)
length(unique(mydata$order_number))

#some orders (10%) have repetitive entries, 

df_rep <- mydata[duplicated(mydata$order_number) | duplicated(mydata$order_number, fromLast=TRUE),]
df_rep[order(df_rep$order_number),]
#we will keep the first occurance


mydata_f <-
  mydata %>%
  group_by(order_number) %>%
  arrange(desc(created_at)) %>%
  filter(row_number()==1 )

dim(mydata_f)
mydata <-
  mydata_f
mydata <- as.data.frame(mydata)
mydata$wq_to_qa_time_h <-
  mydata$wq_to_qa_time/(3600)


#we need to label the rows:
#I) remained at "need quote" stage 
#II) proceeded to "with quote" stage
#II) unclear


df_not_wquote <-
  mydata[mydata$previous_status == 'NeedQuote' &
           mydata$order_status == 'Canceled', ]


df_wquote <-
  mydata[mydata$previous_status == 'withQuote' |
           mydata$previous_status == 'Done' |
           mydata$previous_status == 'Started' |
           mydata$previous_status == 'QuoteAccepted' |
           mydata$previous_status == 'QuoteRejected' |
           mydata$previous_status == 'Finished' , ]

arrow = arrow(angle=15, type = "closed")
n_censored <- nrow(df_wquote[df_wquote$wq_to_qa_time>600,])
n_shown <- nrow(df_wquote[df_wquote$wq_to_qa_time<=600,])

ggplot(df_wquote[df_wquote$wq_to_qa_time>-200&
                   df_wquote$wq_to_qa_time<60*10,], aes(x=wq_to_qa_time)) +
  geom_histogram(bins =100)+
  theme_bw()+
  annotate("segment", x = 520, xend = 600, y =270, yend = 270, arrow=arrow)+
  annotate("text", label = paste0(n_censored, " data points censored\n\n", n_shown, " data points shown"), 
           x = 540, y = 270) +
  geom_vline(xintercept = 15, linetype=2, color = "#0072B2")+
  annotate("text", label = 'automatic qa generation\nthreshold time = 15 seconds', 
           x = 60, y = 1500, color = "#0072B2") +
  geom_vline(xintercept = 200, linetype=2, color = "#D55E00")+
  annotate("text", label = 'automatic qa generation\nthreshold time = 200 seconds', 
           x = 240, y = 1000, color = "#D55E00")

ggsave('distribution_wq_to_wq_time.png', dpi=500, width = 8, height = 6)

nrow(df_wquote[is.na(df_wquote$wq_to_qa_time),])

nrow(df_wquote[df_wquote$wq_to_qa_time <0 ,])

df_wquote_operator <-
  df_wquote[df_wquote$wq_to_qa_time<200,]
df_wquote_organic <-
  df_wquote[df_wquote$wq_to_qa_time>=200,]


df_wquote_operator2 <-
  df_wquote[df_wquote$wq_to_qa_time<15,]
df_wquote_organic2 <-
  df_wquote[df_wquote$wq_to_qa_time>=15,]


dim(df_not_wquote)
dim(df_wquote_operator)
dim(df_wquote_organic)
################################################################
df_wquote1 <-df_wquote_organic
df_not_wquote1 <- rbind(df_not_wquote, df_wquote_operator)
df_wquote2 <-df_wquote_organic2
df_not_wquote2 <- rbind(df_not_wquote, df_wquote_operator2)

dim(df_wquote1)
dim(df_not_wquote1)

dim(df_wquote2)
dim(df_not_wquote2)


df_not_wquote1$label_wn_quote <- 'nq'
df_wquote1$label_wn_quote <- 'wq'

df_not_wquote2$label_wn_quote <- 'nq'
df_wquote2$label_wn_quote <- 'wq'


#conversion rate

nrow(df_not_wquote1) / (nrow(df_not_wquote1) + nrow(df_wquote1))

nrow(df_not_wquote2) / (nrow(df_not_wquote2) + nrow(df_wquote2))


mydf2 <- rbind(df_not_wquote1, df_wquote1)
mydf2b <- rbind(df_not_wquote2, df_wquote2)

mydf2 <- mydf2[,c('label_wn_quote', 'is_delivery_at_holiday', 'is_created_at_holiday', 
                  'duration', 'gender', 'area_cleaning', 'day_of_week_delivery',
                  'day_of_week_created','total_cost', 'all_offers', 'corporate_offers',
                  'individual_offers', 'offers_busy', 'district',
                  'past_day_all_orders', 'past_day_wq_1h', 'past_day_wq_2h',
                  'past_day_wq_3h')]

mydf2b <- as.data.table(mydf2b)
mydf2c <-
  mydf2b[mydf2b$wq_to_qa_time>0|is.na(mydf2b$wq_to_qa_time),]



mydf2b <- mydf2b[,c('label_wn_quote', 'is_delivery_at_holiday', 'is_created_at_holiday', 
                    'duration', 'gender', 'area_cleaning', 'day_of_week_delivery',
                    'day_of_week_created','total_cost', 'all_offers', 'corporate_offers',
                    'individual_offers', 'offers_busy', 'district',
                    'past_day_all_orders', 'past_day_wq_1h', 'past_day_wq_2h',
                    'past_day_wq_3h')]

(nrow(mydf2b) - nrow(mydf2c))/nrow(mydf2b)
mydf2c <- mydf2c[,c('label_wn_quote', 'is_delivery_at_holiday', 'is_created_at_holiday', 
                    'duration', 'gender', 'area_cleaning', 'day_of_week_delivery',
                    'day_of_week_created','total_cost', 'all_offers', 'corporate_offers',
                    'individual_offers', 'offers_busy', 'district',
                    'past_day_all_orders', 'past_day_wq_1h', 'past_day_wq_2h',
                    'past_day_wq_3h')]

mydf2b$cost_to_area <- mydf2b$total_cost / mydf2b$area_cleaning
mydf2b$cost_times_area <- mydf2b$total_cost * mydf2b$area_cleaning
mydf2b$duration_to_area <- mydf2b$duration / mydf2b$area_cleaning
mydf2b$duration_times_area <- mydf2b$duration * mydf2b$area_cleaning
mydf2b$indiv_offers_to_all_offers <- mydf2b$individual_offers / mydf2b$all_offers
mydf2b$corp_offers_to_all_offers <- mydf2b$corporate_offers / mydf2b$all_offers
mydf2b$busy_offers_to_all_offers <- mydf2b$offers_busy / mydf2b$all_offers



mydf2c$cost_to_area <- mydf2c$total_cost / mydf2c$area_cleaning
mydf2c$cost_times_area <- mydf2c$total_cost * mydf2c$area_cleaning
mydf2c$duration_to_area <- mydf2c$duration / mydf2c$area_cleaning
mydf2c$duration_times_area <- mydf2c$duration * mydf2c$area_cleaning
mydf2c$indiv_offers_to_all_offers <- mydf2c$individual_offers / mydf2c$all_offers
mydf2c$corp_offers_to_all_offers <- mydf2c$corporate_offers / mydf2c$all_offers
mydf2c$busy_offers_to_all_offers <- mydf2c$offers_busy / mydf2c$all_offers



mydf2$label_wn_quote <- as.factor(mydf2$label_wn_quote)
mydf2$is_delivery_at_holiday <- as.factor(mydf2$is_delivery_at_holiday)
mydf2$is_created_at_holiday <- as.factor(mydf2$is_created_at_holiday)
mydf2$district <- as.factor(mydf2$district)
#
mydf2$label_wn_quote <- as.numeric(mydf2$label_wn_quote=='wq') 
mydf2$is_delivery_at_holiday <- as.numeric(mydf2$is_delivery_at_holiday) -1
mydf2$is_created_at_holiday <- as.numeric(mydf2$is_created_at_holiday) -1


mydf2b$label_wn_quote <- as.factor(mydf2b$label_wn_quote)
mydf2b$is_delivery_at_holiday <- as.factor(mydf2b$is_delivery_at_holiday)
mydf2b$is_created_at_holiday <- as.factor(mydf2b$is_created_at_holiday)
mydf2b$district <- as.factor(mydf2b$district)

mydf2b$label_wn_quote <- as.numeric(mydf2b$label_wn_quote) -1
mydf2b$is_delivery_at_holiday <- as.numeric(mydf2b$is_delivery_at_holiday) -1
mydf2b$is_created_at_holiday <- as.numeric(mydf2b$is_created_at_holiday) -1


mydf2c$label_wn_quote <- as.factor(mydf2c$label_wn_quote)
mydf2c$is_delivery_at_holiday <- as.factor(mydf2c$is_delivery_at_holiday)
mydf2c$is_created_at_holiday <- as.factor(mydf2c$is_created_at_holiday)
mydf2c$district <- as.factor(mydf2c$district)

mydf2c$label_wn_quote <- as.numeric(mydf2c$label_wn_quote) -1
mydf2c$is_delivery_at_holiday <- as.numeric(mydf2c$is_delivery_at_holiday) -1
mydf2c$is_created_at_holiday <- as.numeric(mydf2c$is_created_at_holiday) -1
###############################
###############################
mydf2 <- as.data.table(mydf2)
mydf2b <- as.data.table(mydf2b)
mydf2c <- as.data.table(mydf2c)

mydf2[is.na(mydf2$gender),'gender'] <- 0 
mydf2c[is.na(mydf2c$gender),'gender'] <- 0

mydf2$gender <- as.factor(mydf2$gender )
mydf2b$gender <- as.factor(mydf2b$gender )
mydf2c$gender <- as.factor(mydf2c$gender )


mydf2[is.na(mydf2$area_cleaning),'area_cleaning'] <- -1 
mydf2b[is.na(mydf2b$area_cleaning),'area_cleaning'] <- -1
mydf2c[is.na(mydf2c$area_cleaning),'area_cleaning'] <- -1

mydf2[is.na(mydf2$total_cost), 'total_cost'] <- -10000
mydf2b[is.na(mydf2b$total_cost), 'total_cost'] <- -10000
mydf2c[is.na(mydf2c$total_cost), 'total_cost'] <- -10000



mydf2[mydf2$total_cost > 500000, 'total_cost'] <- 500000
mydf2b[mydf2b$total_cost > 500000, 'total_cost'] <- 500000
mydf2c[mydf2c$total_cost > 500000, 'total_cost'] <- 500000

mydf2$past_day_wq_3h <- as.numeric(mydf2$past_day_wq_3h)
mydf2$past_day_wq_2h <- as.numeric(mydf2$past_day_wq_2h)
mydf2$past_day_wq_1h <- as.numeric(mydf2$past_day_wq_1h)
mydf2$past_day_all_orders <- as.numeric(mydf2$past_day_all_orders)

mydf2b$past_day_wq_3h <- as.numeric(mydf2b$past_day_wq_3h)
mydf2b$past_day_wq_2h <- as.numeric(mydf2b$past_day_wq_2h)
mydf2b$past_day_wq_1h <- as.numeric(mydf2b$past_day_wq_1h)
mydf2b$past_day_all_orders <- as.numeric(mydf2b$past_day_all_orders)

mydf2c$past_day_wq_3h <- as.numeric(mydf2c$past_day_wq_3h)
mydf2c$past_day_wq_2h <- as.numeric(mydf2c$past_day_wq_2h)
mydf2c$past_day_wq_1h <- as.numeric(mydf2c$past_day_wq_1h)
mydf2c$past_day_all_orders <- as.numeric(mydf2c$past_day_all_orders)

##
mydf2b$day_of_week_created <- as.factor(mydf2b$day_of_week_created)
mydf2b$day_of_week_delivery <- as.factor(mydf2b$day_of_week_delivery)


mydf2c$day_of_week_created <- as.factor(mydf2c$day_of_week_created)
mydf2c$day_of_week_delivery <- as.factor(mydf2c$day_of_week_delivery)

mydf2$day_of_week_created <- as.factor(mydf2$day_of_week_created)
mydf2$day_of_week_delivery <- as.factor(mydf2$day_of_week_delivery)

#four entries have NA as duration, will drop them
mydf2 <- mydf2[!is.na(mydf2$duration), ]
mydf2b <- mydf2b[!is.na(mydf2b$duration), ]
mydf2c <- mydf2c[!is.na(mydf2c$duration), ]

mydf2 <- as.data.frame(mydf2)
mydf2b <- as.data.frame(mydf2b)
mydf2c <- as.data.frame(mydf2c)

#write.csv(mydf2c, "ostadkar_housecleaning_withQuote_needQuote.csv", row.names = F)

########################################################################


library(xgboost)
set.seed(101) # Set Seed so that same sample can be reproduced in future also
mydf2 <- mydf2[sample(nrow(mydf2)),]
mydf2b <- mydf2b[sample(nrow(mydf2b)),]
mydf2c <- mydf2c[sample(nrow(mydf2c)),]



sample <- sample.int(n = nrow(mydf2), size = floor(.75*nrow(mydf2)), replace = F)
sampleb <- sample.int(n = nrow(mydf2b), size = floor(.75*nrow(mydf2b)), replace = F)
samplec <- sample.int(n = nrow(mydf2c), size = floor(.75*nrow(mydf2c)), replace = F)


mydf2b_sub <- mydf2b[sampleb, ]
mydf2c_sub <- mydf2c[samplec, ]

####################################
#determining label counts

dim(mydf2c)

nrow(mydf2c[mydf2c$label_wn_quote==1,])/ (nrow(mydf2c))


nrow(mydf2c[mydf2c$label_wn_quote==0,])/ (nrow(mydf2c))




######################################
train <- mydf2[sample, ]
test  <- mydf2[-sample, ]
trainb <- mydf2b[sampleb, ]
testb  <- mydf2b[-sampleb, ]
trainc <- mydf2c[samplec, ]
testc  <- mydf2c[-samplec, ]


x_train = train[,-grep('label_wn_quote', colnames(train))]
y_train = train[,c('label_wn_quote')]

x_test = test[,-grep('label_wn_quote', colnames(test))]
y_test = test[,c('label_wn_quote')]


x_trainb = trainb[,-grep('label_wn_quote', colnames(trainb))]
y_trainb = trainb[,c('label_wn_quote')]

x_testb = testb[,-grep('label_wn_quote', colnames(testb))]
y_testb = testb[,c('label_wn_quote')]


x_trainc = trainc[,-grep('label_wn_quote', colnames(trainc))]
y_trainc = trainc[,c('label_wn_quote')]

x_testc = testc[,-grep('label_wn_quote', colnames(testc))]
y_testc = testc[,c('label_wn_quote')]

#################################



new_features <-
  c('cost_to_area','cost_times_area','duration_to_area','duration_times_area','indiv_offers_to_all_offers','corp_offers_to_all_offers','busy_offers_to_all_offers')

set.seed(1)
xgboostModelCV <- xgb.cv(data =  data.matrix(mydf2b_sub[,!colnames(mydf2b_sub)%in%c('label_wn_quote', new_features)]), nrounds = 6000, nfold = 5, showsd = TRUE, 
                         metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                         "label" = mydf2b_sub$label_wn_quote,
                         "objective" = "binary:logistic", "max.depth" = 9, "eta" = 1,                               
                         "subsample" = 1, "colsample_bytree" = 1
                         , print_every_n = 10, "min_child_weight" = 0.3, booster = "gbtree",
                         "stratified" = 0,
                         early_stopping_rounds = 10)



xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
rmse_b1 <- tail(xvalidationScores$test_rmse_mean, 1)
trmse_b1 <- tail(xvalidationScores$train_rmse_mean,1)
auc_b1 <- tail(xvalidationScores$test_auc_mean, 1)
tauc_b1 <- tail(xvalidationScores$train_auc_mean,1)

##########################################


##############################################################
##############################################################
set.seed(1)
xgboostModelCV <- xgb.cv(data =  data.matrix(mydf2b_sub[,-grep('label_wn_quote', colnames(mydf2b_sub))]), nrounds = 6000, nfold = 5, showsd = TRUE, 
                         metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                         "label" = mydf2b_sub$label_wn_quote,
                         "objective" = "binary:logistic", "max.depth" = 9, "eta" = 1,                               
                         "subsample" = 1, "colsample_bytree" = 1
                         , print_every_n = 10, "min_child_weight" = 0.3, booster = "gbtree",
                         "stratified" = 0,
                         early_stopping_rounds = 10)



xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
rmse_b <- tail(xvalidationScores$test_rmse_mean, 1)
trmse_b <- tail(xvalidationScores$train_rmse_mean,1)
auc_b <- tail(xvalidationScores$test_auc_mean, 1)
tauc_b <- tail(xvalidationScores$train_auc_mean,1)
trmse_b
tauc_b

#performance improved by adding new features
trmse_b1 - trmse_b

##################################
set.seed(1)
xgboostModelCV <- xgb.cv(data =  data.matrix(mydf2c_sub[,-grep('label_wn_quote', colnames(mydf2c_sub))]), nrounds = 6000, nfold = 5, showsd = TRUE, 
                         metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                         "label" = mydf2c_sub$label_wn_quote,
                         "objective" = "binary:logistic", "max.depth" = 9, "eta" = 1,                               
                         "subsample" = 1, "colsample_bytree" = 1
                         , print_every_n = 10, "min_child_weight" = 0.3, booster = "gbtree",
                         "stratified" = 0,
                         early_stopping_rounds = 10)



xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
rmse_c <- tail(xvalidationScores$test_rmse_mean, 1)
trmse_c <- tail(xvalidationScores$train_rmse_mean,1)
auc_c <- tail(xvalidationScores$test_auc_mean, 1)
tauc_c <- tail(xvalidationScores$train_auc_mean,1)
trmse_c
tauc_c

#removing data points with negative wq_to_wq_time
trmse_b - trmse_c

################################################


set.seed(1)
xgboostModelCV <- xgb.cv(data =  data.matrix(mydf2c_sub[,!colnames(mydf2c_sub)%in%c('label_wn_quote', new_features)]), nrounds = 6000, nfold = 5, showsd = TRUE, 
                         metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                         "label" = mydf2c_sub$label_wn_quote,
                         "objective" = "binary:logistic", "max.depth" = 9, "eta" = 1,                               
                         "subsample" = 1, "colsample_bytree" = 1
                         , print_every_n = 10, "min_child_weight" = 0.3, booster = "gbtree",
                         "stratified" = 0,
                         early_stopping_rounds = 10)



xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
rmse_c1 <- tail(xvalidationScores$test_rmse_mean, 1)
trmse_c1 <- tail(xvalidationScores$train_rmse_mean,1)
auc_c1 <- tail(xvalidationScores$test_auc_mean, 1)
tauc_c1 <- tail(xvalidationScores$train_auc_mean,1)
trmse_c1
tauc_c1


#trmse_b1 - trmse_c1



trmse_c1 - trmse_c

set.seed(1)
xgboostModelCV <- xgb.cv(data =  data.matrix(mydf2c_sub[,-grep('label_wn_quote', colnames(mydf2c_sub))]), nrounds = 6000, nfold = 5, showsd = TRUE, 
                         metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                         "label" = mydf2c_sub$label_wn_quote,
                         "objective" = "binary:logistic", "max.depth" = 25, "eta" = 1,                               
                         "subsample" = 1, "colsample_bytree" = 1
                         , print_every_n = 10, "min_child_weight" = 0.1, booster = "gbtree",
                         "stratified" = 0,
                         early_stopping_rounds = 10)



xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
rmse_c2 <- tail(xvalidationScores$test_rmse_mean, 1)
trmse_c2 <- tail(xvalidationScores$train_rmse_mean,1)
auc_c2 <- tail(xvalidationScores$test_auc_mean, 1)
tauc_c2 <- tail(xvalidationScores$train_auc_mean,1)
trmse_c2
tauc_c2

#parameters extrapolated from the grid search
trmse_c - trmse_c2
##################################

set.seed(1)
xgboostModelCV <- xgb.cv(data =  data.matrix(mydf2c_sub[,-grep('label_wn_quote', colnames(mydf2c_sub))]), nrounds = 6000, nfold = 5, showsd = TRUE, 
                         metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                         "label" = mydf2c_sub$label_wn_quote,
                         "objective" = "binary:logistic", "max.depth" = 25, "eta" = 1,                               
                         "subsample" = 1, "colsample_bytree" = 1
                         , print_every_n = 10, "min_child_weight" = 0.1, booster = "gbtree",
                         "stratified" = 0,
                         early_stopping_rounds = 50)


xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
rmse_c3 <- tail(xvalidationScores$test_rmse_mean, 1)
trmse_c3 <- tail(xvalidationScores$train_rmse_mean,1)
auc_c3 <- tail(xvalidationScores$test_auc_mean, 1)
tauc_c3 <- tail(xvalidationScores$train_auc_mean,1)
trmse_c3
tauc_c3

trmse_c2 - trmse_c3




######################################




library(caret)

xgb <- xgboost(data = data.matrix(mydf2c_sub[,-grep('label_wn_quote', colnames(mydf2c_sub))]), 
                               label = mydf2c_sub$label_wn_quote, 
                               eta = 1,
                              max_depth = 25, 
                               nround=6000, 
                               subsample = 1,
                               colsample_bytree = 1,
                               seed = 1,
                               eval_metric = "auc",
                               objective = "binary:logistic",
                              min_child_weight = 0.1,
                    min_child_weight = 0.1,
               "stratified" = 0,
                            early_stopping_rounds = 50
               )               
               
               
y_pred <- predict(xgb, data.matrix(x_testb))
y_pred_round <- rep(-1,length(y_pred))
y_pred_round[y_pred<0.5] <- 0
y_pred_round[y_pred>=0.5] <- 1



confusion_matrix <- confusionMatrix(data=as.factor(y_pred_round), reference = as.factor(y_testb))
# 
confusion_matrix 




####################################
importance <- xgb.importance(feature_names = setdiff(colnames(mydf2c_sub),'label_wn_quote'), model = xgb)
importance
###############################

x <- rbind(x_trainc, x_testc)
y <- c(y_trainc, y_testc)

feature_vec = setdiff(colnames(x_trainb),'label_wn_quote')
get_trmse <- function(feature_vec) {
  
  #feature_vec <- setdiff(feature_vec, feature_vec[1])
  #cur_x_train <-
  #  x_trainc[,feature_vec]
  #cur_x_test <-
  #  x_testc[, feature_vec]
  
  cur_x <-
    mydf2c_sub[,feature_vec]
  
  xgboostModelCV <- xgb.cv(data =  data.matrix(cur_x), nrounds = 6000, nfold = 5, showsd = TRUE, 
                           metrics = "rmse", verbose = F, "eval_metric" = "rmse",
                           "label" = mydf2c_sub$label_wn_quote,
                           "objective" = "binary:logistic", "max.depth" = 25, "eta" = 1,                               
                           "subsample" = 1, "colsample_bytree" = 1,
                            "min_child_weight" = 0.1, booster = "gbtree",
                           #num_parallel_tree = 10,
                           "stratified" = 0,
                           early_stopping_rounds = 50)
  
  
  
  xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
  #rmse <- tail(xvalidationScores$test_rmse_mean, 1)
  trmse <- tail(xvalidationScores$train_rmse_mean,1) 
  return(trmse)
}


#get_trmse(feature_vec)

set.seed(1)
get_optimal_features <- function(in_features){
  #in_features = colnames(mydf2c_sub)
  cur_trmse <- get_trmse(in_features)
  
  print('current model has these features:')
  print(in_features)
  print('-----')
  #print('assessing performance dropiing this feature:')
  print('with trmse: ')
  print(cur_trmse)
  min_trmse = cur_trmse
  feature_to_drop = '-1'
  for(cur_feature in in_features){
    new_trmse = get_trmse(setdiff(in_features, cur_feature))
    print(paste0('trmse when dropping ', cur_feature, ' :'))
    print(new_trmse)
    if(min_trmse>=new_trmse){
      min_trmse = new_trmse
      feature_to_drop = cur_feature
    }
    
  }
  
  if(feature_to_drop!='-1'){
    print('-----')
    print(paste0("dropping ", cur_feature))
    print('-----')
    return(get_optimal_features(setdiff(in_features, cur_feature)))
  }
  if(feature_to_drop=='-1'){
    print('found optimal feature set')
    return(in_features)
  }
  
  
}

get_optimal_features(setdiff(colnames(mydf2c_sub), 'label_wn_quote'))
