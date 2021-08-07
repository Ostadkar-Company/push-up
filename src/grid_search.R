library(dplyr)
library(ggplot2)
library(viridis)
library(data.table)
library(chron)


setwd("~/myProjects/210601_ostadkar_r1400_2/")

#mydata <-
#  read.csv("Cleaning_data_extracted-r1400-02.csv")
mydata <-
  read.csv("Cleaning_data_extracted-r1400-02_B.csv")

head(mydata)

table(mydata$previous_status)


table(mydata$order_status)
##########################################
#determining start - end date
mydata_b <- mydata
mydata_b$created_at_posix <-
  as.POSIXct(mydata_b$created_at, format = "%Y-%m-%d %H:%M:%S") 

min(mydata_b$created_at_posix)

max(mydata_b$created_at_posix)



######################################

df_status_count <-
  mydata %>%
  group_by(previous_status, order_status )%>%
  summarise(n=n())
df_status_count <-
  as.data.frame(df_status_count)

df_status_count$previous_status <- as.factor(df_status_count$previous_status)
df_status_count$order_status <- as.factor(df_status_count$order_status)


ggplot(df_status_count, aes(x=previous_status, y= order_status)) +
  geom_tile(aes(fill=n), color='gray50') +
  geom_text(aes(label = n)) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_fill_viridis()


ggsave('status_progression_heatmap.png', dpi = 500, width = 6, height = 4)

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

mydf2b <- mydf2b[,c('label_wn_quote', 'is_delivery_at_holiday', 'is_created_at_holiday', 
                  'duration', 'gender', 'area_cleaning', 'day_of_week_delivery',
                  'day_of_week_created','total_cost', 'all_offers', 'corporate_offers',
                  'individual_offers', 'offers_busy', 'district',
                  'past_day_all_orders', 'past_day_wq_1h', 'past_day_wq_2h',
                  'past_day_wq_3h')]
  
mydf2$label_wn_quote <- as.factor(mydf2$label_wn_quote)
mydf2$is_delivery_at_holiday <- as.factor(mydf2$is_delivery_at_holiday)
mydf2$is_created_at_holiday <- as.factor(mydf2$is_created_at_holiday)
mydf2$district <- as.factor(mydf2$district)

mydf2$label_wn_quote <- as.numeric(mydf2$label_wn_quote) -1
mydf2$is_delivery_at_holiday <- as.numeric(mydf2$is_delivery_at_holiday) -1
mydf2$is_created_at_holiday <- as.numeric(mydf2$is_created_at_holiday) -1


mydf2b$label_wn_quote <- as.factor(mydf2b$label_wn_quote)
mydf2b$is_delivery_at_holiday <- as.factor(mydf2b$is_delivery_at_holiday)
mydf2b$is_created_at_holiday <- as.factor(mydf2b$is_created_at_holiday)
mydf2b$district <- as.factor(mydf2b$district)

mydf2b$label_wn_quote <- as.numeric(mydf2b$label_wn_quote) -1
mydf2b$is_delivery_at_holiday <- as.numeric(mydf2b$is_delivery_at_holiday) -1
mydf2b$is_created_at_holiday <- as.numeric(mydf2b$is_created_at_holiday) -1
###############################
mydf2 <- as.data.table(mydf2)
mydf2b <- as.data.table(mydf2b)

mydf2[is.na(mydf2$gender),'gender'] <- 0 
mydf2b[is.na(mydf2b$gender),'gender'] <- 0

mydf2$gender <- as.factor(mydf2$gender )
mydf2b$gender <- as.factor(mydf2b$gender )


mydf2[is.na(mydf2$area_cleaning),'area_cleaning'] <- -1 
mydf2b[is.na(mydf2b$area_cleaning),'area_cleaning'] <- -1

mydf2[is.na(mydf2$total_cost), 'total_cost'] <- -10000
mydf2b[is.na(mydf2b$total_cost), 'total_cost'] <- -10000



mydf2[mydf2$total_cost > 500000, 'total_cost'] <- 500000
mydf2b[mydf2b$total_cost > 500000, 'total_cost'] <- 500000

mydf2$past_day_wq_3h <- as.numeric(mydf2$past_day_wq_3h)
mydf2$past_day_wq_2h <- as.numeric(mydf2$past_day_wq_2h)
mydf2$past_day_wq_1h <- as.numeric(mydf2$past_day_wq_1h)
mydf2$past_day_all_orders <- as.numeric(mydf2$past_day_all_orders)

mydf2b$past_day_wq_3h <- as.numeric(mydf2b$past_day_wq_3h)
mydf2b$past_day_wq_2h <- as.numeric(mydf2b$past_day_wq_2h)
mydf2b$past_day_wq_1h <- as.numeric(mydf2b$past_day_wq_1h)
mydf2b$past_day_all_orders <- as.numeric(mydf2b$past_day_all_orders)


mydf2b$day_of_week_created <- as.factor(mydf2b$day_of_week_created)
mydf2b$day_of_week_delivery <- as.factor(mydf2b$day_of_week_delivery)

mydf2$day_of_week_created <- as.factor(mydf2$day_of_week_created)
mydf2$day_of_week_delivery <- as.factor(mydf2$day_of_week_delivery)

#four entries have NA as duration, will drop them
mydf2 <- mydf2[!is.na(mydf2$duration), ]
mydf2b <- mydf2b[!is.na(mydf2b$duration), ]

mydf2 <- as.data.frame(mydf2)
mydf2b <- as.data.frame(mydf2b)
########################################################################



library(xgboost)
set.seed(101) # Set Seed so that same sample can be reproduced in future also
mydf2 <- mydf2[sample(nrow(mydf2)),]
mydf2b <- mydf2b[sample(nrow(mydf2b)),]

sample <- sample.int(n = nrow(mydf2), size = floor(.75*nrow(mydf2)), replace = F)
sampleb <- sample.int(n = nrow(mydf2b), size = floor(.75*nrow(mydf2b)), replace = F)


train <- mydf2[sample, ]
test  <- mydf2[-sample, ]
trainb <- mydf2b[sampleb, ]
testb  <- mydf2b[-sampleb, ]


x_train = train[,-grep('label_wn_quote', colnames(train))]
y_train = train[,c('label_wn_quote')]

x_test = test[,-grep('label_wn_quote', colnames(test))]
y_test = test[,c('label_wn_quote')]


x_trainb = trainb[,-grep('label_wn_quote', colnames(trainb))]
y_trainb = trainb[,c('label_wn_quote')]

x_testb = testb[,-grep('label_wn_quote', colnames(testb))]
y_testb = testb[,c('label_wn_quote')]

# 
# xgb <- xgboost(data = data.matrix(x_train), 
#                label = y_train, 
#                #eta = 0.1,
#                #max_depth = 15, 
#                nround=3000, 
#                #subsample = 0.5,
#                #colsample_bytree = 0.5,
#                seed = 1,
#                #eval_metric = "merror",
#                #objective = "multi:softprob",
#                num_class = 1,
#                nthread = 5
# )               
# 
# 
# xgb2 <- xgboost(data = data.matrix(x_trainb), 
#                label = y_trainb, 
#                #eta = 0.1,
#                #max_depth = 15, 
#                nround=3000, 
#                #subsample = 0.5,
#                #colsample_bytree = 0.5,
#                seed = 1,
#                #eval_metric = "merror",
#                #objective = "multi:softprob",
#                num_class = 1,
#                nthread = 5
# )               
# 
# y_pred <- predict(xgb, data.matrix(x_test))
# y_pred2 <- predict(xgb2, data.matrix(x_testb))
# 
# 
# y_pred_round <- rep(-1,length(y_pred))
# y_pred_round[y_pred<0.5] <- 0
# y_pred_round[y_pred>=0.5] <- 1
# 
# y_pred_round2 <- rep(-1,length(y_pred2))
# y_pred_round2[y_pred2<.5] <- 0
# y_pred_round2[y_pred2>=.5] <- 1
# 
# 
# library(caret)
# confusion_matrix <- confusionMatrix(data=as.factor(y_pred_round), reference = as.factor(y_test))
# confusion_matrix2 <- confusionMatrix(data=as.factor(y_pred_round2), reference = as.factor(y_testb))
# 
# 
# confusion_matrix 
# 
# confusion_matrix2 


searchGridSubCol <- expand.grid(subsample = c(0.5, 0.7, 1), 
                                colsample_bytree = c(0.4,0.7, 1 ),
                                max_depth = c(3, 6, 9),
                                min_child = c(0.3, 1, 3), 
                                eta = c(1,0.1,0.01),
                                nrounds = c(500,1000,6000),
                                stratified = c(T, F)
)


rmseErrorsHyperparameters_b <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentDepth <- parameterList[["max_depth"]]
  currentEta <- parameterList[["eta"]]
  currentMinChild <- parameterList[["min_child"]]
  ntrees <- parameterList[["nrounds"]]
  stratified <- parameterList[["stratified"]]
  xgboostModelCV <- xgb.cv(data =  data.matrix(x_trainb), nrounds = ntrees, nfold = 5, showsd = TRUE, 
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                           "label" = y_trainb,
                           "objective" = "binary:logistic", "max.depth" = currentDepth, "eta" = currentEta,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                           , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                           "stratified" = stratified,
                           early_stopping_rounds = 10)
  
  xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
  rmse <- tail(xvalidationScores$test_rmse_mean, 1)
  trmse <- tail(xvalidationScores$train_rmse_mean,1)
  auc <- tail(xvalidationScores$test_auc_mean, 1)
  tauc <- tail(xvalidationScores$train_auc_mean,1)
  output <- return(c(auc, tauc, rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild,ntrees, stratified))})





rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentDepth <- parameterList[["max_depth"]]
  currentEta <- parameterList[["eta"]]
  currentMinChild <- parameterList[["min_child"]]
  ntrees <- parameterList[["nrounds"]]
  stratified <- parameterList[["stratified"]]
  xgboostModelCV <- xgb.cv(data =  data.matrix(x_train), nrounds = ntrees, nfold = 5, showsd = TRUE, 
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "auc",
                           "label" = y_train,
                           "objective" = "binary:logistic", "max.depth" = currentDepth, "eta" = currentEta,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                           , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                           "stratified" = stratified,
                           early_stopping_rounds = 10)
  
  xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
  rmse <- tail(xvalidationScores$test_rmse_mean, 1)
  trmse <- tail(xvalidationScores$train_rmse_mean,1)
  auc <- tail(xvalidationScores$test_auc_mean, 1)
  tauc <- tail(xvalidationScores$train_auc_mean,1)
  output <- return(c(auc, tauc, rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild,ntrees, stratified))})

write.csv(as.data.frame(rmseErrorsHyperparameters), 'rmseErrorsHyperparameters.csv', row.names = F)

write.csv(as.data.frame(rmseErrorsHyperparameters_b), 'rmseErrorsHyperparameters_b.csv', row.names = F)


rmseErrorsHyperparameters_t <-
  t(as.matrix(rmseErrorsHyperparameters))
rmseErrorsHyperparameters_t <-
  as.data.frame(rmseErrorsHyperparameters_t)
rmseErrorsHyperparameters_b_t <-
  t(as.matrix(rmseErrorsHyperparameters_b))
rmseErrorsHyperparameters_b_t <-
  as.data.frame(rmseErrorsHyperparameters_b_t)

rmseErrorsHyperparameters_t$thresh_phonecall <-'200sec'
rmseErrorsHyperparameters_b_t$thresh_phonecall <-'15sec'
rmseErrorsHyperparameters_cat <- rbind(rmseErrorsHyperparameters_t, rmseErrorsHyperparameters_b_t)

colnames(rmseErrorsHyperparameters_cat) <-
  c('auc', 'tauc', 'rmse', 'trmse', 'subsampleRate', 'colsampleRate', 
    'depth', 'eta', 'minChild', 'nrounds', 'stratified', 'thresh_phonecall')


write.csv(rmseErrorsHyperparameters_cat,
          'model_grid_search_res.csv', row.names = F)