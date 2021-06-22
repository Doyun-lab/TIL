# 비정형 과제
library(stringr)
library(dplyr)
library(tidyverse)
library(pracma)
library(signal)
library(seewave)
library(e1071)
library(doParallel)
library(foreach)
library(xgboost)
library(randomForest)
library(doParallel)
library(foreach)

# 폴더 설정
setwd("/Users/kwondoyun/Downloads/A_DeviceMotion_data")
d <- getwd()

# 폴더에 있는 파일 읽어오기 (폴더로 들어가서 안에 있는 파일 가져오기)
fls <- dir(d, recursive = T)
for (f in fls){
  a <- file.path(str_c(d, "/", f))
  temp <- read.csv(a)
  assign(f, temp)
}

# x, y, z 가속도 센서값을 이용하여 가속도 magnitude 구하기
# sqrt(x^2 + y^2 + z^2)
mag <- function(df, column){
  df[,str_c("mag", column)] <- with(df, sqrt(get(str_c(column, ".x"))^2 +
                                               get(str_c(column, ".y"))^2 +
                                               get(str_c(column, ".z"))^2))
  return(df)
}

# 전체 데이터 만들기
# 사람별, 실험번호별, 활동별 특징 추출
HAR_total <- data.frame()
for(f in fls){
  temp <- get(f)
  HAR_total <- rbind(HAR_total, temp %>%
                       mutate(exp_no=unlist(regmatches(f, gregexpr("[[:digit:]]+", f)[1]))[1],
                              id=unlist(regmatches(f, gregexpr("[[:digit:]]+", f)[1]))[2],
                              activity=unlist(str_split(f, "\\_"))[1]))
}

# gravity 추가
HAR_total <- mag(HAR_total, "userAcceleration")
HAR_total <- mag(HAR_total, "rotationRate")
HAR_total <- mag(HAR_total, "gravity")

# 가속도 센서, 자이로센서의 평균 / 최대 / 최소 / skewness / 편차 추출
# skewness (왜도) 함수 만들기
skewness <- function(x){
  (sum((x - mean(x))^3)/length(x))/((sum((x - mean(x))^2)/length(x)))^(3/2)
}

# 조화평균 함수 만들기
harmonic_mean <- function(x){
  1/mean(1/x)
}

# 변이계수 함수 만들기
coefficient_var <- function(x){
  100*sd(x)/mean(x)
}

# rrs 함수 만들기
rrs <- function(x) rms(x)*(length(x))^0.5

HAR_summary_extend <- HAR_total %>% group_by(id, exp_no, activity) %>%
  summarize_at(.vars = c("maguserAcceleration", "magrotationRate", "maggravity"),
               .funs = c(mean, min, max, sd, skewness, rms, rrs, IQR, e1071::kurtosis,
                         harmonic_mean))

# 기존 변수에 rotationRate, userAcceleration, gravity 추가
for (d in fls) {
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  f<-mag(f, "gravity")
  assign(d,f)
}

# 피크 관련 통계량
# rotationRate
Peak_rslt <- data.frame()
for(d in fls){
  f <- get(d)
  
  p <- findpeaks(f$magrotationRate, threshold=4)
  Peak_rslt <- rbind(Peak_rslt, data.frame(d,
                                           f_n=ifelse(!is.null(p), dim(p)[1], 0),
                                           p_interval=ifelse(!is.null(p), ifelse(dim(p)[1]>2,
                                                                                 mean(diff(p[,2])),0),0),
                                           p_interval_std=ifelse(!is.null(p), ifelse(dim(p)[1]>2,
                                                                                     std(diff(p[,2])),0),0),
                                           p_mean=ifelse(!is.null(p), mean(p[,1]),0),
                                           p_max=ifelse(!is.null(p), max(p[,1]),0),
                                           p_min=ifelse(!is.null(p), min(p[,1]),0),
                                           p_std=ifelse(!is.null(p), std(p[,1]),0)))
}

# Acceleration도 추가
Peak_rslt_accel <- data.frame()
for(d in fls){
  f <- get(d)
  
  p <- findpeaks(f$maguserAcceleration, threshold=4)
  Peak_rslt_accel <- rbind(Peak_rslt_accel, data.frame(d,
                                                       f_n_a=ifelse(!is.null(p), dim(p)[1], 0),
                                                       p_interval_a=ifelse(!is.null(p), ifelse(dim(p)[1]>2,
                                                                                               mean(diff(p[,2])),0),0),
                                                       p_interval_std_a=ifelse(!is.null(p),
                                                                               ifelse(dim(p)[1]>2, std(diff(p[,2])),0),0),
                                                       p_mean_a=ifelse(!is.null(p), mean(p[,1]),0),
                                                       p_max_a=ifelse(!is.null(p), max(p[,1]),0),
                                                       p_min_a=ifelse(!is.null(p), min(p[,1]),0),
                                                       p_std_a=ifelse(!is.null(p), std(p[,1]),0)))
}

# 파고율 (Crest Factor) - 피크가 얼마나 극단적인지를 나타내는 척도
temp <- data.frame()
for (d in fls){
  f <- get(d)
  f <- f %>% select(magrotationRate, maguserAcceleration, maggravity)
  
  cfR <- crest(f$magrotationRate, 50, plot = F)
  cfA <- crest(f$maguserAcceleration, 50, plot = F)
  cfG <- crest(f$maggravity, 50, plot = F)
  
  temp <- rbind(temp, data.frame(d, cfR=cfR$C, cfA=cfA$C, cfG=cfG$C))
}

# 피크 특징과 파고율 합치기
Peak_final <- merge(Peak_rslt, temp, by = "d")
Peak_final <- merge(Peak_final, Peak_rslt_accel, by = "d")

# 파일명에서 activity, id 등의 정보 추출
id_f <- function(x){
  exp_no=unlist(regmatches(x, gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x, gregexpr("[[:digit:]]+", x)[1]))[2]
  activity=unlist(str_split(x, "\\_"))[1]
  return(cbind(exp_no, id, activity))
}

temp <- data.frame()
for (i in 1:nrow(Peak_final)){
  temp <- rbind(temp, id_f(Peak_final$d[i]))
}

Peak_final2 <- cbind(Peak_final, temp)

# 변화 분석
library(changepoint)

# 변화 횟수 변수화
ch_pt <- data.frame()
for(d in fls){
  f <- get(d)
  
  rslt <- sapply(f %>% select(magrotationRate, maguserAcceleration, maggravity), cpt.mean)
  rslt_cpts1 <- cpts(rslt$magrotationRate)
  rslt_cpts2 <- cpts(rslt$maguserAcceleration)
  rslt_cpts3 <- cpts(rslt$maggravity)
  
  rslt2 <- sapply(f %>% select(magrotationRate, maguserAcceleration, maggravity), cpt.var)
  rslt2_cpts1 <- cpts(rslt2$magrotationRate)
  rslt2_cpts2 <- cpts(rslt2$maguserAcceleration)
  rslt2_cpts3 <- cpts(rslt2$maggravity)
  
  rslt3 <- sapply(f %>% select(magrotationRate, maguserAcceleration, maggravity), cpt.meanvar)
  rslt3_cpts1 <- cpts(rslt3$magrotationRate)
  rslt3_cpts2 <- cpts(rslt3$maguserAcceleration)
  rslt3_cpts3 <- cpts(rslt3$maggravity)
  
  ch_pt <- rbind(ch_pt, data.frame(d, cp1=length(rslt_cpts1), cp2=length(rslt_cpts2),
                                   cp3=length(rslt2_cpts1),
                                   cp4=length(rslt2_cpts2), cp5=length(rslt3_cpts1),
                                   cp6=length(rslt3_cpts2),
                                   cp7=length(rslt_cpts3), cp8=length(rslt2_cpts3),
                                   cp9=length(rslt3_cpts3)))
}

temp <- data.frame()
for (i in 1:nrow(ch_pt)){
  temp <- rbind(temp, id_f(ch_pt$d[i]))
}
ch_pt <- cbind(ch_pt, temp)

# 퓨리에 변환
HAR_test <- HAR_total %>% select(-X)
HAR_test <- HAR_test[,1:12]
x <- 2
z <- 1
name_col <- colnames(HAR_test)
fft_frame <- data.frame(1:1412865)
for (i in HAR_test){
  fft_temp <- fft(i)
  revalue <- Re(fft_temp)
  imvalue <- Im(fft_temp)
  
  temp <- data.frame(revalue, imvalue)
  
  fft_frame <- cbind(fft_frame, temp)
  colnames(fft_frame)[x] <- paste0(name_col[z], "Re")
  colnames(fft_frame)[x+1] <- paste0(name_col[z], "Im")
  x <- x + 2
  z <- z + 1
  
  print(z)
}

fourier_trans <- cbind(fft_frame, HAR_total$exp_no, HAR_total$activity, HAR_total$id)
fourier_trans <- fourier_trans %>% select(-X1.1412865)
var_name <- colnames(fourier_trans)[1:24]
fourier_summary <- fourier_trans %>% group_by(HAR_total$id, HAR_total$exp_no,
                                              HAR_total$activity) %>%
  summarize_at(.vars = var_name,
               .funs = c(mean, min, max, sd, skewness, rms, rrs, IQR, e1071::kurtosis,
                         harmonic_mean, coefficient_var, weighted.mean))

fourier_summary <- as.data.frame(fourier_summary)
colnames(fourier_summary)[1:3] <- c("id", "exp_no", "activity")

# 피크 특징(p_), 파고율 특징(cf) + 변화 분석(cp)
Peak_final3 <- merge(Peak_final2, ch_pt, by = c("d", "exp_no", "id", "activity"))

# 피크 특징, 파고율 특징, 변화 분석 + 통계 특징
combined_all <- merge(Peak_final3, HAR_summary_extend, by = c("id", "exp_no", "activity"))

# 성별 정보 추가
subject_info <- read.csv("/Users/kwondoyun/Downloads/data_subjects_info.csv")
colnames(subject_info)[1] <- "id"
combined_total <- merge(combined_all, subject_info, by = "id")

# model data 생성
model.data_combine <- combined_total %>% select(-id, -exp_no, -d)
model.data_combine <- na.omit(model.data_combine)
# -----------------------------------------------------------------------------
# model 1
model_data_fft <- data.frame(fourier_summary) %>% select(-id, -exp_no)
nrow(model_data_fft)
rf_m <- randomForest::randomForest(as.factor(activity) ~ ., data = model_data_fft)
length(colnames(model_data_fft))

# 중요도 파악
importance_value <- data.frame(randomForest::importance(rf_m)) %>%
  arrange(desc(MeanDecreaseGini))
importance_value

# 중요도 낮은 변수 제거 (MeanDecreaseGini < 1 제거)
unimp <- subset(importance_value, importance_value$MeanDecreaseGini < 1)
nrow(unimp) # 201개
except_name <- rownames(unimp)
model_data_importance <- model_data_fft %>% select(-except_name)

library(caret)
set.seed(23)
cv_list <- createFolds(model_data_importance$activity, k = 10)

# 10-fold cross validation
for(i in 1:length(cv_list)) {
  valid_index <- cv_list[[i]]
  
  # test 데이터
  cv_valid_set <- model_data_importance[valid_index,]
  
  #train 데이터
  cv_train_set <- model_data_importance[-valid_index,]
  
  # 모델 생성
  rf_m <- randomForest::randomForest(as.factor(activity) ~ ., data = cv_train_set)
  
  # predict
  rf_p <- predict(rf_m, newdata = cv_valid_set, type = "class")
  
  # model acurracy 생성
  assign(paste0("cv_accuracy_",i),sum(cv_valid_set$activity == rf_p, na.rm = T)/nrow(cv_valid_set))
  
}

result_cv <- mget(ls(pattern="cv_accuracy_"))
accuracy_vector <- c()
for (i in result_cv){
  accuracy_vector <- c(accuracy_vector, i)
}
mean(accuracy_vector)

# -----------------------------------------------------------------------------
# model 2
# Data set 나누기
x = model_data_fft %>% select(-activity) %>% data.matrix
y = as.factor(model_data_fft$activity)

# 10-fold
cv_model1 = xgb.cv(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length,
                   nfold = 10, nrounds = 500, early_stopping_rounds = 200,
                   objective = "multi:softprob", prediction = T, eta = 0.2, gamma = 0)
cv_model1

pred_df = cv_model1$pred %>% as.data.frame %>%
  mutate(pred = levels(y)[max.col(.)] %>% as.factor, actual = y)
pred_df

pred_df %>% select(pred, actual) %>% table
result <- caret::confusionMatrix(pred_df$pred, pred_df$actual)
result

# 하이퍼 파라미터 튜닝
grid = expand.grid(eta = seq(0.1,0.4,0.05),gamma = seq(0,5,1))
result_para <- c(1,1,1)
for (i in 1:nrow(grid)){
  cv_model1 = xgb.cv(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length,
                     nfold = 10, nrounds = 500, early_stopping_rounds = 200,
                     objective = "multi:softprob", prediction = T, eta = grid[i,1], gamma = grid[i,2])
  
  pred_df = cv_model1$pred %>% as.data.frame %>% 
    mutate(pred = levels(y)[max.col(.)] %>% as.factor, actual = y)
  
  result <- caret::confusionMatrix(pred_df$pred, pred_df$actual)
  
  result2 <- data.frame(result$overall[1], grid[i,1], grid[i,2])
  result_para <- rbind(result_para, result2)
}
result_para %>% arrange(desc(result.overall.1.))

# -----------------------------------------------------------------------------
# model 3
# Data set 나누기
x = model_data_importance %>% select(-activity) %>% data.matrix
y = as.factor(model_data_importance$activity)

# 10-fold
cv_model2 = xgb.cv(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length,
                   nfold = 10, nrounds = 500, early_stopping_rounds = 200,
                   objective = "multi:softprob", prediction = T, eta = 0.2, gamma = 0)
cv_model2

pred_df = cv_model2$pred %>% as.data.frame %>%
  mutate(pred = levels(y)[max.col(.)] %>% as.factor, actual = y)
pred_df

pred_df %>% select(pred, actual) %>% table
result <- caret::confusionMatrix(pred_df$pred, pred_df$actual)
result
