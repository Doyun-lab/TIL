# 비정형 과제
library(stringr)
library(dplyr)
library(tidyverse)
library(pracma)
library(signal)
library(seewave)
library(fBasics)
library(e1071)
library(foreach)
library(xgboost)
library(randomForest)
library(foreach)
library(changepoint)
library(RWeka)

# 데이터 읽어오기
# 폴더 설정
setwd("C://Users//MY//Downloads//A_DeviceMotion_data//A_DeviceMotion_data")

# 디렉토리 확인하기
d <- getwd()
print(d)

# 폴더에 있는 파일 읽어오기
# recusive = 폴더가 나타나면 폴더로 들어가서 안에 있는 파일 가져오기
fls <- dir(d, recursive = T)
for (f in fls){
  a <- file.path(str_c(d, "/", f)) # 폴더 경로명과 파일명 붙이기
  temp <- read.csv(a)
  assign(f, temp) # f 문자열에 객체를 할당해서 객체 만들기
}

# 데이터 살펴보기
summary(`wlk_15/sub_1.csv`)

# 센서값의 크기 구하기
# x, y, z 가속도 센서값을 이용하여 가속도 magnitude 구하기
mag <- function(df, column){
  df[,str_c("mag", column)] <-
    with(df, sqrt(get(str_c(column, ".x"))^2 + get(str_c(column, ".y"))^2 + get(str_c(column, ".z"))^2))
  return(df) 
} # sqrt(.x^2 + .y^2 + .z^2) 를 구하는 것
# column 명에 .x를 붙이면 문자로 되기 때문에 그 문자가 가리키는 데이터를 가져오기 위해 get 사용

# 가속도 센서 x, y, z 값의 크기 구하기
mag(`wlk_15/sub_1.csv`, "userAcceleration")

# 연습문제
# rotation 센서값 구하기
mag(`wlk_15/sub_1.csv`, "rotationRate")

# 핸드폰 센서 데이터로 행동 분류하기
# 현재 시스템에 있는 테이블 다 가져오기
fls

# fls에서 subject #1의 데이터 파일명만 추리기
user1 <- fls[str_detect(fls, "sub_1.csv")]

# subject #1의 walking 데이터 파일명만 추리기
user1_walking <- user1[str_detect(user1, "wlk")]

# 한사람 데이터만 추출하여 시각화
# 한사람의 걷기 데이터만 추출
# user1의 walking 데이터를 넣을 빈 데이터프레임 생성
user1_walking_total <- data.frame()

# user1의 walking 데이터를 하나씩 읽어와서 데이터프레임에 넣기
# user1의 walking 데이터를 차례대로 받고 문자열이 가르키는 데이터를 뽑아옴
user1_walking_total <- data.frame()
for(f in user1_walking){
  temp <- get(f)
  
  user1_walking_total <- rbind(user1_walking_total,
                               temp %>% mutate(exp_no=unlist(regmatches(f, gregexpr("[[:digit:]]+", f)[1]))[1],
                                               id=unlist(regmatches(f, gregexpr("[[:digit:]]+", f)[1]))[2]))
}
# regmatches(f, 패턴의 위치) : 찾는 패턴을 문자열로 반환 (리스트)
# gregexpr(pattern, 문자열) : 문자열에서 패턴이 있는 위치를 반환

# 변수 만들기
# 전체 테이블에 magnitude 변수를 생성
user1_walking_total <- mag(user1_walking_total, "userAcceleration")

# 시간에 따라 가속도 센서값을 시각화
# exp_no 별로 시간값을 생성, 시간값은 데이터 순번으로 새로운 변수 만들기
user1_walking_total <- user1_walking_total %>%
  group_by(exp_no) %>%
  mutate(time=row_number()) %>%
  ungroup()

# 시각화
ggplot(user1_walking_total, aes(x=time, y=maguserAcceleration)) +
  geom_line() + facet_wrap(.~exp_no, nrow=3) # 한 프레임에 여러개 그림, exp_no 별로 시각화

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
head(HAR_total, n=100)

# 가속도, 회전 센서에 대해 magnitude 변수 만들기
HAR_total <- mag(HAR_total, "userAcceleration")
HAR_total <- mag(HAR_total, "rotationRate")

# 비정형 데이터의 정형화
# 전체 데이터에서 통계치 추출
# 가속도 센서, 자이로 센서의 평균, 최대, 최소, skewness, 편차 등을 추출
# skewness 함수 만들기
skewness <- function(x){
  (sum((x - mean(x))^3)/length(x))/((sum((x - mean(x))^2)/length(x)))^(3/2)
}

# userAcceleration, rotationRate 두 변수에 대해서 특질 생성
HAR_summary <- HAR_total %>% group_by(id, exp_no, activity) %>%
  summarize_at(.vars = c("maguserAcceleration", "magrotationRate"), .funs = c(mean, min, max, sd, skewness))
# summarize_at : 선택한 변수에 대해 여러 함수 적용
# .var : 변수선택, .funs : 집계함수

# 분류 모델 구축
# RWeka (알고리즘 이름 class 변수는 factor형이어야 함)
rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
bayes_net <- make_Weka_classifier("wkea/classifiers/bayes/BayesNet")

# 가속도와 회전 센서를 이용해서 행동을 분류할 수 있는지 테스트
# class 변수를 요소형으로 정의
HAR_summary$activity <- as.factor(HAR_summary$activity)

# 변수 일부만 추출 (tibble에서 group 변수를 자동으로 가져와서 ungroup)
# mag 변수와 activity 변수만 가져오기
activity <- HAR_summary %>% ungroup() %>%
  select(c(colnames(HAR_summary)[str_detect(colnames(HAR_summary), "mag")], "activity"))

# mag 포함 여부 체크
str_detect(colnames(HAR_summary), "mag")

# 모델 정의
m <- J48(activity ~ ., data = activity)

# 교차 검증
# numFolds = 10 : 10회 교차검증, complexity : 학습결과가 자세히 나옴, class = TRUE : Confusion matrix 까지
e <- evaluate_Weka_classifier(m, numFolds = 10, complexity = T, class = T)
e

# 알고리즘을 RF로 변경
m_rf <- rf(activity ~ ., data = activity)
e_rf <- evaluate_Weka_classifier(m_rf, numFolds = 10, complexity = T, class = T)
e_rf

# 연습문제
# 성별 정보 불러오기
subject_info <- read.csv("C:/Users/MY/Downloads/data_subjects_info.csv")

colnames(subject_info)[1] <- "id"
subject_info$id <- as.character(subject_info$id)

HAR_summary <- HAR_summary %>%
  inner_join(subject_info, by = "id")

# mag 변수와 activity 변수만 가져오기
activity <- HAR_summary %>% ungroup() %>%
  select(c(colnames(HAR_summary)[str_detect(colnames(HAR_summary), "mag")], "gender"))

activity$gender <- as.factor(activity$gender)

m <- J48(gender ~ ., data = activity)
e <- evaluate_Weka_classifier(m, numFolds = 10, complexity = T, class = T)
e

# 기술통계량
# pracma : matlab의 기능을 R로 옮겨놓음
# fbasics : 수치해석(수학계산)을 위한 패키지

# RMS
# 파형 데이터로부터 도출 가능한 다른 정보
a <- sample(1:10, 10)
a

m <- cummax(a)
m
 
t <- seq(0,1,0.01)
x <- cos(2*pi*t);
plot(t,x,"l")
y <- rms(x)

# 왜도
str(diamonds)
ggplot(diamonds,aes(x=price))+geom_histogram()+ facet_grid(color~.)
skewness(diamonds$price)

# 다이아몬드 색 D는 왼쪽으로 치우쳐져 있으며 볼록한 분포이다
with(diamonds, tapply(price, color, skewness))
with(diamonds, tapply(price, color, kurtosis))

# 나일강 유량 살펴보기
plot(Nile)
hist(Nile)

mean(Nile)
median(Nile)
# 최빈값 (mode)
which.max(table(Nile))

x <- Nile
n = length(x)

# 기하평균
prod(x)^(1/n)
# 조화평균
1/mean(1/x)
# Q-Q plot
# (Quantile Quantile plot): normal dist를 따를 때,
# quantile value와 현재 데이터 분포 상에서의 quantile 값을 scattering하여 나타냄
# x축 : 정규분포를 가정했을 때 사분위수 값, y축 : 실제 사분위수 값
a <- qqnorm(x)
a

# 분포형태와 대칭정도
# 왜도
rowSkewness()
# 첨도
rowKurtosis()

# Seewave의 통계 특징 사용하기
# rms, rss, IQR, kurtosis 추가
rrs <- function(x) rms(x)*(length(x))^0.5

HAR_summary_extend <- HAR_total %>% group_by(id, exp_no, activity) %>%
  summarize_at(.vars = c("maguserAcceleration", "magrotationRate"),
               .funs = c(mean, min, max, sd, skewness, rms, rrs, IQR, e1071::kurtosis))

sapply(HAR_summary_extend, class)

# id, exp_no 지우기 (gouping 해제 후 변수 제거)
HAR_summary_extend2 <- HAR_summary_extend %>% ungroup() %>% select(-c("id", "exp_no"))

m <- J48(as.factor(activity) ~ ., data=HAR_summary_extend2)
e <- evaluate_Weka_classifier(m, numFolds=10, complexity=T, class=T)
e

# 주성분 분석
# PCA
mtcars.pca <- prcomp(HAR_summary_extend2 %>% ungroup() %>% select(-activity),
                     center = T, scale. = T)
mtcars.pca

# 저차원 변수 개수 도출
# 대략 3개 정도로 압축해서 해볼 수 있음
# sdev가 큰 변수 = eigenvalue가 큰 변수 (분산하고 같음)
plot(mtcars.pca$sdev) + title("ScreePlot")

# 주요 차원 3개에 대해 이를 구성할 때 중요하게 쓰인 변수를 2개씩 추출
# 절대값 상위 2개씩 추출하여 학습
m <- J48(as.factor(activity) ~ ., data=HAR_summary_extend2 %>% select(1,2,11,12,15,16))
e <- evaluate_Weka_classifier(m, numFolds=10, complexity=T, class=T)
e

# 피크 분석 (pracma)
x <- seq(0, 1, len = 1024)
pos <- c(0.1,0.13,0.15,0.23,0.25,0.40,0.44,0.65,0.76,0.78,0.81)
hgt <- c(4,5,3,4,5,4.2,2.1,4.3,3.1,5.1,4.2)
wdt <- c(0.005,0.005,0.006,0.01,0.01,0.03,0.01,0.01,0.005,0.008,0.005)

pSignal <- numeric(length(x))
for (i in seq(along=pos)){
  pSignal <- pSignal + hgt[i]/(1 + abs((x - pos[i])/wdt[i]))^4
}
plot(pSignal, type = "l", col = "navy")

# npeaks = 피크 개수, threshold = 피크 정의 기준
# [,1] = 피크크기, [,2] = x축 위치
x <- findpeaks(pSignal, npeaks = 3, threshold = 4, sortstr = T)
points(x[,2], x[,1], pch=20, col="maroon")

# 피크 간격에 대한 통계
mean(diff(sort(x[,2]))) # 평균 (대푯값 하나로 표현)
std(diff(sort(x[,2]))) # 편차 (간격의 산포)

# 센서값의 크기 구하기
mag <- function(df, column){
  df[,str_c("mag", column)] <-
    with(df, sqrt(get(str_c(column, ".x"))^2 + get(str_c(column, ".y"))^2 + get(str_c(column, ".z"))^2))
  return(df)
}

# 원본 데이터에 센서의 크기 변수 추가
for(d in fls){
  f<-get(d)
  f<-mag(f, "rotationRate")
  f<-mag(f, "userAcceleration")
  assign(d,f)
}


# 피크 관련 통계량
Peak_rslt <- data.frame()
for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  p <- findpeaks(f$magrotationRate, threshold=4)
  Peak_rslt <- rbind(Peak_rslt, data.frame(d, # 파일별로 데이터프레임 생성 후 행에 추가, 파일명을 하나의 컬럼으로 하는 데이터프레임 생성
                                           f_n=ifelse(!is.null(p), dim(p)[1], 0), # 피크 개수
                                           p_interval=ifelse(!is.null(p), ifelse(dim(p)[1]>2, mean(diff(p[,2])),0),0), # 피크 간격
                                           p_interval_std=ifelse(!is.null(p), ifelse(dim(p)[1]>2, std(diff(p[,2])),0),0), # 피크 간격 표준편차
                                           p_mean=ifelse(!is.null(p), mean(p[,1]),0),
                                           p_max=ifelse(!is.null(p), max(p[,1]),0),
                                           p_min=ifelse(!is.null(p), min(p[,1]),0),
                                           p_std=ifelse(!is.null(p), std(p[,1]),0)))
}

# 예제 데이터 시각화
temp <- get(fls[1])
plot(mag(temp, "rotationRate")$magrotationRate)
plot(1:length(mag(temp, "rotationRate")$magrotationRate), (mag(temp, "rotationRate")$magrotationRate), "l")
p_temp <- findpeaks((mag(temp, "rotationRate")$magrotationRate), threshold = 5)
points(p_temp[,2], p_temp[,1])    

# 파고율 (Crest Factor) - 피크가 얼마나 극단적인가
temp <- mag(temp, "rotationRate")
crest(temp$magrotationRate, 50, plot=T)
# val : peak 발생 위치의 y값, loc : peak 발생 위치

# 파고율 구하기
temp <- data.frame()
for (d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  
  f <- f %>% select(magrotationRate, maguserAcceleration)
  
  cfR <- crest(f$magrotationRate, 50, plot = T)
  cfA <- crest(f$maguserAcceleration, 50, plot = T)
  
  temp <- rbind(temp, data.frame(d, cfR=cfR$C, cfA=cfA$C))
}

Peak_final <- merge(Peak_rslt, temp, by = "d")

# 추가 변수 생성
# activity, id 정보 추출
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

# activity 구분 (피크 관련 특징만 사용)
activity_Peak <- Peak_final2 %>% ungroup() %>% select(-d, -exp_no, -id)
m <- rf(as.factor(activity) ~ ., data=activity_Peak)
summary(m)

e <- evaluate_Weka_classifier(m, numFolds = 10, complexity = T, class = T)
e

# 변화 분석
library(changepoint)

# 평균의 변화
set.seed(1)
m1 = c(rnorm(100,0,1), rnorm(100,5,1))
time <- 1:length(m1)
plot(time, m1, "l")

m1.amoc <- cpt.mean(m1)
cpts(m1.amoc)

ch_pt <- data.frame()
for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  # 평균의 변화를 감지하도록 두개 변수를 선택한 후 각각에 대해 적용
  rslt <- sapply(f %>% select(magrotationRate, maguserAcceleration), cpt.mean)
  rslt_cpts1 <- cpts(rslt$magrotationRate)
  rslt_cpts2 <- cpts(rslt$maguserAcceleration)
  
  rslt2 <- sapply(f %>% select(magrotationRate, maguserAcceleration), cpt.var)
  rslt2_cpts1 <- cpts(rslt2$magrotationRate)
  rslt2_cpts2 <- cpts(rslt2$maguserAcceleration)
  
  rslt3 <- sapply(f %>% select(magrotationRate, maguserAcceleration), cpt.meanvar)
  rslt3_cpts1 <- cpts(rslt3$magrotationRate)
  rslt3_cpts2 <- cpts(rslt3$maguserAcceleration)
  
  ch_pt <- rbind(ch_pt, data.frame(d, cp1=length(rslt_cpts1), cp2=length(rslt_cpts2), cp3=length(rslt2_cpts1),
                                   cp4=length(rslt2_cpts2), cp5=length(rslt3_cpts1), cp6=length(rslt3_cpts2)))
}
head(ch_pt)

for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  assign(d, f)
}

# 파일명에서 activity, id 정보 추출
id_f <- function(x){
  exp_no=unlist(regmatches(x, gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x, gregexpr("[[:digit:]]+", x)[1]))[2]
  activity=unlist(str_split(x, "\\_"))[1]
  
  return(cbind(exp_no, id, activity))
}

temp <- data.frame()
for (i in 1:nrow(ch_pt)){
  temp <- rbind(temp, id_f(ch_pt$d[i]))
}

ch_pt <- cbind(ch_pt, temp)
ch_pt2 <- ch_pt %>% ungroup() %>% select(-d, -exp_no, -id)

m <- rf(as.factor(activity) ~ ., data=ch_pt2)
table(activity_Peak$activity)
e <- evaluate_Weka_classifier(m, numFolds = 10, complexity = T, class = T)
e

# peak + change pt 관련 특징만 사용
peak_final3 <- merge(Peak_final2, ch_pt, by = c("d", "exp_no", "id", "activity"))
combined <- peak_final3 %>% select(-d, -exp_no, -id, -id_exp_no)
colnames(combined)

m <- rf(as.factor(activity) ~ ., data=combined)
e <- evaluate_Weka_classifier(m, numFolds = 10, complexity = T, class = T)
e

# 모든 특징 사용
peak_final4 <- merge(peak_final3, HAR_summary_extend, by = c("exp_no", "id", "activity"))
combined2 <- peak_final4 %>% select(-d, -exp_no, -id)

m <- rf(as.factor(activity) ~ ., data=combined2)
e <- evaluate_Weka_classifier(m, numFolds = 10, complexity = T, class = T)
e

# 주파수 특징 분석
# 각 주파수의 분산으로 측정
# 정현파 생성
t <- seq(0,200,by=0.1)  # 0.1초 단위로 신호 생성
x <- cos(2*pi*t/16) + 0.75*sin(2*pi*t/5)
plot(t, x, "l")

# 스펙트럼 계산
# 신호를 구성하는 frequency를 구한다음 각 frequency의 power를 확률밀도 함수로 나타냄
x.spec <- spectrum(x)

# 0.1초 단위로 데이터를 생성했으므로 frequency도 0.1 단위로 추출
spx <- x.spec$freq * 1/0.1  

# spectrum*2를 곱해서 크기를 구하고 계산과정에서 주파수 +,-가 존재하도록 유도되는데 
# spectrum함수는 + 주파수의 전력만 보여주기 때문에 
# *2로 해야 해당 주파수의 전력이 절대값이 산출됨
spy <- 2*x.spec$spec

plot(spy~spx, xlab="frequency", ylab="spectral density", type="l")

# spectrum 분석을 통한 변수화
# 상위 두개를 추출하여 변수화하기로 함
spx[which(spy %in% sort(spy,decreasing=TRUE)[1:2])]

# 계단 내려가기
r.spec<-spectrum(`dws_1/sub_1.csv`$magrotationRate,plot=TRUE)

# 앉아있기
r.spec<-spectrum(`sit_13/sub_1.csv`$magrotationRate,plot=TRUE)

# 조깅
r.spec<-spectrum(`jog_16/sub_1.csv`$magrotationRate,plot=TRUE)

# 주파수 파워 계산
# log="no": spectrum이 log scale로 계산되어서 나옴
# 로그 스케일은 작은 값에서의 차이를 증폭하는 효과가 있음
# span=10 : SPIKE가 너무 많이 생성됨, 
# 정해놓은 구간에 10개 데이터를 smoothin한후에 spectrum계산
r.spec <- spectrum(`dws_1/sub_1.csv`$magrotationRate, log="no", span=10, plot=TRUE)

# spectrum 함수 결과 살펴보기
# 주요 주파수 산출, freq가 작은 순서대로 나옴
r.spec$freq

# 주파수별 강도
r.spec$spec

# spectrum 그려보기
fr <- r.spec$freq*50
sp <- r.spec$spec*2
plot(sp~fr, xlab="frequency", ylab="spectrum", type="l")

# 가속도 센서 값 시각화
# 각 그래프에서 행동 label을 붙여보자
# 가속도 센서에 지속적인 중력가속도가 작용
# frequency 0 (계속 발생) 에서 sepctrum이 높게 나옴
# frequency가 0 근처인 것들 제외
# frequency가 높은 데이터만 살리기 -> high pass filter
# high pass filter 적용
bf <- butter(1,0.05, type="high") #주파수가 0.05이상인 값만 통과시키기
spectrum(`jog_16/sub_10.csv`$maguserAcceleration,log="no",span=10,plot=TRUE)
spectrum(signal::filter(bf,`jog_16/sub_10.csv`$maguserAcceleration),log="no",span=10,plot=TRUE)

# freq & spectrum 5개씩 변수화
freq_rslt<-data.frame()
for(d in fls){
  f<-get(d)
  r.spec <-spectrum(f$magrotationRate) 
  
  fr<-r.spec$freq*50
  sp<-r.spec$spec*2
  freq_rslt<-rbind(freq_rslt,as.data.frame(t(c(d, fr[1:5],sp[1:5]))))
} # frequency가 작은 순서대로 나옴 (작은 순서로 5개 뽑은 것)

# id, activity, exp_no추출 함수 만들기
id_f <-function(x){ 
  exp_no=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[2]
  activity=unlist(str_split(x,"\\_"))[1]
  return(cbind(exp_no, id, activity))
}

# d(테이블명)로부터 id, activity, exp_no추출 
temp <-data.frame()
for(i in 1:nrow(freq_rslt)){
  temp <- rbind(temp,id_f(freq_rslt$V1[i]))
}
 
freq_rslt <- cbind(freq_rslt,temp)

# 스무딩 (pracma)
# SG filter : 윈도우 구간에서 다항 함수로 fitting해서 원본 값을 smoothing
# 정현파 생성
ts <- sin(2*pi*(1:1000)/200)
# noise 추가
t1 <- ts + rnorm(1000)/10

# SG filter로 스무딩
t2 <- savgol(t1, 51)

# 원본과 비교
plot(1:1000, t1, col = "grey")
lines(1:1000, ts, col = " blue")
lines(1:1000, t2, col = "red")

# SG filter에서 fitting window 구간 변경
t3 <- savgol(t1, 5)
plot(t1, col="grey", type="l")
lines(t3, col="red")
# window 구간을 짧게 했기 때문에 잔피크 여전히 많음

# median smoothing
# 조금 더 변화가 심한 시그널 생성
t <- seq(0, 1, len=100)
x <- sin(2*pi*t*2.3) + 0.25*rlnorm(length(t), 0.5) # 2.3 Hz sinusoid(정현파) + noise
plot(t, x, type = "l")

# 3-point filter
lines(t, medfilt1(x), col="red", lwd=2)
# 7-point filter
lines(t, signal::filter(MedianFilter(7), x), col="blue", lwd=2)

# 이동평균 smoothing
filt <- Arma(b = c(1, 2, 1)/3, a = c(1, 1))
lines(t, signal::filter(filt, x), col="purple")

# 샘플링 간격을 달리해서 스무딩
t <- seq(0, 1, len=100) # 샘플링 간격격
x <- sin(2*pi*t*2.3) + 0.25*rnorm(length(t)) # 2.3 Hz sinusoid + noise

# 2pi*1 이면 한바퀴 도는데 1초 : 주기 1
# 2pi*t*2.3 이면 1/2.3초 걸림 : 주기 2.3
t <- seq(0, 1, len = 100)
x <- sin(2*pi*t*2.3)
plot(t, x, type = "l")

# 샘플링 간격을 10배 늘림
t1 <- seq(0, 1, len=10)
x1 <- sin(2*pi*t1*2.3) + 0.25*rnorm(length(t1))
lines(t1, x1, col="blue")

# resampling 사용
z <- resample(x, 4, 10)
lines(seq(0,1,len=40), z, col="red")

# 이상치 제거
# hampel filter
# hampel(x, k, t0=3)
# x : time series / k : window size 결정 (window length 2*k+1 in indices)
# t0=3 : 3 sigma를 벗어나는 값을 이상치 (이상치는 중앙값으로 대치)
# 보통 좌우 3개, 총 6개의 값의 중앙갑 수한 후 중앙값에 대한 절대 편차 구하고 벗어나면 이상치
# 신호 생성
x <- sin(2*pi*(0:99)/100)

# 이상치 만들기
x[6] <- 2
x[20] <- 2

# 시각화
t <- 1:length(x)
plot(t, x, "l")

# 이상치 값 제거 필터 적용
filtered <- hampel(x, k=6, t=2)
plot(filtered$y)

# 추세 제거하기 (detrend)
t <- 0:20
x <- 3*sin(t)+t
y <- pracma::detrend(x, tt="linear", bp=c())
# tt : 추세를 어떤 함수로 추정할 것인지
# bp : breaks points
plot(t, x, "l", ylim=c(-5, 25))
lines(t, y, col="red")

# step function으로 추세 제거
y <- pracma::detrend(x, tt="constant", bp=c())
plot(t, x, "l", ylim=c(-5, 25))
lines(t, y, col="red")
