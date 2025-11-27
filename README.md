# ₿ Bitcoin-Conflict-Survival-Analysis
## R 생존 분석을 활용한 비트코인 가격 변동성 연구

본 README는 **생존 분석(Survival Analysis)** 기법을 활용하여 전쟁고 같은 **지정학적 위험(Geopolitical Risk)** 이 
비트코인(Bitcoin) 가격 변동성에 미치는 영향을 분석한 연구 프로젝트를 소개합니다.

## 📌 프로젝트 개요 및 연구 배경
본 연구는 러시아-우크라이나 전쟁과 이란-이스라엘 충돌 기간 동안의 비트코인 일별 종가 데이터를 사용하여, 
비트코인 **가격이 일정 수준 이상으로 급변하는 '이벤트'** 가 발생하기까지 걸리는 시간을 **카플란-마이어(Kaplan-Meier) 추정법**과 
**Cox 비례 위험 모델(Cox Proportional Hazards Model)**을 기반으로 비교 분석했습니다.

## 💡 주요 연구 결과
변동성 심화: 두 전쟁 모두 장기화될수록 비트코인 가격에 더 많은 변동을 주는 것으로 나타났습니다.

## 위험도 비교 (Hazard Ratio): 가격 변동 이벤트 발생 위험도(Hazard Ratio)는
이란-이스라엘 충돌 시 1.52441로, 러시아-우크라이나 전쟁 시의 1.43533보다 높게 나타나, 전자의 충돌이 비트코인 가격 변동에 더 두드러진 영향을 미쳤음을 시사합니다.

## 💻 분석 코드 (analysis.R 주요 로직)
본 분석은 survival 및 survminer 패키지를 사용하여 수행되었으며, 가격 변동(3% 이상 변동)을 **사건(Event)**으로 정의하고,
관측 일수를 **생존 시간(Survival_time)**으로 설정하는 것이 핵심입니다.

### 01. 데이터 전처리및 통합
#### 전쟁기간 설정
  * **이란 이스라엘**
    -*분석 시작 시잠*: 2023년 10월 가자 전쟁 발발 이후, 2024년 초부터 이란의 대리 세력(후티 반군)을 통한 지역적 긴장이 급격히 고조되었으며,
    이는 2024년 4월의 직접적인 공방으로 이어졌습니다. 연구는 이 가장 최근의 고조된 위험 기간을 집중적으로 분석하기 위해 2024년 초를 기점으로 삼았습니다.
    
                     
  * **러시아 우크라이나**
    -*분석 시작 시잠*: 러시아가 우크라이나를 전면적으로 침공하며 전쟁이 대규모로 확전된 날짜로써
     금융 시장과 국제 경제에 미치는 영향이 극대화된 시점을 기준으로 분석하기 위해 해당 날자고 설정하였습니다. 

  * *분석 종료 시점*:분석 시점에 확보할 수 있었던 가장 최근의 데이터 시점까지를 연구의 종료 시점으로 삼았습니다. code:max(데이터$날짜, na.rm = TRUE)
```r
# 열 이름 설정 및 데이터 전처리

colnames(iran_isr_data) <- c("날짜", "종가", "시가", "고가", "저가", "거래량", "변동 %")

iran_isr_data$종가 <- as.numeric(gsub("[^0-9.-]", "", iran_isr_data$종가))

iran_isr_data$날짜 <- as.POSIXct(iran_isr_data$날짜, format = "%Y-%m-%d", tz = "UTC")

iran_isr_data <- iran_isr_data[!is.na(iran_isr_data$종가), ]

# 우크라이나-러시아 전쟁 기간 설정

start_date_ukr_rus <- as.POSIXct("2022-02-24", format = "%Y-%m-%d", tz = "UTC")

end_date_ukr_rus <- max(ukr_rus_data$날짜, na.rm = TRUE)


# 전쟁 기간 동안의 데이터 필터링

ukr_rus_data_filtered <- subset(ukr_rus_data, 날짜 >= start_date_ukr_rus & 날짜 <= end_date_ukr_rus)

ukr_rus_data_filtered$WarEvent <- "Ukraine-Russia War"


# 두 데이터 합치기

combined_data <- rbind(iran_isr_data_filtered, ukr_rus_data_filtered)

```r

### 02. 이벤트 및 생존 시간 정의
-이벤트는 일별 종가의 로그 차분이 3%(log(1 + 0.03)) 이상 변동할 때로 정의됩니다.
```r
# 가격 변동 이벤트 정의 (3% 이상 변동 시 이벤트로 간주)
threshold <- 0.03
price_diff <- c(0, diff(log(combined_data$종가))) # 로그 차분 계산
event_indicator <- abs(price_diff) > log(1 + threshold)

# 이벤트 지표 및 생존 시간 추가
combined_data$Event <- as.integer(event_indicator)
combined_data$Survival_time <- seq_along(combined_data$Event) # 관측치별 고유 시간(일) 생성
```r

### 03. 생존 분석 모델 적합 및 시각화 (Kaplan-Meier)

```r
# 가격 변동 이벤트 정의 (3% 이상 변동 시 이벤트로 간주)
# 생존 분석 모델 적합 (Kaplan-Meier)
fit <- survfit(surv_object ~ WarEvent, data = combined_data)

# 생존 곡선 플롯 (pval: 로그 순위 검정 p-value 표시, conf.int: 신뢰구간 표시)
ggsurvplot(fit, data = combined_data, pval = TRUE, conf.int = TRUE,
           title = "Survival Analysis of Bitcoin Price Changes During Conflicts",
           xlab = "Time (Days)", ylab = "Survival Probability",
           legend.labs = c("Iran-Israel Conflict", "Ukraine-Russia War"))
```r

## 💾 데이터 및 출처
* 데이터 출처: 분석에 사용된 원본 데이터는 **'인베스팅닷컴'**에서 수집한 일별 비트코인 시세 데이터를 사용했습니다.

* 참고: 원본 .csv 파일(iran_isr_data.csv, ukr_rus_data.csv)은 저장소에 포함되어 있지 않습니다. 코드는 분석 방법론과 로직을 참고하는 용도로 활용해 주십시오.

## 📄 연구 논문 (Final Paper)
본 분석의 모든 과정과 상세 결과, 결론은 아래 PDF 논문 파일에서 확인하실 수 있습니다.

## ➡️ [연구 논문 바로가기 (PDF)] (러시아-우크라이나%20전쟁과%20이란-이스라엘%20전쟁에%20따른%20비트코인%20가격%20변동%20및%20투자%20전략.pdf)
