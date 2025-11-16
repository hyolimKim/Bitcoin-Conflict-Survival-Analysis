if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
if (!requireNamespace("survminer", quietly = TRUE)) install.packages("survminer")
library(survival)
library(survminer)

# 데이터 파일 경로 설정
file_path_iran_isr <- "path/to/iran_isr_data.csv"  # 이란-이스라엘 충돌 데이터 파일 경로
file_path_ukr_rus <- "path/to/ukr_rus_data.csv"  # 우크라이나-러시아 전쟁 데이터 파일 경로

# 데이터 로드 - 이란-이스라엘 충돌 데이터
iran_isr_data <- read.csv(file_path_iran_isr, fileEncoding = "UTF-8", header = FALSE, sep = ",")

# 열 이름 설정 및 데이터 전처리
colnames(iran_isr_data) <- c("날짜", "종가", "시가", "고가", "저가", "거래량", "변동 %")
iran_isr_data$종가 <- as.numeric(gsub("[^0-9.-]", "", iran_isr_data$종가))
iran_isr_data$날짜 <- as.POSIXct(iran_isr_data$날짜, format = "%Y-%m-%d", tz = "UTC")
iran_isr_data <- iran_isr_data[!is.na(iran_isr_data$종가), ]

# 이란-이스라엘 충돌 기간 설정
start_date_iran_isr <- as.POSIXct("2024-01-01", format = "%Y-%m-%d", tz = "UTC")
end_date_iran_isr <- max(iran_isr_data$날짜, na.rm = TRUE)

# 충돌 기간 동안의 데이터 필터링
iran_isr_data_filtered <- subset(iran_isr_data, 날짜 >= start_date_iran_isr & 날짜 <= end_date_iran_isr)
iran_isr_data_filtered$WarEvent <- "Iran-Israel Conflict"

# 데이터 로드 - 우크라이나-러시아 전쟁 데이터
ukr_rus_data <- read.csv(file_path_ukr_rus, fileEncoding = "UTF-8", header = FALSE, sep = ",")

# 열 이름 설정 및 데이터 전처리
colnames(ukr_rus_data) <- c("날짜", "종가", "시가", "고가", "저가", "거래량", "변동 %")
ukr_rus_data$종가 <- as.numeric(gsub("[^0-9.-]", "", ukr_rus_data$종가))
ukr_rus_data$날짜 <- as.POSIXct(ukr_rus_data$날짜, format = "%Y-%m-%d", tz = "UTC")
ukr_rus_data <- ukr_rus_data[!is.na(ukr_rus_data$종가), ]

# 우크라이나-러시아 전쟁 기간 설정
start_date_ukr_rus <- as.POSIXct("2022-02-24", format = "%Y-%m-%d", tz = "UTC")
end_date_ukr_rus <- max(ukr_rus_data$날짜, na.rm = TRUE)

# 전쟁 기간 동안의 데이터 필터링
ukr_rus_data_filtered <- subset(ukr_rus_data, 날짜 >= start_date_ukr_rus & 날짜 <= end_date_ukr_rus)
ukr_rus_data_filtered$WarEvent <- "Ukraine-Russia War"

# 두 데이터 합치기
combined_data <- rbind(iran_isr_data_filtered, ukr_rus_data_filtered)

# WarEvent 변수를 factor로 변환
combined_data$WarEvent <- factor(combined_data$WarEvent)

# 가격 변동 이벤트 정의 (3% 이상 변동 시 이벤트로 간주)
threshold <- 0.03
price_diff <- c(0, diff(log(combined_data$종가)))  # 로그 차분 계산, 첫 번째 값은 0으로 설정
event_indicator <- abs(price_diff) > log(1 + threshold)

# 이벤트 지표 추가
combined_data$Event <- as.integer(event_indicator)

# 생존 시간 계산 (초기값을 0으로 설정하고 누적합 계산)
combined_data$Survival_time <- seq_along(combined_data$Event)  # 각 관측치에 대해 고유한 시간 생성

# Surv 객체 생성 (종속변수 정의)
surv_object <- Surv(time = combined_data$Survival_time, event = combined_data$Event)

# 생존 분석 모델 (Kaplan-Meier)
fit <- survfit(surv_object ~ WarEvent, data = combined_data)

# 생존 곡선 플롯
ggsurvplot(fit, data = combined_data, pval = TRUE, conf.int = TRUE,
           title = "Survival Analysis of Bitcoin Price Changes During Conflicts",
           xlab = "Time", ylab = "Survival Probability",
           