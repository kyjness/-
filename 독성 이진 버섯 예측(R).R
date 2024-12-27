rm(list=ls())

#탐색적 데이터 분석
##데이터 둘러보기
raw_df = read.csv('C:/Users/yujin/Desktop/4학년 1학기/전산실습/1차프로젝트 데이터/train.csv')
raw_test = read.csv('C:/Users/yujin/Desktop/4학년 1학기/전산실습/1차프로젝트 데이터/test.csv')
str(raw_df)

##데이터 정제

#id제거
df = raw_df[, setdiff(names(raw_df), "id")]
test = raw_test[, setdiff(names(raw_test), "id")]

#class 컬럼 binary encoding, factor로 변환
df$class = as.factor(ifelse(df$class == "e", 0, ifelse(df$class == "p", 1, NA)))

###결측치 처리
#전부 na로 대체
df = as.data.frame(lapply(df, function(x) {
  x[is.na(x) | x == "" | x == " " | x == "?" | x == "NA"] <- NA
  return(x) }))
test = as.data.frame(lapply(test, function(x) {
  x[is.na(x) | x == "" | x == " " | x == "?" | x == "NA"] <- NA
  return(x)
}))

#피처요약표 생성 및 출력
resumetable = function(df) {
 summary_table = data.frame(
      데이터_타입 = sapply(df, class),
      결측값_개수 = sapply(df, function(x) sum(is.na(x))),
      결측치_비율 = sapply(df, function(x) round((sum(is.na(x)) / nrow(df)) * 100, 2)),
      고유값_개수 = sapply(df, function(x) length(unique(x[!is.na(x)]))),
      stringsAsFactors = FALSE
    )
  print(paste("데이터셋 형상:", paste(dim(df), collapse = " x ")))
  return(summary_table)}
resumetable(df)

# 결측치 비율이 50% 이상인 열 확인
columns_to_drop = row.names(resumetable(df)[resumetable(df)$'결측치_비율' >= 50, ])
# 결과 출력
print(columns_to_drop)

#결측치 비율이 50%이상인 열 제거(포자색상 제외)
columns_to_drop = columns_to_drop[!columns_to_drop %in% ('spore.print.color')]
df_cleaned = df[, !(names(df) %in% columns_to_drop)]
test_cleaned = test[, !(names(test) %in% columns_to_drop)]
colnames(df_cleaned)


#결측치 채우기
resumetable(df_cleaned)

#결측치 비율이 높은 컬럼 missing으로 대체
df_cleaned$gill.spacing[is.na(df_cleaned$gill.spacing)] = "Missing"
df_cleaned$spore.print.color[is.na(df_cleaned$spore.print.color)] = "Missing"

# 데이터프레임에서 범주형 및 연속형 열 분리
categorical_columns = names(df_cleaned)[sapply(df_cleaned, is.character)]
numerical_columns = names(df_cleaned)[sapply(df_cleaned, is.numeric)]

test_categorical_columns = names(test_cleaned)[sapply(test_cleaned, is.character)]
test_numerical_columns = names(test_cleaned)[sapply(test_cleaned, is.numeric)]

# 범주형 변수: 최빈값으로 결측값 채우기
for (col in categorical_columns) {
  mode_value = names(sort(table(df_cleaned[[col]]), decreasing = TRUE))[1]
  df_cleaned[[col]][is.na(df_cleaned[[col]])] = mode_value}
for (col in test_categorical_columns) {
  mode_value = names(sort(table(test_cleaned[[col]]), decreasing = TRUE))[1]
  test_cleaned[[col]][is.na(test_cleaned[[col]])] = mode_value}

# 연속형 변수: 중앙값으로 결측값 채우기
for (col in numerical_columns) {
  median_value = median(df_cleaned[[col]], na.rm = TRUE)
  df_cleaned[[col]][is.na(df_cleaned[[col]])] = median_value}
for (col in test_numerical_columns) {
  median_value = median(test_cleaned[[col]], na.rm = TRUE)
  test_cleaned[[col]][is.na(test_cleaned[[col]])] = median_value}

resumetable(df_cleaned)


###고유값 처리
# 빈도 낮은 범주를 "Unknown"으로 대체하는 함수
replace_infrequent_categories = function(df, column, threshold = 70) {
  value_counts = table(df[[column]])
  infrequent = names(value_counts[value_counts <= threshold])
  df[[column]] = sapply(df[[column]], function(x) { if (x %in% infrequent) {return("Unknown")} else {return(x)} })
  return(df)}
# 범주형 열 처리
for (col in categorical_columns) {
  df_cleaned = replace_infrequent_categories(df_cleaned, col)}
for (col in categorical_columns){
  test_cleaned = replace_infrequent_categories(test_cleaned, col)}

resumetable(df_cleaned)


###정제된 데이터 시각화

#시각화 라이브러리 불러오기
library(ggplot2)
#정제된 데이터 이름 재지정
df1 = df_cleaned
test1 = test_cleaned


# 범주형 데이터 시각화
plot_categorical <- function(data, categorical_columns) {
  # 한 화면에 여러 그래프 배치
  par(mfrow = c(ceiling(length(categorical_columns) / 5), 5)) # 3열씩 배치
  for (col in categorical_columns) {
    # 막대 그래프 생성
    barplot(
      table(data[[col]]), 
      col = "skyblue", 
      xlab = col, 
      las = 1 # x축 텍스트 가로로 설정
    )
  }
  # 그래프 레이아웃 초기화
  par(mfrow = c(1, 1))
}

# 실행
plot_categorical(df1, categorical_columns)

# 연속형 데이터 시각화
plot_numerical <- function(data, numerical_columns) {
  # 한 화면에 여러 그래프 배치
  par(mfrow = c(ceiling(length(numerical_columns) / 3), 3)) # 3열씩 배치
  for (col in numerical_columns) {
    # 히스토그램 생성
    hist(
      data[[col]], 
      col = "lightblue", 
      xlab = col, 
      ylab = "Frequency", 
      breaks = 30 # 구간 수 설정
    )
  }
}

# 실행
plot_numerical(df1, numerical_columns)


