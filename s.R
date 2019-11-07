sentimental <- function(keyword,path){
  print('bb')
    print(keyword)
    print(path)
    # ************************************************
    # -- 감성 분석(단어의 긍정/부정 분석) 
    #    시각화 : 파랑/빨강/녹색 
    #    input: 저장 파일 경로
    #           키워드       
    # ************************************************
    # ------------------------------
    # -- 1) 데이터 가져오기("C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R")
    # ------------------------------
    setwd(path)
    # setwd("C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R")
    # ------------------------------
    # -- 2) 단어 사전에 단어추가S
    # ------------------------------
    # data = read.csv("Input/조국.csv", encoding="UTF-8", header=F, sep="\n", quote="\n")
    data = read.csv("Input/조국.csv", fileEncoding = "CP949", encoding="UTF-8", header=F, sep="\n", quote="\n")

    data[,1] <- gsub('[^[:alnum:][:blank:]+?&/\\-]', '', data[,1]) # 문장부호 제거
    data[,2] <- gsub('[^[:alnum:][:blank:]+?&/\\-]', '', data[,1]) # 문장부호 제거

    write.csv(
      data,           # 파일에 저장할 데이터 프레임 또는 행렬
      file="Output/aa.csv"    # 데이터를 저장할 파일명
      ,row.names=F # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
    )

    # -- 긍정단어와 부정단어를 카운터하여 긍정/부정 형태로 빈도 분석
    #    negative.txt : 부정어 사전
    #    positive.txt : 긍정어 사전

    # -- (1) 긍정어/부정어 영어 사전 가져오기
    positive <- readLines("감성사전/positive.txt")
    negative <- readLines("감성사전/negative.txt")
    length(positive)                                                
    length(negative)
    length(data)
    typeof(data)

    names(data)
    # ------------------------------
    # -- 2) 감성 분석 함수 정의-sentimental
    # ------------------------------

    # -- (1) 문자열 처리를 위한 패키지 로딩 
    library(plyr)                                                        # laply()함수 제공
    library(stringr)                                         # str_split()함수 제공

    # -- (2) 감성분석을 위한 함수 정의
    sentimental = function(sentences, positive, negative){
      
      scores = laply(sentences, function(sentence, positive, negative) {
        # sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
        # sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
        # sentence = gsub('\\d+', '', sentence)        # 숫자 제거
        # sentence = tolower(sentence)                 # 모두 소문자로 변경(단어가 모두 소문자 임)
        word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상) 
        words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
        
        pos.matches = match(words, positive)           # words의 단어를 posDic에서 matching
        neg.matches = match(words, negative)
        
        pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
        neg.matches = !is.na(neg.matches)
        
        score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정    
        return(score)
      }, positive, negative)
      
      scores.df = data.frame(score=scores, text=sentences)
      return(scores.df)
    }
    result = sentimental(data[,1], positive, negative)   # 100 줄 단위로 긍정어/부정어 사전을 적용한 점수 합계

    # -- score값을 대상으로 color 칼럼 추가
    result$color[result$score >=1] = "blue"
    result$color[result$score ==0] = "green"
    result$color[result$score < 0] = "red"

    # -- 감성분석 결과 차트보기
    # plot(result$score, col=result$color) # 산포도 색생 적용
    # barplot(result$score, col=result$color, main ="뉴스검색_PI(동원 신영수외 1)_20191030 감성분석 결과화면")   # 막대차트

    # ------------------------------
    # -- 3) 단어의 긍정/부정 분석 
    # ------------------------------

    # -- (1) 감성분석 빈도수 
    table(result$color)

    # -- (2) score 칼럼 리코딩 
    result$remark[result$score >=1] = "긍정"
    result$remark[result$score ==0] = "중립"
    result$remark[result$score < 0] = "부정"

    sentiment_result= table(result$remark)
    sentiment_result
    # -- (3) 제목, 색상, 원크기
    pie(sentiment_result,main="정경심", 
        col=c("violetred2","palegreen4","cornflowerblue"), radius=0.8)                # ->  1.2

    # ------------------------------
    # -- 3) 결과파일 저장
    # ------------------------------
    # -- (1) 결과 엑셀 저장
    write.csv(
      result,           # 파일에 저장할 데이터 프레임 또는 행렬
      file="Output/어쩌구저꺼구.csv",     # 데이터를 저장할 파일명
      row.names=F  # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
    )
    # -- (2) 결과 이미지 저장
    png(filename="Output/sentimental.png" , width=800, height=600)

    pie(sentiment_result,main="정경심 감성분석 결과", 
        col=c("cornflowerblue","violetred2","palegreen4"), radius=0.8)    

    dev.off()

}
# sentimental("조국", "C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R")