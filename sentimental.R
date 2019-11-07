# ************************************************
# -- 감성분석
#    input: 입력 파일명 (keyword)
#           저장 파일 경로(path) ex: "C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R/"
#    output: "${path}/Output/${keyword}/..."
# ************************************************
sentimental <- function(keyword,path){
      print('sentimental start')
      print(keyword)
      print(path)
      
      # keyword <- "201911061"
      # path <- "C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R"
      output.path <- paste(path,"/Output/",keyword, "/",sep="")
      if(!file.exists(output.path)) dir.create(output.path);

      setwd(output.path)
      # setwd("C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R")
      # data = read.csv("Input/a.csv", encoding="UTF-8", header=F, sep="\n", quote="\n")
      data = readLines(paste("../../Input/",keyword,".csv", sep=""), encoding="UTF-8")

      # data[,1] <- gsub('[^[:alnum:][:blank:]+?&/\\-]', '', data[,1]) # 문장부호 제거
      # data[,2] <- gsub('[^[:alnum:][:blank:]+?&/\\-]', '', data[,1]) # 문장부호 제거
      data <- gsub('[^[:alnum:][:blank:]+?&/\\-]', '', data) # 문장부호 제거

      write.csv(
        data,           # 파일에 저장할 데이터 프레임 또는 행렬
        file=paste(keyword,".txt",sep="")    # 데이터를 저장할 파일명
        ,row.names=F # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
      )

      positive <- readLines("../../감성사전/positive.txt")
      negative <- readLines("../../감성사전/negative.txt")
      length(positive)                                                
      length(negative)
      length(data)
      typeof(data)

      names(data)
      library(plyr)                                                        # laply()함수 제공
      library(stringr)                                         # str_split()함수 제공

      # -- (2) 감성분석을 위한 함수 정의
      sentimental = function(sentences, positive, negative){
        scores = laply(sentences, function(sentence, positive, negative) {
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
      result = sentimental(data, positive, negative)   # 100 줄 단위로 긍정어/부정어 사전을 적용한 점수 합계

      # -- score값을 대상으로 color 칼럼 추가
      result$color[result$score >=1] = "blue"
      result$color[result$score ==0] = "green"
      result$color[result$score < 0] = "red"

      table(result$color)

      # -- (2) score 칼럼 리코딩
      result$remark[result$score >=1] = "긍정"
      result$remark[result$score ==0] = "중립"
      result$remark[result$score < 0] = "부정"

      sentiment_result= table(result$remark)
      # -- (3) 제목, 색상, 원크기
      pie(sentiment_result,main=keyword, 
          col=c("cornflowerblue","violetred2","palegreen4"), radius=0.8)                 # ->  1.2

      # ------------------------------
      # -- 3) 결과파일 저장
      # ------------------------------
      # -- (1) 결과 엑셀 저장
      write.csv(
        result,           # 파일에 저장할 데이터 프레임 또는 행렬
        file=paste(keyword,".csv",sep="") ,     # 데이터를 저장할 파일명
        row.names=F  # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
      )
      # -- (2) 결과 이미지 저장
      png(filename=paste(keyword,"_sentimental",".png",sep=""), width=800, height=600)
      pie(sentiment_result,main=keyword, 
          col=c("cornflowerblue","violetred2","palegreen4"), radius=0.8)    
      dev.off()
      print('sentimental end')
}
sentimental(keyword,path)