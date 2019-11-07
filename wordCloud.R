# ************************************************
# -- 워드 클라우드
#    input: 입력 파일명 (keyword)
#           저장 파일 경로(path) ex: "C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R/"
#    output: "${path}/Output/${keyword}/..."
# ************************************************
wordCloud <-  function(keyword,path){
        print('wordCloud start')
        print(keyword)
        print(path)
        # keyword <- "201911062"
        # path <- "C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R"
        output.path <- paste(path,"/Output/",keyword,sep="")
        if(!file.exists(output.path)) dir.create(output.path);

        setwd(output.path)

        data = readLines(paste("../../Input/",keyword,".csv", sep=""), encoding="UTF-8")
        head(data)
        # localeToCharset()
        # Encoding(data)
        # data<-iconv(data,'UTF-8')
        # Encoding(data)
        data<- gsub('[[:punct:]]', '', data) # 문장부호 제거
        data <- gsub('[[:cntrl:]]', '', data) # 특수문자 제거
        head(data)

        # -- (1) 문자열 처리를 위한 패키지 로딩 
        library(KoNLP)
        library(wordcloud)
        library(RColorBrewer)
        useSejongDic()
        # install.packages("KoNLP")
        # install.packages("wordcloud")
        # install.packages("tm")
        # install.packages("slam")
        # ------------------------------
        # -- 2) 대상 텍스트 분석
        # ------------------------------
        # -- (1) 명사 추출
        extractNoun(data)
        nouns = sapply(data, extractNoun, USE.NAMES=F)

        nouns = unlist(nouns)
        nouns = Filter(function(x){nchar(x) >= 2}, nouns) #1글자 이하인 명사 삭제

        unlist(nouns)
        write(unlist(nouns), paste(keyword,".txt", sep="")) 
        wc_data = read.table(paste(keyword,".txt", sep=""))

        # -- (2) wordcount 데이터 준비
        wordcount = table(wc_data)
        write(unlist(wordcount), paste(keyword,".txt", sep=""))
        head(sort(wordcount, decreasing=F), 100)

        # -- (3) wordcount 색상 정의
        palette = brewer.pal(8, "Set2")
        wordcloud(names(wordcount), freq = wordcount, scale=c(10,2), rot.per = 0.25, random.order=F, colors=palette, random.color=T)

        # -- (4) 대상 경로에 이미지 저장
        png(filename=paste(keyword,"_wordCloud",".png", sep="") , width=800, height=600)
        wordcloud(names(wordcount), freq = wordcount, scale=c(15,2), rot.per = 0.25, random.order=F, colors=palette, random.color=T)
        dev.off()
        
        # -- (5) 불필요 파일 삭제
        fn.pdf = 'Rplots.pdf'
        fn.txt = paste(keyword,".txt", sep="")
        if (file.exists(fn.pdf)) file.remove(fn.pdf)
        if (file.exists(fn.txt )) file.remove(fn.txt )
        print('wordCloud end')
}
wordCloud(keyword, path)