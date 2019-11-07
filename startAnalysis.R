# ************************************************
# -- 감성 분석 & 워드클라우드 
#    input: 저장 파일 경로 / 키워드       
# ************************************************
# -- (1) argument 가져오기 (keyword, path)
args <- commandArgs(TRUE)
keyword <- args[1]
path <- args[2]

# -- (2) 감성분석 
sentimental.path <- paste(path,"R_script/sentimental.R", sep="")
source(sentimental.path, encoding="UTF-8")

# -- (3) 워드 클라우드 
wordCloud.path <- paste(path,"R_script/wordCloud.R", sep="")
source(wordCloud.path, encoding="UTF-8")