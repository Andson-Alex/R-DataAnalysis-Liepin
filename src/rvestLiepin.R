library(rvest)
library(dplyr)
library(stringr)
library(xml2)
library(readr)
library(tidyverse)

setwd("E:/Rcase/practices")
#首先，构建仅爬取一页的Function:
crawl_one_page <- function(key, i) {
  #key 为爬取岗位关键词， i 为页码
  base_url <- "https://www.liepin.com/zhaopin/?&fromSearchBtn=2"
  key <- iconv(key, to = "UTF-8") # 保证关键词文本为UTF-8编码
  #构建URL并加码。注意：别忘了curPage是从0开始的。
  liepin_url <- str_c(base_url, "&key=", key, "&curPage=", (i-1)) %>% URLencode()
  #由于网站信息可能随时更新，所以下载网页方便验证数据准确性。
  #构建文件路径，绝对路径
  file_name <- "E:/Rcase/practices/html/scrapepage.html" 
  download_html(liepin_url, file_name) 
  #下面就是真正从网页上抓取数据了
  lp_html <- read_html(file_name)
  position <- lp_html %>%     #岗位名称
    html_nodes(".job-info h3 a") %>%
    html_text() %>%
    str_trim() #删除多余的空白
  company <- lp_html %>%      #公司名称
    html_nodes(".company-name a") %>%
    html_text()
  #岗位要求，例如：12-18万_北京_大专及以上_5年以上
  req_info <- lp_html %>%     
    html_nodes(".condition") %>%
    html_attr("title") #get title attribute
  req_tb<- str_split(req_info, "_", simplify = TRUE) %>% as_tibble()
  ##req_tb<- str_split(req_info, "_", simplify = TRUE) %>% as.data.frame() %>% t()
  
 
  colnames(req_tb) <- c('salary', 'city', 'education', 'experience')
  pos_tb <- tibble(position, company) %>% bind_cols(req_tb)
  print(pos_tb)
  pos_tb <- pos_tb[which(pos_tb$salary != "面议" ),]
  
  pos_tb$salary <- sub('万', '', pos_tb$salary) %>% strsplit('-') %>%
    lapply(function(x){as.numeric(x) %>% mean()}) %>% unlist()
  return(pos_tb)
}

#之后，构建爬取n个网页的function：
crawl_n_page <- function(key, n){
  print("Hello World!!!")
  pos_lt <- vector("list", n)
  for (i in seq_len(n)) {
    pos_lt[[i]] <- crawl_one_page(key, i) 
    cat("Crawling page:", i, "\tProgress:", (i/n)*100, "%\n") #显示进度
    Sys.sleep(1)
  }
  pos_tb <- bind_rows(pos_lt) #将list转换为tibble
  return(pos_tb)
}

#爬取10页数据分析岗位招聘信息
position <- "人工智能"
#首先尝试爬取1页是否成功： pos_tb <- crawl_one_page(position, 4)
#然后爬取10页内容
pos_tb <- crawl_n_page(position, 100)

write.csv(pos_tb,file="E:/Rcase/practices/data/ai_info032801.csv")
