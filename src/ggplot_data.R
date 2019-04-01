#readxl包的read_excel函数
# install.packages("readxl")
##install.packages("VIM")

#install.packages("jiebaR")
#install.packages("jiebaRD")
#install.packages("wordcloud2")
library(readxl)
library(VIM)
library(zoo)
library(jiebaR)
library(jiebaRD)
library(plyr)

# 数据加载
#AI.df <- read.csv(file.choose())
AI.df <- read_excel(file.choose())
str(AI.df)
#VIM包的aggr函数来识别

aggr(AI.df,prop=TRUE,numbers=TRUE)


# ggplot画图
library(ggplot2)
# library(plyr)
library(wordcloud2)
attach(AI.df)

#创建ggplot绘图对象
p.cn <- ggplot(AI.df)+my.ggplot.theme()

#问题1：不同地区，数据分析岗位的需求分布以及对应的薪资分布
city.table <- data.frame(prop.table(table(reorder_size(city,TRUE))))
city.table <- city.table[c(1:15),]
##str(city.table)
##print(city.table)
names(city.table)[1] <- "city"
p1 <- ggplot(city.table,aes(x=city,y=Freq)) + my.ggplot.theme()+
  geom_bar(fill="turquoise3",stat = "identity") + 
  labs(x="城市",y="不同城市需求占总量的比率",
       title="“上海，深圳，北京，广州，杭州”占据了近90%的需求量,\n是供应链管理方向的首选") +
  scale_y_continuous(labels = scales::percent)

# group_diff <- diff(range(salary))/20
group_diff <- diff(range(salary))/150
p2 <- p.cn + geom_histogram(aes(x=salary,y=..density..),
                            binwidth = group_diff,fill="turquoise3",color="white")+
  stat_density(geom = "line",position = "identity",
               aes(x=salary),color="brown1")+
  labs(x="年薪(w/年)",
       title="供应链管理方向平均年薪为25.7w，年薪的分布空间比较大，主要集中在12w~30w之间") +
  scale_x_continuous(limits = c(0,100)) + scale_y_continuous(limits = c(0,0.04))

multiplot(p1,p2,cols = 1)


p.cn$type <- NA
p.cn$type[p.cn$city %in% top.freq(city,5)$x] <- "top5"
p.cn$type[is.na(p.cn$type)] <- "other"
p.cn$type <- factor(p.cn$type,levels=c("top5","other"))

p.cn + geom_boxplot(aes(x=city,y=salary,fill=p.cn$type))+
  labs(x="城市",y="年薪(w/年)",
       title="各城市供应链管理相关岗位的平均年薪",fill="平均年薪")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1)) + scale_y_continuous(limits = c(5,40))


#问题2：不同经验，数据分析岗位的需求分布以及对应的薪资分布
exp.table <- prop.table(table(experience))
exp.table <- as.data.frame(exp.table)
p3 <- ggplot(exp.table,aes(x=experience,y=Freq)) + my.ggplot.theme()+
  geom_bar(fill="turquoise3",stat = "identity") + 
  labs(x="工作经验",y="不同工作经验需求占总量的比率",
       title="企业需要更有经验的供应链专家，主要需求\n在3年以上工作经验") +
  scale_y_continuous(labels = scales::percent)

p4 <- p.cn + geom_boxplot(aes(x=experience,y=salary),fill="turquoise3")+
  labs(x="工作经验",y="平均年薪(w/年)",
       title="工作经验在3年以上，供应链\n相关岗位的年薪有非常可观的增长") + scale_y_continuous(limits = c(0,60))

multiplot(p3,p4,cols = 2)

#问题3：不同学历，数据分析岗位的需求分布以及对应的薪资分布

edu.table <- prop.table(table(education))
edu.table <- as.data.frame(edu.table)
p5 <- ggplot(edu.table,aes(x=education,y=Freq)) + my.ggplot.theme()+
  geom_bar(fill="turquoise3",stat = "identity") + 
  labs(x="学历",y="不同学历需求占总量的比率",title="超过60%的岗位需要本科及以上的学历") +
  scale_y_continuous(labels = scales::percent)
p6 <- p.cn + geom_boxplot(aes(x=education,y=salary),fill="turquoise3")+
  labs(x="学历",y="年薪（w/年）",
       title="供应链相关职位年薪与学历的关系不明显") + scale_y_continuous(limits = c(0,75))
multiplot(p5,p6,cols = 2)


#问题4：不同企业规模，数据分析岗位的各项需求分布及薪资分布

scale.table <- data.frame(prop.table(table(scale)))
p7 <- ggplot(scale.table,aes(x=scale,y=Freq)) + my.ggplot.theme()+
  geom_bar(fill="turquoise3",stat = "identity") + 
  labs(x="企业规模",y="不同企业规模需求占总量的比率",title="接近90%的需求量集中在150人以上\n规模的企业")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))+
  scale_y_continuous(labels = scales::percent)
p8 <- p.cn + geom_boxplot(aes(x=scale,y=salary),fill="turquoise3")+
  labs(x="企业规模",y="月薪(K/月)",
       title="150人以下规模且需快速发展的企业\n愿意给出更高的薪酬")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))
multiplot(p7,p8,cols = 2)


#企业规模与工作经验要求分析
sc.exp <- data.frame(prop.table(table(scale,experience),1))
ggplot(data=sc.exp,aes(x=scale,y=Freq,fill=experience)) + my.ggplot.theme()+
  geom_bar(stat = "identity")+
  labs(x="企业规模",y="需求比例",fill="工作经验",
       title="50~150人规模的企业对分析师的工作经验要求最高")+
  geom_text(aes(label=paste(round(sc.exp$Freq,3)*100,'%',sep = '')),colour = "white",
            position=position_stack(.5), vjust=00)+
  scale_y_continuous(labels = scales::percent)


#问题5：探索数据分析岗位对工具型技能的需求

#工具型技能分布
key.df <- data.frame(table(reorder_size(merge.df$keyword,TRUE)))
key.df$Freq <- key.df$Freq/length(CN.clean$id)
ggplot(key.df)+my.ggplot.theme() + 
  geom_bar(aes(x=Var1,y=Freq),fill = "turquoise3",stat = "identity") + 
  labs(x="工具型技能",y="不同技能需求占总岗位需求量的比率",
       title="SQL,R,Python,Excel是数据分析师的必备技能，超过78%的岗位都要求掌握SQL，
       \nR语言的需求量居于第二") +
  theme(axis.text.x = element_text(angle = 30,hjust = 1))+
  geom_text(aes(x=Var1,y=Freq,label=paste(round(key.df$Freq,3)*100,'%',sep = '')),vjust=-0.2)+
  scale_y_continuous(labels = scales::percent)


#工具型技能与工作经验的分析
key.exp <- data.frame(prop.table(table(merge.df$keyword,merge.df$experience),2))
names(key.exp)[1:2] <- c("kw","exp")
ggplot(key.exp)+my.ggplot.theme()+
  geom_bar(aes(x=kw,y=Freq,fill=exp),stat = "identity")+
  labs(x="工具型技能",y="比率",fill="工作经验",
       title="工作经验要求越高对BI、EXCEL等office工具的需求越少，
       \n对数据库、Hadoop和Hive等大数据工具的需求越高")+
  facet_grid(exp~.)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))+
  geom_text(aes(x=kw,y=Freq,label=paste(round(key.exp$Freq,3)*100,'%',sep = '')),vjust=0,size=3)+
  scale_y_continuous(labels = scales::percent)


#工具型技能与薪资的分析
merge.df$type <- NA
merge.df$type[merge.df$keyword %in% top.freq(merge.df$keyword,5)$x] <- "top5"
merge.df$type[is.na(merge.df$type)] <- "other"
merge.df$type <- factor(merge.df$type,levels=c("top5","other"))
ggplot(merge.df) + my.ggplot.theme()+
  geom_boxplot(aes(x=keyword,y=avg_salary,fill=merge.df$type))+
  labs(x="工具型技能",y="月薪分布(K/月)",
       title="掌握Spark，Hadoop，Hive，R，Python等工具可以获得更高的薪资",fill="需求量排名")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))


#问题6：探索非工具型关键词的需求

#生成词云
wordcloud2(not.tool.keyword, size = 0.9, fontFamily = "微软雅黑")