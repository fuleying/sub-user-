library(RODBC)
conn <- odbcConnect("kingdee", uid = "fuleying", pwd = "aaaa")

mydata <- sqlQuery(conn, "select FPRODUCTREGNO , FACCOUNTNAME , FACCOUNTUSER , FADDITIONALINFO , FFUNCTIONKEY , 
	FOPERATIONCOUNT , FOPERATIONDAY , FSENDDATE from t0801 where FPRODUCTREGNO like '306016823%'")
mydata2 <- sqlQuery(conn, "select FPRODUCTREGNO , FACCOUNTNAME , FACCOUNTUSER , FADDITIONALINFO , FFUNCTIONKEY , 
	FOPERATIONCOUNT , FOPERATIONDAY , FSENDDATE from t0802 where FPRODUCTREGNO like '306016823%'")
mydata <- rbind(mydata,mydata2)
#
#重复至t0930
#
mydata2 <- sqlQuery(conn, "select FPRODUCTREGNO , FACCOUNTNAME , FACCOUNTUSER , FADDITIONALINFO , FFUNCTIONKEY , 
	FOPERATIONCOUNT , FOPERATIONDAY , FSENDDATE from t0930 where FPRODUCTREGNO like '306016823%'")
mydata <- rbind(mydata,mydata2)


#三种保存数据的方式
#换行时该方法会使数据列分开显示
sink("t89.txt")
mydata
sink()

#明确使用","作为分割，默认的"|"分割在换行时容易出现问题
write.table(mydata,file= "t89.txt",sep =";",col.names =NA)

#最好的完整保存数据的方法
save(mydata,file = "kingdee.RData")


#对应的读取数据的两种方法
t89 <- read.table("t89.txt", header = TRUE , sep = ";")

load("C:\\Users\\fuleying\\Documents\\kingdee.RData")


#将提取的数据导入数据库

#创建数据表
CREATE TABLE `kingdee`.`t89` (
  `FNUM` VARCHAR(50) NULL, 	#提取的数据多了一列递增索引
  `FPRODUCTREGNO` VARCHAR(100) NULL,
  `FACCOUNTNAME` VARCHAR(200) NULL,
  `FACCOUNTUSER` VARCHAR(50) NULL,
  `FADDITIONALINFO` VARCHAR(60) NULL,
  `FFUNCTIONKEY` VARCHAR(128) NULL,
  `FOPERATIONCOUNT` DECIMAL(10,0) NULL,
  `FOPERATIONDAY` DECIMAL(20,0) NULL,
  `FSENDDATE` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP);

#将数据导入数据表
load data local infile "D:/kingdee/t89.txt" into TABLE t89 fields terminated by ';';
