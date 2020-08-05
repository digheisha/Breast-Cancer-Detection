package_listing <- c('data.table', 'tidyverse','DT' , 'leaflet','plotly','ggthemes','ggplot2')
library(MASS)
library(reshape2)
library(reshape)
library(corrplot)
bc.data <- read.csv("data.csv", header=T, stringsAsFactors = F)
str(bc.data)
#The last column has all NA values, I will be removing it
bc.data$X <- NULL
bc.data <- bc.data[,-1]
bc.data$diagnosis <- factor(ifelse(bc.data$diagnosis=="B","Benign","Malignant"))
View(bc.data)
summary(bc.data)
sum(is.null(bc.data))
bc.data[duplicated(bc.data)]
head(bc.data)
ggplot(data=bc.data,aes(x=diagnosis)) + geom_bar() + geom_text(stat='Count',aes(label=..count..),vjust=-1)

data_mean <- bc.data[ ,c(2:11)]
data_se <- bc.data[ ,c(12:21)]
data_worst <- bc.data[ ,c(22:31)]
ggplot(data = melt(data_mean, id.var = "diagnosis"), mapping = aes(x = value)) + geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x') 
ggplot(data = melt(data_se, id.var = "diagnosis"), mapping = aes(x = value)) + geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x')  
ggplot(data = melt(data_worst, id.var = "diagnosis"), mapping = aes(x = value)) + geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x')                       

nc=ncol(bc.data)
df <- bc.data[,3:nc-1]
df$diagnosis <- as.integer(factor(df$diagnosis))-1
correlations <- cor(df,method="pearson")
corrplot(correlations, number.cex = .9, method = "number", order = "FPC",type = "upper", tl.cex=0.8,tl.col = "black")

cor.test(bc.data$radius_worst, bc.data$perimeter_mean)
plot_ly(data = bc.data, x = ~radius_worst, y = ~perimeter_mean, color = ~diagnosis) %>% layout(title = 'Perimeter mean v. Radius worst')

cor.test(bc.data$radius_worst, bc.data$area_worst)
plot_ly(data = bc.data, x = ~radius_worst, y = ~area_worst, color = ~diagnosis) %>% layout(title = 'Area worst v. Radius worst')

cor.test(bc.data$perimeter_worst, bc.data$radius_worst)
plot_ly(data = bc.data, x = ~perimeter_worst, y = ~radius_worst, color = ~diagnosis) %>% layout(title = 'Perimeter worst v. Radius worst')

cor.test(bc.data$texture_worst, bc.data$texture_mean)
plot_ly(data = bc.data, x = ~texture_worst, y = ~texture_mean, color = ~diagnosis) %>% layout(title = 'Texture worst v. Texture mean')
