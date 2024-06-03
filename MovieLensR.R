
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r,include=FALSE}

library(stringr)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
library(stats)
library(corrplot)
library(tidyr)
library(lubridate)
library(data.table)
library(DT)
library(knitr)
library(methods)
library(gridExtra)
library(grid)
library(corrplot)
library(Matrix)
library(corrplot)
library(RColorBrewer)
library(magrittr)
library(stringi)
library(viridis)
library(heuristica)
library(MatrixStats)
library(gam)
library(modelr)
library(tidyselect)
library(tidyr)
library(tibble)
library(purr)
library(broom)
library(forcats)
library(DBI)
library(hexbin)


```{r,echo=TRUE}
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",dl)
ratings <- read.table(text=gsub("::","\t",readLines(unzip(dl,-10m100K/ratings.dat"))),
  col.names=c("userID","movieID","rating","timestamp"))
  ```
 
  ```{r}
  movies<-str_split_fixed(readLines(unzip(dl,"ml-10M100K/movies.dat")),"\\::",3)
  colnames(movies)<-c("movieid","title","genres")
  movies<-as.data.frame(movies) %>% mutate(movieid=as.numeric(levels(movieid))[movieid],title=as.character(title),genres=as.character(genres))
  

movielens<-left_join(ratings,movies,by="movieid")
nrow(movielens)
n_distinct(movielens$movieid)
n_distinct(movielens$genres)
n_distinct(movielens$userid)
```

```{r}
set.seed(1)
test_index<-createDataPartition(y=movielens$rating,times=1,p=0.1,list=FALSE)
edx<-movielens[-test_index,]
temp<-movielens[test_index,]
```

```{r}
validation<-temp%>% semi_join(edx,by="movieid")%>% semi_join(edx,by="userid")
```

```{r}
removed<-anti_join(temp,validation)
edx<-rbind(edx,removed)
```

```{r}
head(edx)
glimpse(edx)
```
```{r}
n_distinct(edx$userid)
n_distinct(edx$genres)
n_distinct(edx$movieid)
nrow(edx)
```

```{r}
edx<-mutate(edx,year_rated=year(as_datetime(timestamp)))
head(edx)
```

```{r}
premier<-stringi::stri_extract(edx$title,regex="(\\d{4})",comments=TRUE)%>%as.numeric()
edx_with_title_dates<-edx%>%mutate(premier_date=premier)
head(edx_with_title_dates)
```
```{r}
edx_with_title_dates<-edx_with_title_dates%>%select(-timestamp)
head(edx_with_title_dates)

edx_with_title_dates%>%filter(premier_date>2018)%>%group_by(movieid,title,premier_date)%>%summarize(n=n())
edx_with_title_dates%>%filter(premier_date<1900)%>%group_by(movieid,title,premier_date)%>%summarize(n=n())

edx_with_title_dates[edx_with_title_dates$movieid=="27266","premier_date"]<-2004
edx_with_title_dates[edx_with_title_dates$movieid=="671","premier_date"]<-1996
edx_with_title_dates[edx_with_title_dates$movieid=="2308","premier_date"]<-1973
edx_with_title_dates[edx_with_title_dates$movieid=="4159","premier_date"]<-2001
edx_with_title_dates[edx_with_title_dates$movieid=="5310","premier_date"]<-1985
edx_with_title_dates[edx_with_title_dates$movieid=="8864","premier_date"]<-2004
edx_with_title_dates[edx_with_title_dates$movieid=="1422","premier_date"]<-1997
edx_with_title_dates[edx_with_title_dates$movieid=="4311","premier_date"]<-1998
edx_with_title_dates[edx_with_title_dates$movieid=="5472","premier_date"]<-1972
edx_with_title_dates[edx_with_title_dates$movieid=="6290","premier_date"]<-2003
edx_with_title_dates[edx_with_title_dates$movieid=="6645","premier_date"]<-1971
edx_with_title_dates[edx_with_title_dates$movieid=="8198","premier_date"]<-1960
edx_with_title_dates[edx_with_title_dates$movieid=="8905","premier_date"]<-1992
edx_with_title_dates[edx_with_title_dates$movieid=="53953","premier_date"]<-2007
```

```{r}
edx_with_title_dates<-edx_with_title_dates%>%mutate(age_of_movie=2018-premier_date,rating_date_range=year_rated-premier_date)
head(edx_with_title_dates)
```
```{r,message=FALSE}
edx%>%group_by(movieid)%>%summarize(n=n()%>%ggplot(aes(n))+geom_histogram(fill="cadetblue3",color="grey20",bins=10)+scale_x_log10()+ggtitle("Number of Movies Ratings")
```
```{r,message=FALSE}
edx%>%group_by(userid)%>%summarize(n=n())%>%ggplot(aes(n))+geom_histogram(fill="cadetblue3",color="grey20",bins=10)+scale_x_log10()+ggtitle("Number of Users Ratings")
```
```{r}
user_avgs<-edx_with_title_dates%>%group_by(userid)%>%summarize(avg_user_rating=mean(rating))
movie_avgs<-edx_with_title_dates%>%group_by(movieid)%>%summarize(avg_movie_rating=mean(rating))
age_avgs<-edx_with_title_dates%>%group_by(age_of_movie)%>%summarize(avg_rating_by_age=mean(rating))
year_avgs<-edx_with_title_dates%>%group_by(year_rated)%>%summarize(avg_rating_by_year=mean(rating))
head(age_avgs)
```
```{r}
head(user_avgs)
```
```{r,message=FALSE}
age_avgs%>%ggplot(aes(age_of_movie,avg_rating_by_age))+geom_point()+ggtitle("Age of the movie vs. Average of the movie ratings")
```
```{r,message=FALSE}
user_avgs%>%ggplot(aes(userid,avg_user_rating))+geom_point(alpha=1/20,colour="blue")+ggtitle("Users vs. The Average User Rating")
```
```{r}
summary(lm(avg_rating_by_age~age_of_the_movie,data=age_avgs))
```
```{r,message=FALSE}
avg_rating.lm<-lm(avg_rating_by_age~age_of_movie,data=age_avgs)
avg_rating.res<-resid(avg_rating.lm)
plot(age_avgs$age_of_movie,avg_rating.res,ylab='residuals',xlab='age of the movie',main='The average rating by the age of the movie')+abline(0,0)
```
```{r,message=FALSE}
#movies that are younger than 75 years old
age_of_movie_less_than75<-age_avgs%>%filter(age_of_movie<75)
#movies that are younger than 75 years old versus the movie rating average
age_of_movie_less_than75<-ggplot(aes(age_of_movie,avg_rating_by_age))+geom_point()+ggtitle("Age of the movie Vs. Movie Rating Average")
```
```{r}
age_lessthan75_rating.lm<-lm(avg_rating_by_age~age_of_movie,data=age_of_movie_less_than75)
summary(age_lessthan75_rating.lm)
```
```{r,message=FALSE}
head(age_of_movie_less_than75)
age_lessthan75.res<-resid(age_lessthan75_rating.lm)
plot(age_of_movie_less_than75$age_of_movie,age_lessthan75.res,ylab='residuals',xlab='The Age of the Movie',main='Movie Rating Average by the age of the movie')+abline(0,0)
```
```{r}
age_between20_and_75<-age_avgs%>%filter(age_of_movie>20)&(age_of_movie<75))
```
```{r,message=FALSE}
age_between20_and_75%>%ggplot(aes(age_of_movie,avg_rating_by_age))+geompoint()+ggtitle("Movies between the ages of 30 to 75 vs. Movie rating average")
```
```{r}
summary(lm(avg_rating_by_age~age_of_movie,data=age_between20_and_75))
```
```{r}
ages_between20_and_40<-age_avgs%>%filter((age_of_movie<20)&(age_of_movie<40))
age_between20_and_40%>%ggplot(aes(age_of_movie,avg_rating_by_age))+ggeom_point()+ggtitle("The Age of the Movies between 20 and 40 years old")
```
```{r}
summary(lm(avg_rating_by_age~age_of_movie,data=age_between20_and_40))
```
```{r,message=FALSE}
age_less_than30<-age_avgs%>%filter((age_of_movie>30))
#Movies that are less than 30 years old with an average movie rating
age_less_than30%>%ggplot(aes(age_of_movie,avg_rating_by_age))+geom_point()+ggtitle('Age of the Movie less than 30 years old')
```
```{r}
summary(lm(avg_rating_by_age~age_of_movie,data=age_less_than30))
```
```{r}
dat<-edx_with_title_dates%>%separate_rows(genres,sep="\\|")
head(dat)
```
```{r}
genre_count_by_movieid<-dat%>%group_by(movieid,genres)%>%summarize(n=n())
head(genre_count_by_movieid)
```
```{r}
number_of_genres<-dat%>%group_by(genres)%>%summarize(n=n())
number_of_genres
```
```{r}
genre_list<-number_of_genres$genres
genre_list
```
```{r}
temp<-dat%>%group_by(genres)%>%summarize(n=n())%>%ungroup()%>%mutate(sumN=sum(n),percentage=n/sumN)%>%arrange(-percentage)
```
```{r,message=FALSE}
temp%>%ggplot(aes(reorder(genres,percentage),percentage,fill=percentage))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="Y10rRd")+labs(y="percentage",x="genre")+ggtitle("Distributionof Genres by the percentage of movies rates")
```
```{r,message=FALSE}
temp<-dat%>%group_by(genres)%>%summarize(mean_rating_by_genre=mean(rating))%>%arrange(-mean_rating_by_genre)
temp%>%ggplot(aes(reorder(mean_rating_by_genre),mean_rating_by_genre,fill=mean_rating_by_genre))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="y10rRd")+labs(y="Mean rating",x="genre")+ggtitle("Average Rating of genres")
```
```{r}
avg_rating_greater_than_4<-edx%>%group_by(title)%>%summarize(mean_rating=mean(rating),n=n())%>%filter(mean_rating>=4)%>%arrange(desc(n=mean_rating))
avg_rating_greater_than_4%>%filter(n>=10000)%>%ggplot(aes(reorder(title,n),n,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+xlab("movie")+ylab("number of ratings")+ggtitle("Movies with an average rating\ngreater than or equal to 4\nand number of ratings>10000")
```
```{r,message=FALSE}
avg_between3_4<-edx%>%group_by(title)%>%summarize(mean_rating=mean(rating),n=n())%>%filter(n>10000,(mean_rating>=3&mean_rating<4))%>%arrange(desc(n,mean_rating))
p<-avg_between3_4%>%slice(1:40)
p%>%ggplot(aes(reorder(title,n),n,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("Average ratings 3<=r<4 and n >10000")+xlab('movie')+ylab('Number of ratings')+theme_classic()
```
```{r, message=FALSE}
avg_between2_3<-edx%>%group_by(title)%>%summarize(mean_rating=mean(rating,n=n())%>%filter(n>5000,(mean_rating>=2 & mean_rating<3))%>%arrange(desc(n,mean_rating))
avg_between2_3%>%ggplot(aes(reorder(title,n),n,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("Average ratings 2<=r<3 and n>5000")+xlab('Movie')+ylab('Number of Ratings')+theme_classic()
```

avg_rating_less_than_2<-edx%>%group_by(title)%>%summarize(mean_rating=mean(rating),n,n=n())%>%filter(n>500,mean_rating<2)%>%arrange(desc(n,mean_rating))
avg_rating_less_than_2%>%ggplot(aes(reorder(title, n),n,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("Average rating<2")+xlab('Movie')+ylab('Number of Ratings')+theme_classic()
```
```{r,message=FALSE}
p<-mean(edx$rating)
edx%>%group_by(title)%>%summarize(b_i=mean(rating-p),n,n=n())%>%filter(b_i>0.5,n>10000)%>%ggplot(aes(reorder(title,b_i),b_i,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("")+xlab("Movie Title")+ggtitle("Movie Rating-p\n for Number of Ratings>10000")+theme_classic()
```
```{r,message=FALSE}
movie_avgs<-edx%>%group_by(movieid)%>%summarize(b_i=mean(rating-p))
movie_reg_avgs<-edx%>%group_by(movieid)%>%summarize(b_i=sum(rating-p)/(n()+1),n_i=n())
movie_titles<-edx%>%select(movieid,title)%>%distinct()
edx_with_avgs<-edx%>%group_by(title,movieid)%>%summarize(n=n())%>%left_join(movie_reg_avgs,by="movieid")%>%arrange(desc(b_i,n))
edx_with_avgs%>%filter(n>15000)%>%ggplot(aes(reorder(title,b_i),b_i,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("")+xlab("movie title")+ggtitle("Regularized Averages\nnumber of ratings>20000")+theme_classic()
```
```{r,message=FALSE}
head(edx_with_avgs)
b<-edx_with_avgs%>%arrange(b_i)%>%filter(b_i<-2)%>%arrange((b_i))
b
b%>%ggplot(aes(reorder(title,b_i),b_i,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("")+xlab("movie title")+ggtitle("Regularized movie averages b_i<-2")+theme_classic()
```
```{r,message=FALSE}
edx_with_avgs%>%filter(n>10000,b_i<0.0)%>%ggplot(aes(reorder(title,b_i),b_i,fill=n))+geom_bar(stat="identity")+coord_flip()+scale_fill_distiller(palette="PuBuGn")+ggtitle("")+xlab("movie title")+ggtitle("Regularized movie averages\n number of ratings>20000")+theme_classic()
```
```{r}
n_movies_ratings<-edx_with_title_dates%>%group_by(movieid)%>%summarize(n=n())
avg_movie_rating<-edx_with_title_dates%>%group_by(movieid)%>%summarize(avg_movie_rating=mean(rating))
corr_dat<-edx_with_title_dates%>%select(rating,userid,movieid,age_of_movie,year_rated,premier_date,rating_date_range)%>%leftjoin(n_movies_ratings,by="movieid")%>%left_join(avg_movie_rating,by="movieid")
head(corr_dat)
```
```{r,message=FALSE}
temp<-corr_dat%>%select(one_of("movieid","userid","rating","age_of_movie","year_rated","premier_date","n","rating_date_range","avg_movie_rating"))%>%as.matrix()
A<-corr(temp,use="pairwise.complete.obs")
corrplot(A,order="hclust",addrect=2,type="lower",col=brewer.pal(n=8,name="RdBu"))
```
```{r,message=FALSE}
corr_by_age_of_movie<-corr_dat%>%filter((age_of_movie>20)&(age_of_movie<70))
temp<-corr_by_age_of_movie%>%select(one_of("rating","userid","movieid","n","premier_date","rating_date_range","age_of_movie","avg_movie_rating"))%>%as.matrix()
A<-cor(temp,use="pairwise.complete.obs")
corrplot(A,order="hclust",addrect=2,type="lower",col=brewer.pal(n=8,name="RdBu"))
```
```{r,message=FALSE}
get_cor<-function(df){
  a<-cor(df$x,df$y,use="pairwise.complete.obs");bx<-substitute(italic(r)==cor,list(cor=format(a,digits=2)))
  as.character(as.expression(eq));
}
corr_dat%>%ggplot(aes(n,avg_movie_rating))+stat_bin_hex(bins=50)+scale_fill_distiller(palette="Spectral")+stat_smooth(method="lm",color="orchid",size=7)+ylab("Average movie rating")+xlab("Number of ratings")
```
```{r,message=FALSE}
corr_dat%>%ggplot(aes(age_of_movie,avg_movie_rating))+stat_bin_hex(bins=50)+scale_fill_distiller(palette="Spectral")+stat_smooth(method="lm",color="orchid",size=1)+annotate("text",x=75,y=0.9,label=get_cor(data.frame(x=corr_by_age_of_movie$age_of_movie,y=corr_by_age_of_movie$avg_movie_rating)),parse=TRUE,color="orchid",size=7)+ylab("Average movie rating")+xlab("Age of movie")
```
```{r,message=FALSE}
RMSE<-function(true_ratings,predicted_ratings){
  sqrt(mean(true_ratings-predicted_ratings)^2))}

}
lambdas<-seq(0,5,0.5)
rmses<-sapply(lambdas,function(1){
  r<-mean(edx_with_title_dates$rating)
  bi<-edx_with_title_dates%>%group_by(movieid)%>%summarize(bi=sum(rating-r)/(n=n()+1))
  ba<-edx_with_title_dates%>%left_join(bi,by="movieid")%>%group_by(userid)%>%summarize(ba=sum(rating-bi-r)/(n()+1))
  predicted_ratings<-edx_with_title_dates%>%left_join(bi,by="movieid")%>%left_join(ba,by="userid")%>%mutate(pred=r+bi+ba)%>%.$pred
  return(RMSE(predicted_ratings,edx_with_title_dates$rating))})
qqplot(lambdas,RMSEs)
lambdas(which.min(RMSEs))
``