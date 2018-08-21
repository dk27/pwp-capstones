library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(MASS)


movies<-movies %>%
   mutate(feature_film=as.factor(ifelse(title_type =="Feature Film", "yes", "no")),
         drama=as.factor(ifelse(genre=="Drama", "yes", "no")),
         mpaa_rating_R=as.factor(ifelse(mpaa_rating=="R", "yes", "no")),
         oscar_season=as.factor(ifelse(thtr_rel_month %in% c(10:12), "yes", "no")),
         summer_season=as.factor(ifelse(thtr_rel_month %in% c(5:8), "yes", "no")))
table(movies$summer_season)

### Regression
movies_no_na = na.omit(movies)
m_score <- glm(audience_score ~ feature_film+drama+runtime+mpaa_rating_R+thtr_rel_year+
              oscar_season+summer_season+imdb_rating+imdb_num_votes+
              critics_score+best_pic_nom+best_pic_win+best_actor_win+
              best_actress_win+best_dir_win+top200_box, data = movies_no_na)
summary(m_score)
BIC(m_score)
m_resid=resid(m_score) 

plot(movies_no_na$audience_score, m_resid,
       ylab="Residuals",xlab="Audience Score",
       main="Residuals vs Fitted")
abline(0,0)

m_score_log <- glm(audience_score ~ feature_film+drama+runtime+mpaa_rating_R+thtr_rel_year+
                 oscar_season+summer_season+imdb_rating+imdb_num_votes+
                 critics_score+best_pic_nom+best_pic_win+best_actor_win+
                 best_actress_win+best_dir_win+top200_box, data = movies,
                 family=gaussian(link="log"))
summary(m_score_log)
plot(m_score_log)

ggplot(movies, aes(x=critics_score, y=audience_score)) +
  geom_point(shape=1) + geom_smooth(method=lm) 

ggplot(movies, aes(x=critics_score, y=audience_score, color=mpaa_rating_R)) +
  geom_point(shape=1) + geom_smooth(method=lm) 

ggplot(movies, aes(x=critics_score, y=audience_score, color=drama)) +
  geom_point(shape=1) + geom_smooth(method=lm) 

hist(movies$runtime)
hist(movies$imdb_rating)

BIC(m_score_log)
BIC(m_score)

m_score_log1<-glm(audience_score~feature_film+runtime+imdb_rating+imdb_num_votes,
                  data=movies, family=gaussian(link="log"))

null<-glm(audience_score~1,
                  data=movies, family=gaussian(link="log"))
BIC(m_score_log1)
summary(m_score_log1)
plot(m_score_log1)
stp<-step(m_score_log, scope = list(lower=null,upper=m_score_log),
     direction="both", k=log(651))
summary(stp)
stp$anova

### let's do BMA
movies_no_na = na.omit(movies)
bma_movies = bas.lm(log(audience_score)~ . -audience_score, data = movies_no_na,
                   prior = "BIC", 
                   modelprior = uniform())
bma_movies
summary(bma_movies)

summary(subset(movies, select=c(runtime, imdb_rating, imdb_num_votes,
                      critics_score, audience_score)))

### EDA


summary(movies$audience_score)
hist<-qplot(movies$audience_score,
            geom="histogram",
            binwidth = 10,  
            main = "Histogram for Audience Score", 
            xlab = "Audience Score",  
            fill=I("lightcyan3"), 
            col=I("dodgerblue4"), 
            alpha=I(.7),
            xlim=c(0,100))
hist  

  movies %>% 
  group_by(feature_film) %>%
  summarise(avg_score = round(mean(audience_score),0), 
            min_score = min(audience_score), 
            max_score = max(audience_score),
            median_score=round(median(audience_score),0),
            StDev_score=round(sd(audience_score),0),
            total = n())
  
  movies %>% 
    group_by(drama) %>%
    summarise(avg_score = round(mean(audience_score),0), 
              min_score = min(audience_score), 
              max_score = max(audience_score),
              median_score=round(median(audience_score),0),
              StDev_score=round(sd(audience_score),0),
              total = n())
  
  movies %>% 
    group_by(mpaa_rating_R) %>%
    summarise(avg_score = round(mean(audience_score),0), 
              min_score = min(audience_score), 
              max_score = max(audience_score),
              median_score=round(median(audience_score),0),
              StDev_score=round(sd(audience_score),0),
              total = n())

  movies %>% 
    group_by(oscar_season) %>%
    summarise(avg_score = round(mean(audience_score),0), 
              min_score = min(audience_score), 
              max_score = max(audience_score),
              median_score=round(median(audience_score),0),
              StDev_score=round(sd(audience_score),0),
              total = n())
  
  movies %>% 
    group_by(summer_season) %>%
    summarise(avg_score = round(mean(audience_score),0), 
              min_score = min(audience_score), 
              max_score = max(audience_score),
              median_score=round(median(audience_score),0),
              StDev_score=round(sd(audience_score),0),
              total = n())
  
  ### graphs
  p1 <- ggplot(movies, aes(x = summer_season, y = audience_score)) +
    geom_boxplot(fill = "lightcyan3", colour = "dodgerblue4",alpha = 0.7, outlier.colour = "tomato4")+ 
    scale_x_discrete(name = "Summer Season?") +
    scale_y_continuous(name = "Audience Score",breaks = seq(0, 100, 20))+ 
    ggtitle("Audience Score Distribution by Summer Season")+ 
    theme_bw()
  p1
  
  hist<-qplot(movies$audience_score,
        geom="histogram",
        binwidth = 10,  
        main = "Histogram for Audience Score", 
        xlab = "Audience Score",  
        fill=I("lightcyan3"), 
        col=I("dodgerblue4"), 
        alpha=I(.7),
        xlim=c(0,100))
hist  

 p2 <- ggplot(movies, aes(drama)) + geom_bar(aes(fill=oscar_season), width = 0.7) + 
  labs(title="Audience Score by Feature Film",
       subtitle="Feature Film during Oscar Season")
 p2 
  