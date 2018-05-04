
## Twitter influencer function

library(tidyverse)
library(readxl)

Twitter_influencer_df <- function(df, platform ="Twitter", N = 200, min_follower = 5000) {
  proper_case <- platform
  platform <- tolower(platform)
 
   ## global variables
  count <-  df %>% 
    select(Author, Country, `Query Id`, `Page Type`, contains(proper_case))  %>% 
    filter(`Page Type` == platform) %>%
    group_by(`Twitter Author ID`, Author) %>% 
    count() %>%
    ungroup() %>%
    summarise(max = max(n))
  
  max_followers_df = max(df$`Twitter Followers`,na.rm=T)
  min_followers_df = min(df$`Twitter Followers`, na.rm = T)
  max_retweets_df = max(df$`Twitter Retweets`, na.rm = T)
  min_retweets_df = min(df$`Twitter Retweets`, na.rm = T)
  print(max_followers_df)
  

  ## weights
  default_weights <- c(0.3,0.3,0.4)
  
  ## start manipulation
  tmp <- df %>% 
    select(Author, Country, `Query Id`, `Page Type`, contains(platform))  %>% 
    filter(`Page Type` == platform) %>%
    group_by(`Twitter Author ID`, Author) %>%
    
    ## summarize
    summarise(max_follower = max(`Twitter Followers`),
              max_following = max(`Twitter Following`),
              mentions = n(),
              Retweets_average = mean(`Twitter Retweets`, na.rm = T),
              Sum_of_Retweets = sum(`Twitter Retweets`, na.rm = T),
              Normalised_Followers = (max_follower - min_followers_df) /(max_followers_df - min_followers_df),
              Normalised_Avg_Retweets = (Retweets_average - min_retweets_df) / (max_retweets_df - min_retweets_df),
              Percent_rank_mentions = (mentions/count$max),
              Influencer_score = (Normalised_Followers*0.3) + (Normalised_Avg_Retweets*0.3) + (mentions*0.4)) %>%
    arrange(desc(Influencer_score)) %>%
    ungroup() %>%
    top_n(N)
  
    tmp <- tmp %>%
      select(Author, Mentions = mentions, `Max num of Twitter Followers`= max_follower,
             `Sum of Twitter Retweets` = Sum_of_Retweets, `Average Retweets` = Retweets_average,
             `Normalise Followers` = Normalised_Followers, `Normalise Avg. Retweets` = Normalised_Avg_Retweets,
             `Percent Rank Mentions` = Percent_rank_mentions, `INFLUENCER SCORE` = Influencer_score)
 
    tmp <- tmp  %>% 
     filter(`Max num of Twitter Followers` > min_follower) %>% 
     mutate(max_ = max(`INFLUENCER SCORE`))  %>% 
     mutate(norm_score = `INFLUENCER SCORE`/ max_ * 100)  %>% 
     select(-max_) 
    
  return(tmp)

}


