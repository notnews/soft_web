#####################################################################################
##
##    File Name:        06_top10_pred_graphs.R
##    Date:             2019-06-30
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Notes:            
##
#####################################################################################
setwd(basedir)
setwd("db2")

## Libraries
library(rio)
library(tidyverse)
library(goji)

df_nyt16 <- import("predictions/agg_2016_nyt_pred.csv")

# Custom ggplot theme
cust_theme <- theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        legend.key       = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.title   = element_text(size = 10, color = "#555555"),
        axis.text    = element_text(size = 10, color = "#555555"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

soft_news <- c("Arts", "Books", "Classifieds", "Dining", "Editorial", "Leisure", "Local", 
               "Obits", "Other", "Real Estate", "Sports", "Style", "Travel")  
hard_news <- c("Business Finance", "Foreign News", "National", "Science","Health")

df_nyt16 <- 
  df_nyt16 %>%
  select(-c(url,date)) %>% 
  mutate(news_source = str_replace(news_source, "fox", "FOX News"),
         news_source = str_replace(news_source, "google", "Google"),
         news_source = str_replace(news_source, "hpmg", "Huffington Post"),
         news_source = str_replace(news_source, "nyt", "NYT"),
         news_source = str_replace(news_source, "usat", "USA Today"),
         news_source = str_replace(news_source, "wapo", "WaPo"),
         news_source = str_replace(news_source, "wsj", "WSJ"),
         news_source = str_replace(news_source, "yahoo", "Yahoo"))

df_nyt16 <- 
  df_nyt16 %>%  
  mutate(pred_soft = rowSums(select(., one_of(soft_news))),
         pred_hard = rowSums(select(., one_of(hard_news))),
         pred = pred_soft + pred_hard) %>% 
  select(news_source, label_name, pred_soft, pred_hard, pred)

df_nyt16_by_outlet <- 
  df_nyt16 %>% 
  group_by(news_source) %>%
  summarize(share_of_not_news = mean(pred_soft))

avg_outlet_prop <- mean(df_nyt16_by_outlet$share_of_not_news, na.rm = T)

## Dot plot
df_nyt16_by_outlet$news_source  <- droplevels(factor(df_nyt16_by_outlet$news_source))
df_nyt16_by_outlet$news_source = factor(df_nyt16_by_outlet$news_source, levels = df_nyt16_by_outlet$news_source[order(df_nyt16_by_outlet$share_of_not_news)])

ggplot(df_nyt16_by_outlet , aes(share_of_not_news, news_source, order = share_of_not_news,)) +
  geom_point(aes(alpha = .8)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values = c("#dd3333", "#3333dd")) +
  scale_x_continuous("Proportion of Soft News Stories", breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1))) +
  cust_theme +
  theme(legend.position = "none")

ggsave("~/Dropbox/Research/Repos/not_news/not_news_us_dev/figs/df_nyt16_dotplot.pdf")


ggplot(df_nyt16_by_outlet, aes(share_of_not_news)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(avg_outlet_prop, 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(df_nyt16_by_outlet$share_of_not_news, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in an Outlet") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("~/Dropbox/Research/Repos/not_news/not_news_us_dev/figs/df_nyt16_density_outlet.pdf")



## Density Plot
ggplot(df_nyt16, aes(pred_soft)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(mean(df_nyt16$pred_soft), 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(df_nyt16$pred_soft, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in Articles") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("~/Dropbox/Research/Repos/not_news/not_news_us_dev/figs/df_nyt16_density_article.pdf")


