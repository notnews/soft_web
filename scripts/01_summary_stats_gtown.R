setwd(githubdir)
setwd("not_news/not_news_us_dev/")

## Load libraries 
library("tidyverse")
library("xtable")
#devtools::install_github("soodoku/goji")
library("goji")

## Loading the data
gtown        <- read_csv("output/us_media_clean_summarized_gt1k.csv")
gtown_weekly <- read_csv("output/us_media_clean_weekly_summarized_gt1k.csv")
gtown_yearly <- read_csv("output/us_media_clean_yearly_count.csv")
gtown_pred   <- read_csv("output/predictions/gtown_nyt_pred.csv")

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

## Outlets we want to keep for the analysos 
keeps <- c("BB - Bloomberg", "Bloomberg", "Daily Kos", "Houston Chronicle", 
           "Newswire Today", "NPR", "NPR World News", "Palestine Chronicle", 
           "Radio Liberty", "Senegambia News", "Speeple News", "Star Telegram", 
           "Stars & Stripes", "The Drudge Report", "The Saipan Tribune", "UNESCO News", 
           "United Nations Radio", "United Nations Radio News", "Wall Street Journal", 
           "world news", "Yahoo Alerts", "Yahoo Asia News", "Yahoo news", "Africa Daily", 
           "Africa Focus", "All Headline News", "Big News Network", "Big News Network (Bahamas)", 
           "Daily News Central", "Media Monitors", "Media-Newswire", "MediaLine Mideast",
           "Yahoo", "World News")


## Types of news
soft_news <- c("Arts", "Books", "Classifieds", "Dining", "Editorial", "Leisure", "Local", 
               "Obits", "Other", "Real Estate", "Sports", "Style", "Travel")  

hard_news <- c("Business Finance", "Foreign News", "National", "Science", "Health")

## GTOWN Summary Statistics Table
gtown <- 
  gtown %>% 
  rename(news_source = src_name) %>% 
  filter(news_source %in% keeps) %>% 
  mutate(news_source = str_squish(news_source),  
         news_source = ifelse(news_source == 'BB - Bloomberg', 'Bloomberg', news_source), 
         news_source = ifelse(news_source == 'Big News Network (Bahamas)', 'Big News Network', news_source), 
         news_source = ifelse(news_source == 'NPR World News', 'NPR', news_source), 
         news_source = ifelse(news_source == 'TheMediaLine Mideast News Source', 'MediaLine Mideast', news_source), 
         news_source = ifelse(news_source == 'radio liberty', 'Radio Liberty', news_source), 
         news_source = ifelse(news_source == 'world news', 'World News', news_source), 
         news_source = ifelse(news_source == 'United Nations Radio News', 'United Nations Radio', news_source), 
         news_source = ifelse(news_source == 'Yahoo Alerts', 'Yahoo Al', news_source), 
         news_source = ifelse(news_source == 'Yahoo Asia News', 'Yahoo Asia', news_source), 
         news_source = ifelse(news_source == 'Yahoo news', 'Yahoo', news_source)) %>% 
  separate(from_date, into = c("fy", "fm", "fd"), sep = "-") %>%
  mutate(fy = as.numeric(fy)) %>% 
  filter(fy < 2016) %>% 
  unite("from_date", c("fy", "fm"), sep = "-") %>% 
  separate(to_date, into = c("ty", "tm", "td"), sep = "-") %>% 
  mutate(ty = as.numeric(ty)) %>% 
  filter(ty < 2016) %>% 
  unite("to_date", c("ty", "tm"), sep = "-") %>% 
  select(-c("fd", "td")) %>% 
  rename("Source" = news_source,
         "Start Date" = from_date,
         "End Date" = to_date,
         "Total" = n_transcripts)  


print(xtable(gtown,  type = "latex", tabular.environment= "longtable",
               caption = "GTOWN - Summary Statistics",
               label = "tab:gtown_summary"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/01_1_gtown_sources_freq_time.tex")

rm(gtown)


## GTOWN WEEKLY Summary Statistics Table
gtown_weekly <- 
  gtown_weekly %>% 
  rename(news_source = sourceName) %>% 
  filter(news_source %in% keeps) %>% 
  mutate(news_source = str_squish(news_source),  
         news_source = ifelse(news_source == 'BB - Bloomberg', 'Bloomberg', news_source), 
         news_source = ifelse(news_source == 'Big News Network (Bahamas)', 'Big News Network', news_source), 
         news_source = ifelse(news_source == 'NPR World News', 'NPR', news_source), 
         news_source = ifelse(news_source == 'TheMediaLine Mideast News Source', 'MediaLine Mideast', news_source), 
         news_source = ifelse(news_source == 'radio liberty', 'Radio Liberty', news_source), 
         news_source = ifelse(news_source == 'world news', 'World News', news_source), 
         news_source = ifelse(news_source == 'United Nations Radio News', 'United Nations Radio', news_source), 
         news_source = ifelse(news_source == 'Yahoo Alerts', 'Yahoo', news_source), 
         news_source = ifelse(news_source == 'Yahoo Asia News', 'Yahoo Asia', news_source), 
         news_source = ifelse(news_source == 'Yahoo news', 'Yahoo', news_source)) %>% 
  rename("Source" = news_source, 
         "Total" = count,
         "Mean" = mean, 
         "Std. Dev." = std, 
         "Min" = min, 
         "Max" = max,  
         "Zeros" = zero_transcripts)
 
print(xtable(gtown_weekly, type = "latex", tabular.environment= "longtable",
             caption = "GTOWN - Summary Statistics, detailed",
             label = "tab:gtown_summary_detailed,"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/01_2_gtown_summary.tex")

rm(gtown_weekly)

## GTOWN YEARLY Summary Statistics Table

gtown_yearly <- 
  gtown_yearly %>% 
  filter(X1 %in% keeps) %>% 
  rename(news_source = X1) %>% 
  mutate(news_source = str_squish(news_source),  
         news_source = ifelse(news_source == 'BB - Bloomberg', 'Bloomberg', news_source), 
         news_source = ifelse(news_source == 'Big News Network (Bahamas)', 'Big News Network', news_source), 
         news_source = ifelse(news_source == 'NPR World News', 'NPR', news_source), 
         news_source = ifelse(news_source == 'TheMediaLine Mideast News Source', 'MediaLine Mideast', news_source), 
         news_source = ifelse(news_source == 'radio liberty', 'Radio Liberty', news_source), 
         news_source = ifelse(news_source == 'world news', 'World News', news_source), 
         news_source = ifelse(news_source == 'United Nations Radio News', 'United Nations Radio', news_source), 
         news_source = ifelse(news_source == 'Yahoo Alerts', 'Yahoo', news_source), 
         news_source = ifelse(news_source == 'Yahoo Asia News', 'Yahoo Asia', news_source), 
         news_source = ifelse(news_source == 'Yahoo news', 'Yahoo', news_source)) %>% 
  rename("Source" = news_source, "2000" = n_transcripts, "2001" = n_transcripts_1, "2002" = n_transcripts_2, "2003" = n_transcripts_3, 
         "2004" = n_transcripts_4, "2005" = n_transcripts_5, "2006" = n_transcripts_6, "2007" = n_transcripts_7,
         "2008" = n_transcripts_8, "2009" = n_transcripts_9, "2010" = n_transcripts_10, "2011" = n_transcripts_11, "2012" = n_transcripts_12, 
         "2013" = n_transcripts_13, "2014" = n_transcripts_14, "2015" = n_transcripts_15)

print(xtable(gtown_yearly, type = "latex", tabular.environment= "longtable",
             caption = "GTOWN - Yearly Counts by Source",
             label = "tab:gtown_yearly_counts"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/01_3_gtown_yearly.tex")

rm(gtown_yearly)

## GTOWN News Prediction
gtown_pred1 <- 
  gtown_pred %>% 
  select(news_source, label_name) %>% 
  filter(news_source %in% keeps) %>% 
  mutate(news_source = str_squish(news_source),  
         news_source = ifelse(news_source == 'BB - Bloomberg', 'Bloomberg', news_source), 
         news_source = ifelse(news_source == 'Big News Network (Bahamas)', 'Big News Network', news_source), 
         news_source = ifelse(news_source == 'NPR World News', 'NPR', news_source), 
         news_source = ifelse(news_source == 'TheMediaLine Mideast News Source', 'MediaLine Mideast', news_source), 
         news_source = ifelse(news_source == 'radio liberty', 'Radio Liberty', news_source), 
         news_source = ifelse(news_source == 'world news', 'World News', news_source), 
         news_source = ifelse(news_source == 'United Nations Radio News', 'United Nations Radio', news_source), 
         news_source = ifelse(news_source == 'Yahoo Alerts', 'Yahoo', news_source), 
         news_source = ifelse(news_source == 'Yahoo Asia News', 'Yahoo', news_source), 
         news_source = ifelse(news_source == 'Yahoo news', 'Yahoo', news_source)) %>% 
  add_count(news_source) %>% 
  rename(total = n) 

gtown_pred_1 <- 
  gtown_pred1 %>% 
  add_count(news_source, label_name) %>% 
  rename(label = n) %>% 
  mutate(news_source = str_squish(news_source)) %>% 
  unique() %>% 
  mutate(percent  = label/total,
         percent = round(percent,2)) %>% 
  select(-label) %>% 
  spread(key = label_name, value = percent) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  rename("News Source" = news_source,
         "Total" = total,
         "Business" = "Business Finance", 
         "Foreign" = "Foreign News") %>% 
  filter(Total > 999) 


print(xtable(gtown_pred_1, type = "latex", tabular.environment= "longtable",
             caption = "GTOWN Prediction - Share of News Types",
             label = "tab:gtown_pred_news_types"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/01_4_gtown_pred_summary.tex")

rm(gtown_pred_1)


gtown_pred_2 <- 
  gtown_pred1 %>% 
  mutate(label_name = ifelse(label_name == "Business Finance" | label_name == "Foreign News" |
                               label_name == "National" | label_name == "Science" | label_name == "Health",
                             "Hard", "Soft")) %>% 
  add_count(news_source, label_name) %>% 
  rename(label = n) %>% 
  unique() %>% 
  mutate(percent  = label/total,
         percent = round(percent,2)) %>% 
  select(-label) %>% 
  spread(key = label_name, value = percent) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  rename(Source = news_source,
         Total = total) %>% 
  filter(Total > 999) %>% 
  mutate(Source = trimws(Source, which = c("left"))) %>% 
  arrange(Source)

print(xtable(gtown_pred_2, type = "latex", tabular.environment= "longtable",
             caption = "GTOWN Prediction - Hard and Soft News",
             label = "tab:gtown_pred_hard_soft"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/01_5_gtown_pred_hard_soft.tex")
rm(gtown_pred_2)


## Graphs

# gtown_pred_sum <-
#   gtown_pred %>%
#    select(-c(total, label_name)) %>%
#   group_by(news_source) %>%
#   summarise_all(mean)
# write_csv(gtown_pred_sum, "not_news/not_news_us_dev/output/gtown_pred_label_means.csv")

gtown_pred2 <- 
  gtown_pred %>% 
  filter(news_source %in% keeps) %>% 
  select(-c(date, url,articleId)) %>% 
  mutate(news_source = str_squish(news_source),  
         news_source = ifelse(news_source == 'BB - Bloomberg', 'Bloomberg', news_source), 
         news_source = ifelse(news_source == 'Big News Network (Bahamas)', 'Big News Network', news_source), 
         news_source = ifelse(news_source == 'NPR World News', 'NPR', news_source), 
         news_source = ifelse(news_source == 'TheMediaLine Mideast News Source', 'MediaLine Mideast', news_source), 
         news_source = ifelse(news_source == 'radio liberty', 'Radio Liberty', news_source), 
         news_source = ifelse(news_source == 'world news', 'World News', news_source), 
         news_source = ifelse(news_source == 'United Nations Radio News', 'United Nations Radio', news_source), 
         news_source = ifelse(news_source == 'Yahoo Alerts', 'Yahoo', news_source), 
         news_source = ifelse(news_source == 'Yahoo Asia News', 'Yahoo Asia', news_source), 
         news_source = ifelse(news_source == 'Yahoo news', 'Yahoo', news_source)) %>% 
  add_count(news_source) %>% 
  rename(total = n)

# gtown_pred_sum <-
#   gtown_pred %>%
#    select(-c(total, label_name)) %>%
#   group_by(news_source) %>%
#   summarise_all(mean)
# write_csv(gtown_pred_sum, "not_news/not_news_us_dev/output/gtown_pred_label_means.csv")

gtown_pred2 <- 
  gtown_pred2 %>%  
  mutate(pred_soft = rowSums(select(., one_of(soft_news))),
         pred_hard = rowSums(select(., one_of(hard_news))),
         pred = pred_soft + pred_hard) %>% 
  select(news_source, label_name, pred_soft, pred_hard, pred, total)

gtown_pred_by_outlet <- 
  gtown_pred2 %>% 
  group_by(news_source) %>%
  summarize(share_of_not_news = mean(pred_soft))

avg_outlet_prop <- mean(gtown_pred_by_outlet$share_of_not_news, na.rm = T)

## Dot plot
gtown_pred_by_outlet$news_source  <- droplevels(factor(gtown_pred_by_outlet$news_source))
gtown_pred_by_outlet$news_source = factor(gtown_pred_by_outlet$news_source, levels = gtown_pred_by_outlet$news_source[order(gtown_pred_by_outlet$share_of_not_news)])
write_csv(gtown_pred_by_outlet, "not_news/not_news_us_dev/output/gtown_pred_by_outlet.csv")

main_outlets <- c("Yahoo", 
                  "Bloomberg", 
                  "Wall Street Journal",
                  "NPR", 
                  "Newswire Today",
                  "Daily Kos",
                  "Star Telegram",
                  "Houston Chronicle",
                  "The Drudge Report",
                  "Stars & Stripes",
                  "Daily News Central")

gtown_pred_by_outlet_main = subset(gtown_pred_by_outlet, news_source %in% main_outlets)


ggplot(gtown_pred_by_outlet_main , aes(share_of_not_news, news_source, order = share_of_not_news,)) +
  geom_point(aes(alpha = .8)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values = c("#dd3333", "#3333dd")) +
  scale_x_continuous("Proportion of Soft News Stories", breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1))) +
  cust_theme +
  theme(legend.position = "none")

ggsave("not_news/not_news_us_dev/figs/gtown_pred_dotplot.pdf")

## Density Plot
ggplot(gtown_pred_by_outlet , aes(share_of_not_news)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(avg_outlet_prop, 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(gtown_pred_by_outlet$share_of_not_news, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in an Outlet") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("not_news/not_news_us_dev/figs/gtown_pred_density_outlet.pdf")


ggplot(gtown_pred2 , aes(pred_soft)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(mean(gtown_pred2$pred_soft), 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(gtown_pred2$pred_soft, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in the Articles") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("not_news/not_news_us_dev/figs/gtown_pred_density_article.pdf")

rm(gtown_pred, gtown_pred1, gtown_pred2, gtown_pred_1, gtown_pred_2, gtown_pred_by_outlet, gtown_pred_by_outlet_main,
   keeps, soft_news, hard_news, main_outlets)

