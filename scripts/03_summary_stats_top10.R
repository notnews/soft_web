setwd(githubdir)
setwd("not_news/not_news_us_dev/")

## Load libraries
library("tidyverse")
library("xtable")
library("goji")
library("lubridate")



soft_news <- c("Arts", "Books", "Classifieds", "Dining", "Editorial", "Leisure", "Local", 
               "Obits", "Other", "Real Estate", "Sports", "Style", "Travel")  

hard_news <- c("Business Finance", "Foreign News", "National", "Science","Health")

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



top10_12 <- read_csv("~/Dropbox/db2/top10/agg_2012.csv")
top10_16 <- read_csv("~/Dropbox/db2/top10/agg_2016.csv")
df_top10_pred     <- read_csv("output/predictions/top10_2016_2017_nyt_pred.csv")
df_top10_pred2012 <- read_csv("output/predictions/top10_2012_nyt_pred.csv") %>% 
  mutate(date = ymd(date),
         news_source = ifelse(news_source == "fox", "foxnews", news_source),
         news_source = ifelse(news_source == "hpmg", "huffingtonpost", news_source),
         news_source = ifelse(news_source == "usat", "usatoday", news_source))

df_top10_pred <- 
  df_top10_pred %>% 
  select(-src_list) %>% 
  bind_rows(df_top10_pred2012)

top10_12 <-
  top10_12 %>% 
  select(src, date, data_cat) %>% 
  mutate(src = ifelse(src == "fox_politics", "fox", src),
         date = as.Date(date, "%d%h%Y"),
         min_date = min(date),
         max_date = max(date),
         data_cat = str_extract(data_cat, "[aA-zZ]+")) %>% 
  separate(min_date, into = c("year", "month", "day")) %>% 
  select(-c(day, date)) %>% 
  unite("min_date", c("year", "month"), sep = "-") %>% 
  separate(max_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("max_date", c("year", "month"), sep = "-")

top10_16 <-
  top10_16 %>% 
  select(src, date, data_cat) %>% 
  mutate(date = as.Date(date, "%d%h%Y"),
         min_date = min(date),
         max_date = max(date),
         data_cat = str_extract(data_cat, "[aA-zZ]+")) %>% 
  separate(min_date, into = c("year", "month", "day")) %>% 
  select(-c(day, date)) %>% 
  unite("min_date", c("year", "month"), sep = "-") %>% 
  separate(max_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("max_date", c("year", "month"), sep = "-") 

top10 <- rbind(top10_12, top10_16)

top10_1 <-
  top10 %>% 
  group_by(src, data_cat) %>% 
  mutate(min_date = min(min_date),
         max_date = max(max_date)) %>% 
  add_count(src, data_cat) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(src = ifelse(src == "wsj", "Wall Street Journal", src),
         src = ifelse(src == "fox", "Fox News", src),
         src = ifelse(src == "hpmg", "Huffington Post", src),
         src = ifelse(src == "usat", "USA Today", src),
         src = ifelse(src == "yahoo", "Yahoo!", src),
         src = ifelse(src == "nyt", "New York Times", src),
         src = ifelse(src == "wapo", "Washington Post", src),
         src = ifelse(src == "google", "Google", src),
         data_cat = str_replace(data_cat, "top", "Top"),
         data_cat = str_replace(data_cat, "homepage", "Homepage"),
         data_cat = str_replace(data_cat, "politics", "Politics")) %>% 
  arrange(src, min_date) %>% 
  rename("Source" = src, 
         "Category" = data_cat,
         "Start Date" = min_date,
         "End Date" = max_date,  
         "Total" = n)

print(xtable(top10_1, type = "latex", tabular.environment= "longtable",
             caption = "Top 10 - Summary Statistics by Category",
             label = "tab:top10_summary_category"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/06_1_top10_summary.tex")

rm(top10_1)


top10_2 <-
  top10 %>% 
  select(-data_cat) %>% 
  group_by(src) %>%
  mutate(min_date = min(min_date),
         max_date = max(max_date)) %>% 
  ungroup() %>% 
  mutate(src = ifelse(src == "wsj", "Wall Street Journal", src),
         src = ifelse(src == "fox", "Fox News", src),
         src = ifelse(src == "hpmg", "Huffington Post", src),
         src = ifelse(src == "usat", "USA Today", src),
         src = ifelse(src == "yahoo", "Yahoo!", src),
         src = ifelse(src == "nyt", "New York Times", src),
         src = ifelse(src == "wapo", "Washington Post", src),
         src = ifelse(src == "google", "Google", src)) %>% 
  group_by(src) %>% 
  add_count(src) %>% 
  unique() %>% 
  rename("Source" = src, 
         "Start Date" = min_date,
         "End Date" =  max_date, 
         "Total" = n)


print(xtable(top10_2, type = "latex", tabular.environment= "longtable",
             caption = "Top 10 - Summary Statistics by Channel",
             label = "tab:top10_summary_by_channel"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/06_2_top10_summary_channel.tex")

rm(top10_2)


## Top10 Pred Summary 
df_top10_pred1 <- 
  df_top10_pred %>%  
  select(-date) %>% 
  unique() %>% 
  select(news_source, label_name) %>% 
  mutate(news_source = str_replace(news_source, "foxnews", "Fox News"),
         news_source = str_replace(news_source, "google", "Google"),
         news_source = str_replace(news_source, "huffingtonpost", "Huffington Post"),
         news_source = str_replace(news_source, "nyt", "NYT"),
         news_source = str_replace(news_source, "usatoday", "USA Today"),
         news_source = str_replace(news_source, "washingtonpost", "WaPo"),
         news_source = str_replace(news_source, "wsj", "WSJ"),
         news_source = str_replace(news_source, "yahoo", "Yahoo!")) %>% 
  add_count(news_source) %>% 
  rename(total = n)  

df_top10_pred1_1 <- 
  df_top10_pred1 %>%  
  add_count(news_source, label_name) %>% 
  rename(label = n) %>% 
  unique() %>% 
  mutate(percent  = label/total,
         percent = round(percent,2)) %>% 
  select(-label) %>% 
  spread(key = label_name, value = percent) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  rename("News Source" = news_source,
         "Total" = total,
         "Business" = "Business Finance", 
         "Foreign" = "Foreign News") 

print(xtable(df_top10_pred1_1, type = "latex", 
             tabular.environment= "longtable",
             size = "small",
             floating.environment = "sidewaystable",
             caption = "Top 10 - Share of News",
             label = "tab:top10_summary"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/06_3_top10_pred_summary.tex")

rm(df_top10_pred1_1)

## Top10 Pred Summary 
df_top10_pred1_2 <- 
  df_top10_pred1 %>%   
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
         Total = total)

#mean(df_top10_pred_2$Hard)
#mean(ddf_top10_pred_2$Soft)
print(xtable(df_top10_pred1_2, type = "latex", tabular.environment= "longtable",
             caption = "Top 10 - Share of Hard and Soft News",
             label = "tab:top10_share_hard_soft"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/06_4_top10_pred_hard_soft.tex")

rm(df_top10_pred1_2)

## Top10 Predicxtion figures
df_top10_pred2 <- 
  df_top10_pred %>%  
  mutate(pred_soft = rowSums(select(., one_of(soft_news))),
         pred_hard = rowSums(select(., one_of(hard_news))),
         pred = pred_soft + pred_hard) 

df_top10_yearly <- 
  df_top10_pred2 %>% 
  separate(date, into = "year", sep = "-") %>% 
  unique() %>% 
  select(news_source, year, pred_soft) %>% 
  mutate(news_source = str_replace(news_source, "foxnews", "FOX News"),
         news_source = str_replace(news_source, "google", "Google"),
         news_source = str_replace(news_source, "huffingtonpost", "Huffington Post"),
         news_source = str_replace(news_source, "nyt", "NYT"),
         news_source = str_replace(news_source, "usatoday", "USA Today"),
         news_source = str_replace(news_source, "washingtonpost", "WaPo"),
         news_source = str_replace(news_source, "wsj", "WSJ"),
         news_source = str_replace(news_source, "yahoo", "Yahoo")) %>% 
  group_by(news_source, year) %>%
  summarize(mean = mean(pred_soft),
            sd   = sd(pred_soft),
            mean_p2sd = mean + 2 * sd,
            mean_m2sd = mean - 2 * sd) %>%
  ungroup() 

lcols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
linetype = c("dashed", "dotted", "dotdash", "longdash", "twodash", "4C88C488", "12345678")

df_top10_yearly %>% 
  filter(news_source != "USA Today") %>% 
  filter(news_source != "Yahoo") %>% 
ggplot(aes(x = year, y= mean, group = news_source)) + 
  geom_line(aes(color = news_source)) + 
  #geom_line(aes(color = news_source, linetype = news_source)) + 
  scale_color_manual(name = "News Source", values = lcols) +
  #scale_linetype_manual(name = "News Source", values = linetype) +
  #geom_hline(yintercept= avg_outlet_prop, color = "black", linetype = "dashed") + 
  ylim(0,1) +
  theme_minimal() + xlab("Year") + ylab("Share of Soft News in an Outlet")

ggsave("figs/top10_soft_time.pdf")

## Top10 Pred Summary 
df_top102 <- 
  df_top10_pred2 %>%  
  select(-date) %>% 
  unique() %>% 
  select(news_source, label_name, pred_soft) %>% 
  mutate(news_source = str_replace(news_source, "foxnews", "FOX News"),
         news_source = str_replace(news_source, "google", "Google"),
         news_source = str_replace(news_source, "huffingtonpost", "Huffington Post"),
         news_source = str_replace(news_source, "nyt", "NYT"),
         news_source = str_replace(news_source, "usatoday", "USA Today"),
         news_source = str_replace(news_source, "washingtonpost", "WaPo"),
         news_source = str_replace(news_source, "wsj", "WSJ"),
         news_source = str_replace(news_source, "yahoo", "Yahoo")) %>% 
  add_count(news_source) %>% 
  rename(total = n) %>% 
  add_count(news_source, label_name) %>% 
  rename(total_label = n) 

df_top102_by_outlet <- 
  df_top102 %>% 
  group_by(news_source) %>%
  summarize(share_of_not_news = mean(pred_soft))

avg_outlet_prop <- mean(df_top102_by_outlet$share_of_not_news, na.rm = T)

## Dot plot
df_top102_by_outlet$news_source  <- droplevels(factor(df_top102_by_outlet$news_source))
df_top102_by_outlet$news_source = factor(df_top102_by_outlet$news_source, levels = df_top102_by_outlet$news_source[order(df_top102_by_outlet$share_of_not_news)])
#write_csv(df_top102_by_outlet, "output/df_top102_by_outlet.csv")

ggplot(df_top102_by_outlet, aes(share_of_not_news, news_source, order = share_of_not_news,)) +
  geom_point(aes(alpha = .8)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values = c("#dd3333", "#3333dd")) +
  scale_x_continuous("Proportion of Soft News Stories", breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1))) +
  cust_theme +
  theme(legend.position = "none")

ggsave("figs/top10_pred_dotplot.pdf")

## Density Plot
ggplot(df_top102_by_outlet, aes(share_of_not_news)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(avg_outlet_prop, 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(df_top102_by_outlet$share_of_not_news, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in an Outlet") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("figs/top10_pred_density_outlet.pdf")
rm(df_top102_by_outlet)

ggplot(df_top102, aes(pred_soft)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(mean(df_top102$pred_soft), 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(df_top102$pred_soft, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in Articles") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("figs/top10_pred_density_article.pdf")

rm(df_top102, avg_outlet_prop, df_top10_pred2, df_top10_yearly)
