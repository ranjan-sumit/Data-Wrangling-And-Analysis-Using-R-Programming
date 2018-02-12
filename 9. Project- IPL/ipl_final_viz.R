# Raw data from @cricsheet
rm(list = ls())

setwd("~/Documents/sirpi/training /jan2018_datascience_workshop/code/ipl_final")
library(tidyverse)
library(cowplot)

"ipl_final.csv" %>%
  read_csv(
    skip = 1, 
    trim_ws = TRUE,
    col_names = c("infokey_or_ball",
                  "infoval_or_innings",
                  "over_and_ball",
                  "batting_team",
                  "striker",
                  "nonstriker",
                  "bowler",
                  "runs_off_bat",
                  "extras",
                  "kind_of_wicket",
                  "dismissed_player")) -> raw_data

raw_data %>%
  filter(infokey_or_ball == "ball") %>% 
  separate(over_and_ball, into = c("over","ball"), convert = TRUE) %>% 
  mutate(runs = runs_off_bat + extras) %>% 
  mutate(over = over + 1) %>% 
  group_by(batting_team,over) %>%
  mutate(cum_runs_overwise = cumsum(runs)) %>%
  ungroup() %>%
  group_by(batting_team) %>%
  mutate(cum_runs_teamwise = cumsum(runs)) -> tidy_data
  
tidy_data %>%
  ggplot(aes(x=ball,y=runs,fill=batting_team)) +
  geom_col(position ="dodge") + 
  facet_wrap(~over,scales = "free_x",nrow = 1) + 
  theme_grey() +
  theme(legend.position = "top",
        legend.justification = "left",
        title = element_text(face = "bold",size = 48), 
        axis.title = element_text(face="bold"),
        legend.text = element_text(face="bold",size = 48),
        legend.key.size = unit(5, 'lines')) +
  geom_text(aes(label=ifelse(extras == 0,"",paste0(extras,"E")),colour = batting_team),y = -0.1,show.legend = "none",size = 4) + 
  geom_text(aes(label=ifelse(kind_of_wicket %>% is.na(),"","W"),colour = batting_team),y = -0.2,show.legend = "none",size = 4) + 
  scale_fill_manual(values=c("#004BA0", "#d11d9b"),labels = c("MI","RPS")) +
  scale_colour_manual(values=c("#004BA0", "#d11d9b"),labels = c("MI","RPS")) +
  labs(title = "IPL2017 T20 Final - Runs Distribution",
       subtitle = "",
       caption = "source: @cricsheet, graph: @lan24hd",
       y = "Runs",
       fill = "",
       colour = "") -> p1

pdf_ratio <- 3
"p1.pdf" %>% ggsave(p1,width = 10.24*pdf_ratio,height = 7.60*pdf_ratio, unit = "in")


tidy_data %>%
  ggplot(aes(x=ball,y=cum_runs_overwise,color=batting_team)) +
  geom_step(alpha = 0.9,size=3) + 
  facet_wrap(~over,scales = "free_x",nrow = 1) + 
  theme_grey() +
  theme(legend.position = "top",
        legend.justification = "left",
        title = element_text(face = "bold",size = 48), 
        axis.title = element_text(face="bold"),
        legend.text = element_text(face="bold",size = 48),
        legend.key.size = unit(5, 'lines')) +
  geom_text(aes(label=ifelse(extras == 0,"",paste0(extras,"E")),colour = batting_team),y = -0.1,show.legend = "none",size = 4) + 
  geom_text(aes(label=ifelse(kind_of_wicket %>% is.na(),"","W"),colour = batting_team),y = -0.2,show.legend = "none",size = 4) + 
  scale_fill_manual(values=c("#004BA0", "#d11d9b"),labels = c("MI","RPS")) +
  scale_colour_manual(values=c("#004BA0", "#d11d9b"),labels = c("MI","RPS")) +
  labs(title = "IPL2017 T20 Final - Runs Distribution",
       subtitle = "",
       caption = "source: @cricsheet, graph: @lan24hd",
       y = "Cumulative runs overwise",
       fill = "",
       colour = "") -> p2

"p2.pdf" %>% ggsave(p2,width = 10.24*pdf_ratio,height = 7.60*pdf_ratio, unit = "in")

tidy_data %>%
  ggplot(aes(x=ball,y=cum_runs_teamwise,color=batting_team)) +
  geom_line(size=3) + 
  facet_wrap(~over,scales = "free_x",nrow = 1) + 
  theme_grey() +
  theme(legend.position = "top",
        legend.justification = "left",
        title = element_text(face = "bold",size = 48), 
        axis.title = element_text(face="bold"),
        legend.text = element_text(face="bold",size = 48),
        legend.key.size = unit(5, 'lines')) +
  geom_text(aes(label=ifelse(extras == 0,"",paste0(extras,"E")),colour = batting_team),y = -0.1,show.legend = "none",size = 4) + 
  geom_text(aes(label=ifelse(kind_of_wicket %>% is.na(),"","W"),colour = batting_team),y = -0.2,show.legend = "none",size = 4) + 
  scale_fill_manual(values=c("#004BA0", "#d11d9b"),labels = c("MI","RPS")) +
  scale_colour_manual(values=c("#004BA0", "#d11d9b"),labels = c("MI","RPS")) +
  labs(title = "IPL2017 T20 Final - Runs Distribution",
       subtitle = "",
       caption = "source: @cricsheet, graph: @lan24hd",
       y = "Cumulative runs",
       fill = "",
       colour = "") -> p3

"p3.pdf" %>% ggsave(p3,width = 10.24*pdf_ratio,height = 7.60*pdf_ratio, unit = "in")

p1_mod <- p1 + theme(legend.position = "top",
                     legend.text = element_text(face="bold",size = 24),
                     legend.key.size = unit(3, 'lines'),
                     axis.title.y = element_text(size = 30),
                     plot.margin = unit(c(1,1,-5,1),"lines")) + 
  labs(title = "IPL2017 T20 Final - Runs Distribution",caption ="", x="")
p2_mod <- p2 + theme(legend.position = "none",
                     axis.title.y = element_text(size = 30),
                     plot.margin = unit(c(-5,1,-5,1),"lines")) + labs(title = "",caption ="", x="") 
p3_mod <- p3 + theme(legend.position = "none",
                     axis.title.y = element_text(size = 30),
                     plot.margin = unit(c(-5,1,1,1),"lines")) + labs(title = "") 
#grid.arrange(p1_mod,p2_mod,p3_mod)  -> ipl_plot
plot_grid(p1_mod,p2_mod,p3_mod, ncol = 1, rel_heights = c(1.3,1,1.2)) -> ipl_plot





"ipl_plot.pdf" %>% ggsave(ipl_plot,width = 10.24*pdf_ratio,height = 7.60*pdf_ratio, unit = "in")


