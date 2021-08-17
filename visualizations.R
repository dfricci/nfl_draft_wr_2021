library(formattable)
library(knitr)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(data.table)

#re-format Data
rownames(result) <- 1:nrow(result) 
colnames(result) <- c("Player", "School", "Round", "1st Round", "2nd Round", 
                      "3rd Round", "4th Round ", "5th Round", "6th Round", "7th Round", "Undrafted")

format <- result[, c(1, 2, 4:11)]
format <- format[order(-format$`1st Round`),]
rownames(format) <- NULL

formattable(format, align = c("l", "l", rep("r", ncol(format)-2)),
            list('Player' = formatter("span", style = ~ style(color = "dark gray",
                                                              font.weight = "bold")),
           area(col = 3:10) ~ function(x) percent(x, digits = 1),
           area(col = 3:10) ~ color_tile("#DeF7E9", "#71CA97"))
            )

##GGPlot
ranks <-read.csv(file.choose(), header = TRUE)
format <- format %>%
  left_join(ranks, by = c("Player" = "player"))
format <- format[,c(1:10, 13:14) ]
colnames(format)[3] <- "one"


#Visual
wr_draft_pick <- ggplot(format, aes(x = actual, y = one)) +
#  geom_segment(aes(x = 0, xend = 249, y = 0 + 1, yend = 0 + 1*249), alpha = 0.5, color = "gray") +
  geom_point(shape = 21, fill = "blue", size = 3) +
  geom_text_repel(aes(label = Player), color = "black", fontface = "bold", force = 1, point.padding = 0.1,
                  segment.size = 0.2) +
  labs(x = "Actual Pick",
       y = "Percentage Chance of Being a 1st Round Pick",
       title = "Model Performance",
       subtitle = "2021 NFL Draft Wide Receivers") +
  theme_bw()+
  geom_smooth(method = NULL , formula = y ~ log(x), size = 1, linetype = 0) +
  theme(axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "red3", face = "bold"),
        plot.caption = element_text(size = 12))
wr_draft_pick
ggsave("model_plot.png", wr_draft_pick, 
       height = 6, width = 8, units = "in", dpi = 350)


