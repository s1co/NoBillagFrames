#Clear workspace
rm(list = ls())
#Set working directory
setwd("richtiges wd einfügen")
#Set preferred options
#options(stringsAsFactors = FALSE)
#Load packages
library(feather)
library(ggplot2)
library(RColorBrewer)
library(dynlm)

df_1 <- data.frame(feather::read_feather('data_modified/data_r_1.feather'))
# Barplot für Frame 1
p<-ggplot(data=df_1, aes(x=Datetime, y=Count, fill=Percentage*100)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x = "Woche", y = "Anzahl Tweets zu #NoBillag")
# Graduelle Farbe defieren
p+scale_fill_gradient(low = "#295BCD", high = "#FE5A37", name = "Anteil Tweets\nder Medien (in %)")
ggsave("data_output/df_1.png", dpi=300)

df_2 <- data.frame(feather::read_feather('data_modified/data_r_2.feather'))
# Barplot für Frame 2
p<-ggplot(data=df_2, aes(x=Datetime, y=Count, fill=Percentage*100)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x = "Woche", y = "Anzahl Tweets zu #NoBillag")
# Graduelle Farbe defieren
p+scale_fill_gradient(low = "#295BCD", high = "#FE5A37", name = "Anteil Tweets\nder Medien (in %)")
ggsave("data_output/df_2.png", dpi=300)

df_3 <- data.frame(feather::read_feather('data_modified/data_r_3.feather'))
# Barplot für Frame 3
p<-ggplot(data=df_3, aes(x=Datetime, y=Count, fill=Percentage*100)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x = "Woche", y = "Anzahl Tweets zu #NoBillag")
# Graduelle Farbe defieren
p+scale_fill_gradient(low = "#295BCD", high = "#FE5A37", name = "Anteil Tweets\nder Medien (in %)")
ggsave("data_output/df_3.png", dpi=300)

df_4 <- data.frame(feather::read_feather('data_modified/data_r_4.feather'))
# Barplot für Frame 4
p<-ggplot(data=df_4, aes(x=Datetime, y=Count, fill=Percentage*100)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x = "Woche", y = "Anzahl Tweets zu #NoBillag")
# Graduelle Farbe defieren
p+scale_fill_gradient(low = "#295BCD", high = "#FE5A37", name = "Anteil Tweets\nder Medien (in %)")
ggsave("data_output/df_4.png", dpi=300)

df_5 <- data.frame(feather::read_feather('data_modified/data_r_5.feather'))
# Barplot für Frame 5
p<-ggplot(data=df_5, aes(x=Datetime, y=Count, fill=Percentage*100)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x = "Woche", y = "Anzahl Tweets zu #NoBillag")
# Graduelle Farbe defieren
p+scale_fill_gradient(low = "#295BCD", high = "#FE5A37", name = "Anteil Tweets\nder Medien (in %)")
ggsave("data_output/df_5.png", dpi=300)

df_6 <- data.frame(feather::read_feather('data_modified/data_r_6.feather'))
# Barplot für Frame 6
p<-ggplot(data=df_6, aes(x=Datetime, y=Count, fill=Percentage*100)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x = "Woche", y = "Anzahl Tweets zu #NoBillag")
# Graduelle Farbe defieren
p+scale_fill_gradient(low = "#295BCD", high = "#FE5A37", name = "Anteil Tweets\nder Medien (in %)")
ggsave("data_output/df_6.png", dpi=300)


# Barplot der gesamten Frames erstellen
## Neues df vorbereiten
labels <- data.frame(sum(df_1$Count))
colnames(labels) <- c("Anzahl")
labels["Frame"] <- data.frame(c("Demokratie"))

labels <- rbind(labels, setNames(data.frame(t(c(sum(df_2$Count),"Kommerzialisierung"))),c("Anzahl", "Frame")))
labels <- rbind(labels, setNames(data.frame(t(c(sum(df_3$Count),"Service Public"))),c("Anzahl", "Frame")))
labels <- rbind(labels, setNames(data.frame(t(c(sum(df_4$Count),"Finanzen"))),c("Anzahl", "Frame")))
labels <- rbind(labels, setNames(data.frame(t(c(sum(df_5$Count),"SRG"))),c("Anzahl", "Frame")))
labels <- rbind(labels, setNames(data.frame(t(c(sum(df_6$Count),"Libertarismus"))),c("Anzahl", "Frame")))


## Barplot erstellen
p<-ggplot(data=labels, aes(x=Frame, y=Anzahl)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal()
p
# Graduelle Farbe defieren
ggsave("data_output/frames_bar.png", dpi=300)


# Regressionen erstellen
linearTest <- dynlm(Count ~ L(Percentage, 1), data=df_1)
summary(linearTest)
linearTest <- dynlm(Count ~ L(Percentage, 1), data=df_2)
summary(linearTest)
linearTest <- dynlm(Count ~ L(Percentage, 1), data=df_3)
summary(linearTest)
linearTest <- dynlm(Count ~ L(Percentage, 1), data=df_4)
summary(linearTest)
linearTest <- dynlm(Count ~ L(Percentage, 1), data=df_5)
summary(linearTest)
linearTest <- dynlm(Count ~ L(Percentage, 1), data=df_6)
summary(linearTest)


