# Define mean Q1, Q2, Q3 and Q4

fakgrad <- Analysedata_Ansatte

fakgrad$Q1 <- rowMeans(fakgrad[,11:13], na.rm = TRUE)
fakgrad$Q2 <- rowMeans(fakgrad[,14:16], na.rm = TRUE)
fakgrad$Q3 <- rowMeans(fakgrad[,17:19], na.rm = TRUE)
fakgrad$Q4 <- rowMeans(fakgrad[,20:22], na.rm = TRUE)


fakgradlong <- fakgrad %>%
  pivot_longer(
    cols = starts_with("M"),
    names_to = "Måned",
    values_to = "Fakgrad",
    values_drop_na = TRUE
  )

fakgradlong$Måned <- factor(fakgradlong$Måned, levels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12"))

# Plot alle ansatte 



ggplot(fakgradlong, aes(x = Måned, y = Fakgrad))+
  stat_summary(aes(y = Fakgrad,group=1), fun = mean, colour="red", geom="line",group=1)


ggplot(fakgradlong, aes(x = Måned, y = Fakgrad))+
  geom_boxplot()+
  geom_jitter(aes(col = Type, shape = Avdeling))

ggplot(fakgradlong, aes(x = Måned, y = Fakgrad))+
  geom_boxplot()+
  geom_point(aes(col = Type, shape = Avdeling))+
  facet_wrap(~ Stillingsnivå)

ggplot(fakgradlong, aes(x = Måned, y = Fakgrad))+
  geom_boxplot()+
  geom_point(aes(col = Stillingsnivå, shape = Avdeling))+
  facet_wrap(~ Type)

oppsummering <- fakgradlong %>%
  group_by(Type, Stillingsnivå) %>%
  summarise(
    Snitt = mean(Fakgrad),
    Median = median(Fakgrad)
  )


model <- lm(Q1 ~ Type + Avdeling + Stillingsnivå, data = fakgradlong)
summary(model)