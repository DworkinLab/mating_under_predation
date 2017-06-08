#eff_plot <- effect("DGRP", mod2)
#eff_plot <- as.data.frame(eff_plot)
#eff_plot
#p_eff <- ggplot(eff_plot, aes(y = fit, x = reorder(DGRP,fit))) + 
#  geom_point(size = 2) + 
#  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2) +
#  labs(y = "Fit", x = "DGRP Line") + 
#  ggtitle("DGRP effect plot") +
#  geom_hline(yintercept = 0.5, size=0.25) +
#  theme(plot.title = element_text(size=22)) +
#  theme(axis.text.x = element_text(angle=90, hjust = 1))
#p_eff

courtship_time_sum <- lmer(sum ~ Box + Replicate + TempCent + HumCent + BPCent + (1|Date), data = courtship_for_analysis)
corsum_eff <- effect("Box", courtship_time_sum)
corsum_eff <- as.data.frame(corsum_eff)
corsum_eff$Behaviour <- "Courtship Duration"
summary(courtship_time_sum)
car::Anova(courtship_time_sum)

corprop_eff <- effect("Box", courtship_model1)
corprop_eff <- as.data.frame(corprop_eff)
corprop_eff$Behaviour <- "Proportion Time Courting"
summary(courtship_model1)
car::Anova(courtship_model1)
#make column of the names of group (will be the x axis)

corcount_eff<- effect("Box", courtship_model2)
corcount_eff <- as.data.frame(corcount_eff)
corcount_eff$Behaviour <- "Courtship Bouts"
car::Anova(courtship_model2)
#coplat_eff <- effect("Box", copul_model1)
#coplat_eff <- as.data.frame(coplat_eff)
#coplat_eff$Behaviour <- "Copulation Latency"


#copdur_eff <- effect("Box", copul_model2)
#copdur_eff <- as.data.frame(copdur_eff)
#copdur_eff$Behaviour <- "Copulation Duration"

copprop_eff <- effect("Box", copprop_mod)
copprop_eff <- as.data.frame(copprop_eff)
copprop_eff$Behaviour <- "Proportion Copulation"
car::Anova(copprop_mod)

coplat_0_eff <- effect("Box", copul_model12)
coplat_0_eff <- as.data.frame(coplat_0_eff)
coplat_0_eff$Behaviour <- "Copulation Latency"
car::Anova(copul_model12)


copdur_0_eff <- effect("Box", copul_model22)
copdur_0_eff <- as.data.frame(copdur_0_eff)
copdur_0_eff$Behaviour <- "Copulation Duration"
summary(copul_model22)
car::Anova(copul_model22)

copulation_all <- rbind(coplat_0_eff, copdur_0_eff)
head(copulation_all)
gg1 <- ggplot(copulation_all, aes(x=Behaviour, y=fit, fill=Box))
gg1 + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), size = 1.2, width = 0.2) + ylab("Time (seconds)")

proportion_all <- rbind(corprop_eff, copprop_eff)
head(proportion_all)
gg2 <- ggplot(proportion_all, aes(x=Behaviour, y=fit, fill=Box))
gg2 + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), size = 1.2, width = 0.2) + ylab("Proportion")

gg3 <- ggplot(corcount_eff, aes(x=Behaviour, y=fit, fill=Box))
gg3 + geom_bar(stat="identity", position= position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), size = 1.2, width = 0.2) + ylab("Count")

allallall <- rbind(corcount_eff, corprop_eff, copdur_0_eff, coplat_0_eff, copprop_eff)
ggAll <- ggplot(allallall, aes(x=Behaviour, y=fit, fill=Box))
ggAll + geom_bar(stat="identity", position= position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), size = 1.2, width = 0.2) + ylab("Count")



Times <- rbind(coplat_0_eff, copdur_0_eff, corsum_eff)
head(Times)
colnames(Times) <- c("Treatment", "fit", "se", "lower", "upper", "Behaviour")
ggtime <- ggplot(Times, aes(x=Behaviour, y=fit, fill=Treatment))
ggtime2 <- ggtime + geom_bar(stat="identity", 
                  position= position_dodge()) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), size = 1.2,
                width = 0.2) + 
  ylab("Time (sec)") + 
  xlab("") +
  theme(text = element_text(size=15), axis.text.x= element_text(size=15))

ggtime2 + scale_fill_manual(values=c("#999999", "#E69F00"))
#scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))