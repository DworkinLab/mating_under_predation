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



corprop_eff <- effect("Box", courtship_model1)
corprop_eff <- as.data.frame(corprop_eff)
corprop_eff$Behaviour <- "Proportion Time Courting"
#make column of the names of group (will be the x axis)

corcount_eff<- effect("Box", courtship_model2)
corcount_eff <- as.data.frame(corcount_eff)
corcount_eff$Behaviour <- "Courtship Bouts"

#coplat_eff <- effect("Box", copul_model1)
#coplat_eff <- as.data.frame(coplat_eff)
#coplat_eff$Behaviour <- "Copulation Latency"


#copdur_eff <- effect("Box", copul_model2)
#copdur_eff <- as.data.frame(copdur_eff)
#copdur_eff$Behaviour <- "Copulation Duration"

copprop_eff <- effect("Box", copprop_mod)
copprop_eff <- as.data.frame(copprop_eff)
copprop_eff$Behaviour <- "Proportion of pairs copulating"

coplat_0_eff <- effect("Box", copul_model12)
coplat_0_eff <- as.data.frame(coplat_0_eff)
coplat_0_eff$Behaviour <- "Copulation Latency"

copdur_0_eff <- effect("Box", copul_model22)
copdur_0_eff <- as.data.frame(copdur_0_eff)
copdur_0_eff$Behaviour <- "Copulation Duration"

copulation_all <- rbind(coplat_0_eff, copdur_0_eff)
head(copulation_all)
gg1 <- ggplot(copulation_all, aes(x=Behaviour, y=fit, fill=Box))
gg1 + geom_bar() +
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.2, width = 0.2)
