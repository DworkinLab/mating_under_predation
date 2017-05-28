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
#make column of the names of group (will be the x axis)

corcount_eff<- effect("Box", courtship_model2)
corcount_eff <- as.data.frame(corcount_eff)

coplat_eff <- effect("Box", copul_model1)
coplat_eff <- as.data.frame(coplat_eff)

copdur_eff <- effect("Box", copul_model2)
copdur_eff <- as.data.frame(copdur_eff)

coplat_0_eff <- effect("Box", copul_model12)
coplat_0_eff <- as.data.frame(coplat_0_eff)

copdur_0_eff <- effect("Box", copul_model22)
copdur_0_eff <- as.data.frame(copdur_0_eff)

copprop_eff <- effect("Box", copprop_mod)
copprop_eff <- as.data.frame(copprop_eff)

