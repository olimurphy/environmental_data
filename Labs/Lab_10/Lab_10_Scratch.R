#Lab 10: ANOVA by hand :p

#Building an ANOVA by hand



rm(list = ls())

require(here)
rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)
                        
n_obs = nrow(rope)
n_groups = length(unique(rope$rope.type))
                        
ss_tot = sum((mean(rope$p.cut) - c(rope$p.cut))^2)
df_tot = (n_obs - 1)
                        
agg_sq_resids =
 aggregate(
 x = rope$p.cut,
 by = list(rope$rope.type), 
 FUN = function(x) sum((mean(x) - c(x))^2))
                    
ss_within = sum(agg_sq_resids$x)
df_within = (n_obs - n_groups)
                        
ss_among = (ss_tot - ss_within)
df_among = (n_groups - 1)
                        
ms_within = ss_within / (n_obs - n_groups)
ms_among  = ss_among / (n_groups - 1)
                        
f_ratio = ms_among / ms_within
f_pval = pf(f_ratio, df_among, df_within, lower.tail = FALSE)

#ok that sucked luckily there's a function <3
fit_1 = lm(p.cut ~ rope.type, data = rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"
