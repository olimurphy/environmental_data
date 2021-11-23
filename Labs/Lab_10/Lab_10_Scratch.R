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
                        df_within = 
                          aggregate(
                            x = rope$rope.type,
                            by = list(rope$rope.type),
                            FUN = function(x) (length(x) - n_groups)
                          )
                        
                        ss_among = ss_tot - ss_within
                        df_among = 
                        
                        ms_within = ss_within / (n_obs - n_groups)
                        ms_among  = ss_among / (n_groups - 1)
                        
                        f_ratio = ms_among / ms_within
                        f_pval = ....