# ---
# code:  Compute ATT, Abadie Imbens SE, and p values (longitudinal data and fake the panel)
# author: Louise Glew, louise.glew@gmail.com; 
# created: February 2018
# modified: 

#---
#--- inputs:  
# outcomes:  matrix produced by outcome_ATT_method1 OR outcome_ATT_method2 functions 
# weights: optional weight matrix (if weighted treatment effects being computed)
# grouping_var: optional group_by variable(s) -- if more than one grouping variable, identify them in concatenated strings 
#               e.g., grouping_var = c("MPAID","GenderHHH")
# conf:  confidence intervals for standardized mean difference (default = 0.95)
# sig.test: logical test for whether to consider significance of treatment effect when assigning impact typology (default = FALSE)
# p.value:  p value to use if sig.test = TRUE (default = 0.05)


att.significance.by.group <- function(outcomes, HHData, weights = NULL, grouping_var = NULL, conf = NULL, sig.test = NULL, p.value = NULL) {
  
  
# Dealing with optional weight matrix and conf level (defining a default if they were not defined in function call)
  pacman::p_load(Matching,MBESS,plyr,dplyr)
  
  weight.matrix <- 
    data.frame(w=ifelse(is.null(weights) == T,
                        rep(1, length(outcomes$MPA.outcome)),
                        weights))
  
  group <-
    ifelse(is.null(grouping_var) == T,
           NA,
           as.character(grouping_var))
  
  conf.level <-
    ifelse(is.null(conf) == T,
           0.95,
           conf)
  
  p.value <- 
    ifelse(is.null(p.value) == T,
           0.05,
           p.value)

# Developing initial data frames 

  est.data <- 
    outcomes %>%
    dplyr::rename(HouseholdID = tr1tx) %>%
    left_join(HHData, by = "HouseholdID") %>%
    transmute(X=seq(1:length(HouseholdID)),
              MPA.outcome=MPA.outcome,
              Control.outcome=Control.outcome,
              MPAID=MPAID) %>%
    reshape2::melt(.,id.vars=c("X","MPAID"),variable.name="Tr",value.name="Y") %>%
    mutate(Tr=ifelse(Tr=="MPA.outcome",1,0))
  
  match.out <- data.frame(MPAID=NA, 
                          est=NA,
                          se=NA,
                          nobs=NA)
  
  for(i in unique(est.data[,"MPAID"])) {
    est.data.subset <- filter(est.data, MPAID==i)
    match.store <- Match(X=est.data.subset$X,
                         Tr=est.data.subset$Tr,
                         Y=est.data.subset$Y,
                         exact=TRUE)
    match.out[i,"MPAID"] <- i
    match.out[i,"est"] <- match.store$est
    match.out[i,"se"] <- match.store$se
    match.out[i,"nobs"] <- match.store$nobs
  }
  
  
  x <- 
    outcomes %>%
    dplyr::rename(HouseholdID = tr1tx) %>%
    left_join(HHData, by = "HouseholdID") %>%
    left_join(match.out, by = "MPAID") %>%
    cbind.data.frame(weight.matrix) %>%
    dplyr::group_by(get(group))
  

# Calculating smd, ci, impact type, p.val & consolidating with est & se
  
  initial.results <- 
    summarise(x,
              mpa.mean = mean(MPA.outcome),
              mpa.sd = sd(MPA.outcome),
              control.mean = mean(Control.outcome),
              control.sd = sd(Control.outcome),
              est = mean(est),   #ATT calculated through Match function
              se = mean(se),   #Abadie Imbens standard error calculated through Match function
              p.val = (1 - pnorm(abs(est/se))) * 2,
              n.1 = length(MPA.outcome),
              n.2 = length(Control.outcome),
              lower.ci = ci.smd(n.1 = n.1,
                                n.2 = n.2,
                                conf.level = conf.level,
                                smd = smd(Group.1 = MPA.outcome,
                                          Group.2 = Control.outcome))$Lower.Conf.Limit.smd,
              smd = ci.smd(n.1 = n.1,
                           n.2 = n.2,
                           conf.level = conf.level,
                           smd = smd(Group.1 = MPA.outcome,
                                     Group.2 = Control.outcome))$smd,
              upper.ci = ci.smd(n.1 = n.1,
                                n.2 = n.2,
                                conf.level = conf.level,
                                smd = smd(Group.1 = MPA.outcome,
                                         Group.2 = Control.outcome))$Upper.Conf.Limit.smd,
              u3 = round((1 - pnorm(1, ((smd * 1) + 1), 1)), 3),
              impact.type = ifelse(p.val >= p.value, 
                                   "mirror",
                                   ifelse(is.null(sig.test) == F, 
                                          ifelse((est > 0 & 
                                                    mean(MPA.outcome) > 0 & 
                                                    mean(Control.outcome) < 0), 
                                                 "catalyze", 
                                                 ifelse((est > 0 & 
                                                           mean(MPA.outcome) > 0 & 
                                                           mean(Control.outcome) > 0), 
                                                        "magnify",
                                                        ifelse((est > 0 & 
                                                                  mean(MPA.outcome) < 0 & 
                                                                  mean(Control.outcome) < 0), 
                                                               "buffer", 
                                                               ifelse((est < 0 & 
                                                                         mean(MPA.outcome) < 0 & 
                                                                         mean(Control.outcome) > 0), 
                                                                      "reverse", 
                                                                      ifelse((est < 0 & 
                                                                                mean(MPA.outcome) > 0 & 
                                                                                mean(Control.outcome) > 0), 
                                                                             "constrain",
                                                                             "exacerbate"))))),
                                          ifelse((est > 0 & 
                                                    mean(MPA.outcome) > 0 & 
                                                    mean(Control.outcome) < 0), "catalyze", 
                                                 ifelse((est > 0 & 
                                                           mean(MPA.outcome) > 0 & 
                                                           mean(Control.outcome) > 0), "magnify",
                                                        ifelse((est > 0 & 
                                                                  mean(MPA.outcome) < 0 & 
                                                                  mean(Control.outcome) < 0), "buffer", 
                                                               ifelse(est == 0, "mirror", 
                                                                      ifelse((est < 0 & 
                                                                                mean(MPA.outcome) < 0 & 
                                                                                mean(Control.outcome) > 0), "reverse", 
                                                                             ifelse((est < 0 & 
                                                                                       mean(MPA.outcome) > 0 & 
                                                                                       mean(Control.outcome) > 0), "constrain", 
                                                                                    "exacerbate"))))))))
    )
  
  
# Subsetting to final output data frame
  
  final.result <- 
    initial.results[,-which(names(initial.results) %in% c("varest","n.1","n.2"))]

  
  return(final.result)
}


