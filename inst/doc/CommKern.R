## ----label = "format-setup", include = FALSE----------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----label = "setup"----------------------------------------------------------
library(CommKern)

## ---- fig.width = 7, fig.height = 3.5, fig.show='hold'------------------------
matrix_plot(SBM_net)

## -----------------------------------------------------------------------------
net <- matrix_to_df(func_mat = SBM_net$func_mat, str_mat = SBM_net$str_mat)
identical(net, SBM_net)

## -----------------------------------------------------------------------------
str(SBM_net)

## -----------------------------------------------------------------------------
str(simasd_covars)

## -----------------------------------------------------------------------------
str(simasd_comm_df, max.level = 0)

## -----------------------------------------------------------------------------
str(simasd_hamil_df)

## -----------------------------------------------------------------------------
str(simasd_array)

## -----------------------------------------------------------------------------
net <- matrix_to_df(func_mat = SBM_net$func_mat, str_mat = SBM_net$str_mat)
identical(net, SBM_net)

## -----------------------------------------------------------------------------
str(hms)

## -----------------------------------------------------------------------------
hms_object <-
  hms(
      input_net = SBM_net,
      spins = 2,
      alpha = 0,
      coolfact = 0.99,
      false_pos = 0.01,
      max_layers = 1)
str(hms_object)

## ---- fig.show='hold'---------------------------------------------------------
community_plot(hms_object)

## ---- fig.show='hold'---------------------------------------------------------
set.seed(7183)

x <- sample(x = rep(1:3, 4), 12)
y <- sample(x = rep(1:3, 4), 12)
z <- sample(x = rep(1:3, 4), 12)

xyz_comms <- data.frame(id=seq(1:length(x)),x_comm=x,y_comm=y,z_comm=z)
xyz_alleg <- community_allegiance(xyz_comms)
xyz_melt <- reshape2::melt(xyz_alleg)

print(xyz_comms)

ggplot2::ggplot(data = xyz_melt) +
  ggplot2::theme_minimal() +
  ggplot2::aes(x = as.factor(Var1), y = as.factor(Var2), fill = value) +
  ggplot2::geom_tile() +
  ggplot2::xlab('Node') + ggplot2::ylab('Node') +
  ggplot2::ggtitle('Community Allegiance Example') +
  ggplot2::scale_fill_gradient2(
    low  = 'navy',
    high = 'goldenrod1',
    mid  = 'darkturquoise',
    midpoint = 0.5,
    limit = c(0, 1),
    space = 'Lab',
    name='')

## -----------------------------------------------------------------------------
set.seed(7183)

x <- sample(x = rep(1:3, 4), 12)
y <- sample(x = rep(1:3, 4), 12)
z <- sample(x = rep(1:3, 4), 12)

xyz_comms_mat <- matrix(c(x,y,z),nrow=length(x),ncol=3)
consensus_similarity(xyz_comms_mat)

## -----------------------------------------------------------------------------
str(group_network_perturb)

str(group_adj_perturb)

## ---- fig.show='hold'---------------------------------------------------------
sim_nofuzzy <-
   group_network_perturb(
     n_nodes = 50,
     n_comm = 4,
     n_nets = 3,
     perturb_prop = 0.1,
     wcr = c(8, 8),
     bcr = c(1.5, 8)
   )

nofuzzy_adj <-
  group_adj_perturb(sim_nofuzzy, n_nets = 3, n_nodes = 50)

if (require(pheatmap)) {
 pheatmap::pheatmap(
   nofuzzy_adj[1,,],
   treeheight_row = FALSE,
   treeheight_col = FALSE
 )
}

## ---- fig.show='hold'---------------------------------------------------------
sim_fuzzy <-
   group_network_perturb(
     n_nodes = 50,
     n_comm = 4,
     n_nets = 3,
     perturb_prop = 0.1,
     wcr = c(8, 8),
     bcr = c(1.5, 8),
     bfcr = c(3.5, 8),
     fuzzy_comms = c('comm_b', 'comm_c')
   )

fuzzy_adj <-
  group_adj_perturb(sim_fuzzy, n_nets = 3, n_nodes = 50)

if (require(pheatmap)) {
 pheatmap::pheatmap(
   fuzzy_adj[1,,],
   treeheight_row = FALSE,
   treeheight_col = FALSE
 )
}

## -----------------------------------------------------------------------------
set.seed(7183)

x <- sample(x = rep(1:3, 4), 12)
y <- sample(x = rep(1:3, 4), 12)

purity(x,y)

## -----------------------------------------------------------------------------
set.seed(7183)

x <- sample(x = rep(1:3, 4), 12)
y <- sample(x = rep(1:3, 4), 12)

NMI(x,y)

## -----------------------------------------------------------------------------
set.seed(7183)

x <- sample(x = rep(1:3, 4), 12)
y <- sample(x = rep(1:3, 4), 12)

adj_RI(x,y)

## -----------------------------------------------------------------------------
x <- c(2,2,3,1,3,1,3,3,2,2,1,1)
y <- c(3,3,2,1,1,1,1,2,2,3,2,3)
z <- c(1,1,2,3,2,3,2,1,1,2,3,3)

xyz_comms <- data.frame(x_comm = x, y_comm = y, z_comm = z)

ext_distance(xyz_comms, variant = 'NMI')
ext_distance(xyz_comms, variant = 'adj_RI')
ext_distance(xyz_comms, variant = 'purity')

## -----------------------------------------------------------------------------
hamil_df <- data.frame(id  = seq(1:8),
                       ham = c(-160.5375, -167.8426, -121.7128,
                               -155.7245, -113.9834, -112.5262,
                               -117.9724, -171.374))

ham_distance(hamil_df)

## -----------------------------------------------------------------------------
str(score_log_nonparam)

## -----------------------------------------------------------------------------
simasd_ham_mat <- ham_distance(simasd_hamil_df)

score_log_nonparam(outcome=simasd_covars$dx_group,
                   dist_mat=simasd_ham_mat)

## -----------------------------------------------------------------------------
str(score_log_semiparam)

## -----------------------------------------------------------------------------
simasd_ham_mat <- ham_distance(simasd_hamil_df)
simasd_confound <- simasd_covars[,3:5]
simasd_confound$handedness <- as.factor(simasd_confound$handedness)

score_log_semiparam(outcome=simasd_covars$dx_group,
                    covars=simasd_confound,
                    dist_mat=simasd_ham_mat)

## -----------------------------------------------------------------------------
str(score_cont_nonparam)

## -----------------------------------------------------------------------------
simasd_NMI_mat <- ext_distance(comm_df=simasd_comm_df,
                               variant=c("NMI"))

score_cont_nonparam(outcome=simasd_covars$verbal_IQ,
                   dist_mat=simasd_NMI_mat)

## -----------------------------------------------------------------------------
str(score_cont_semiparam)

## -----------------------------------------------------------------------------
simasd_pur_mat <- ext_distance(comm_df=simasd_comm_df,
                               variant=c("purity"))
simasd_confound <- simasd_covars[,3:5]
simasd_confound$handedness <- as.factor(simasd_confound$handedness)

score_cont_semiparam(outcome=simasd_covars$verbal_IQ,
                    covars=simasd_confound,
                    dist_mat=simasd_pur_mat)

