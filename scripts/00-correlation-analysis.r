source("scripts/01-setup.r")
# 
# min_max_cor <- function(sobj) {
#   sigma <- sobj$Sigma
#   x <- sobj$X
#   y <- sobj$Y
#   m <- sobj$m
#   cor_df <- list(
#     population = list(
#       predictor = cov2cor(sigma[-c(1:m), -c(1:m)]),
#       response = cov2cor(sigma[1:m, 1:m])
#     ),
#     sample = list(
#       predictor = cor(x),
#       response = cor(y)
#     )
#   )
#   out <- modify_depth(cor_df, 2, function(x){
#     x_ <- abs(x[col(x) != row(x)])
#     data.frame(
#       min_max = c("min", "max"),
#       cor = c(min(x_), max(x_))
#     )
#   })
#   bind_rows(modify_depth(out, 1, bind_rows, .id = "Var1"), .id = "Var2")
# }
# 
# sim_obj <- design %>% 
#   transmute(sobj = pmap(., simrel)) %>% 
#   rownames_to_column(var = "Design") %>% 
#   left_join(design_chr %>% rownames_to_column(var = "Design"),
#             by = "Design") %>% 
#   select(Design, p, eta, gamma, sobj)
# 
# cor_df <- sim_obj %>% 
#   group_by(Design, p, eta, gamma) %>% 
#   transmute(cor = map(sobj, min_max_cor)) %>% 
#   unnest() %>% 
#   ungroup() %>% 
#   mutate(abs_cor = abs(cor))
# 
# plot_cor <- function(cor_df, type = "predictor") {
#   facet_form <- switch (type,
#     predictor = ". ~ gamma + p",
#     response = ". ~ eta"
#   )
#   plt <- cor_df %>% 
#     filter(Var1 == type) %>% 
#     ggplot(aes(
#       x = Design,
#       y = abs_cor,
#       color = min_max,
#       shape = Var2,
#       linetype = Var2
#     )) +
#     geom_point() +
#     geom_line(aes(group = interaction(Var2, min_max))) +
#     facet_grid(as.formula(facet_form), 
#                labeller = label_both,
#                scales = 'free', space = "free") +
#     theme(legend.title = element_blank()) +
#     labs(y = "Absolute Correlation") +
#     scale_color_discrete(label = tools::toTitleCase) +
#     scale_linetype_discrete(label = tools::toTitleCase) +
#     scale_shape_discrete(label = tools::toTitleCase) +
#     ggtitle(tools::toTitleCase(type))
#   return(plt)
# }
# 
# p1 <- plot_cor(cor_df, type = "predictor")
# p2 <- plot_cor(cor_df, type = "response")
# ggpubr::ggarrange(
#   p1, p2,
#   nrow = 2, ncol = 1,
#   common.legend = TRUE, 
#   legend = "bottom"
# )

min_max_cor <- function(sobj) {
  sigma <- sobj$Sigma
  x <- sobj$X
  y <- sobj$Y
  m <- sobj$m
  cor_df <- list(
    population = list(
      predictor = cov2cor(sigma[-c(1:m), -c(1:m)]),
      response = cov2cor(sigma[1:m, 1:m])
    ),
    sample = list(
      predictor = cor(x),
      response = cor(y)
    )
  )
  out <- modify_depth(cor_df, 2, function(x){
    x_ <- abs(x[col(x) != row(x)])
    data.frame(
      min_max = c("min", "max"),
      cor = c(min(x_), max(x_))
    )
  })
  bind_rows(modify_depth(out, 1, bind_rows, .id = "Var1"), .id = "Var2")
}
plot_cor <- function(cor_df, type = "predictor") {
  facet_form <- switch (type,
                        predictor = ". ~ gamma + p",
                        response = ". ~ eta"
  )
  plt <- cor_df %>% 
    filter(Var1 == type) %>% 
    ggplot(aes(
      x = Design,
      y = abs_cor,
      color = min_max,
      shape = Var2,
      linetype = Var2
    )) +
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.y = mean, geom = "line",
                 aes(group = interaction(Var2, min_max))) +
    facet_grid(as.formula(facet_form), 
               labeller = label_both,
               scales = 'free', space = "free") +
    theme(legend.title = element_blank()) +
    labs(y = "Absolute Correlation") +
    scale_color_discrete(label = tools::toTitleCase) +
    scale_linetype_discrete(label = tools::toTitleCase) +
    scale_shape_discrete(label = tools::toTitleCase) +
    ggtitle(tools::toTitleCase(type))
  return(plt)
}

if (file.exists("scripts/robj/cor_df.Rdata")) {
  load("scripts/robj/cor_df.Rdata")
} else {
  fpath <- "scripts/robj/sim_obj/"
  fname <- dir(fpath, pattern = "dgn.+")
  full_path <- `names<-`(paste0(fpath, fname), gsub("\\.Rdata", "", fname))
  cor_df <- map_df(full_path, function(f){
    local({
      load(f)
      cor_df <- map_df(out, min_max_cor, .id = "Replication")
      assign("cor_df", cor_df, envir = parent.env(environment()))
    })
    return(cor_df)
  }, .id = "File")
  
  save(cor_df, file = "scripts/robj/cor_df.Rdata")
}

corr <- as_tibble(cor_df) %>% 
  separate(File, c("Dummy", "Design", "Method"), "-") %>%
  select(-Dummy) %>% 
  left_join(
    design_chr %>% rownames_to_column(var = "Design") %>% 
      select(Design, gamma, eta, p),
    by = "Design"
  ) %>% 
  mutate(abs_cor = abs(cor))

p1 <- plot_cor(corr, type = "predictor")
p2 <- plot_cor(corr, type = "response")
plt <- ggpubr::ggarrange(
  p1, p2,
  nrow = 2, ncol = 1,
  common.legend = TRUE, 
  legend = "bottom"
)
ggsave(plt, filename = "scripts/plots/corr-plot.pdf", 
       width = 15, height = 10, scale = 0.6)
