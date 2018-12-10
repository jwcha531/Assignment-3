#function pour sampling 
s_df <- function(diff_mean, t_good, t_bad, sd_good, sd_bad){
  # sampling des moyennes de Bad et Good 
  m_bad <- sample(0:100, 1, replace=TRUE)
  m_good <- m_bad + diff_mean
  # créer l'aléatoire pour les groupes bad et good
  g_bad <- as.integer(rnorm(t_bad, mean = m_bad, sd = sd_bad)) 
  g_good <- as.integer(rnorm(t_good, mean = m_good, sd = sd_good))
  
  n_df <- tibble::tibble(language = c(rep("adger-good", t_good), rep("adger-bad", t_bad)),
                         rating = c(g_good, g_bad))
  return (n_df)  
}






#function pour coeffiecient d'hypothese
coef_s <- function(diff_mean, t_good, t_bad, sd_good, sd_bad, sampling, N_SAMPLES=99){
  #créer le vector de coefficient pour hypothèse
  coef_h <- rep(0, N_SAMPLES)
  for (i in 1:N_SAMPLES){
    dataset <- sampling(diff_mean, t_good, t_bad, sd_good, sd_bad)
    h <- lm(rating ~ language, data = dataset)
    coef_lang <- coef(h)[2]
    coef_h[i] <- coef_lang
  }
  return (tibble::tibble(coef = coef_h))
}






#function pour le couper
s_df_cut <- function(diff_mean, t_good, t_bad, sd_good, sd_bad){
  m_bad <- sample(0:100, 1, replace=TRUE)
  m_good <- m_bad + diff_mean
  
  g_bad <- as.integer(rnorm(t_bad, mean = m_bad, sd = sd_bad))
  g_bad <- replace(g_bad, g_bad < 0, 0)
  g_bad <- replace(g_bad, g_bad > 100, 100)
  
  g_good <- as.integer(rnorm(t_good, mean = m_good, sd = sd_good))
  g_good <- replace(g_good, g_good < 0, 0)
  g_good <- replace(g_good, g_good > 100, 100)
  
  new_df <- tibble::tibble(language = c(rep("adger-good", t_good), rep("adger-bad", t_bad)),
                           rating = c(g_good, g_bad))
  return (new_df)
}