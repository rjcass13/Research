data = read.csv("/Users/zhanwentao/Documents/Abhi/Conference/IBC2024/short-course/geospatial_stats_ML_short_course_2024/data/Output/tensor.csv")

library(BRISC)
library(ggplot2)
X = as.matrix(data[,1, drop = F])
Y = as.matrix(data[,2])
coords = as.matrix(data[,3:4])

estimation_result = BRISC_estimation(coords, Y, cbind(1, X))
beta = estimation_result$Beta

colnames(X) = NULL
df_plt = do.call(rbind, list(
  data.frame(X = X, Y = Y, label = "data"),
  data.frame(X = X, Y = 10*sin(2*pi*X), label = "f(x)"),
  data.frame(X = X, Y = beta[1] + beta[2]*X, label = "BRISC")
))

ggplot(df_plt, aes(x = X, y = Y, color = label)) +
  geom_point(size = 0.5) +
  theme_bw()
  
