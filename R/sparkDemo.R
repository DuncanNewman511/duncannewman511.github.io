## https://www.datamechanics.co/blog-post/tutorial-run-your-r-sparklyr-workloads-at-scale-with-spark-on-kubernetes

library(sparklyr)

library(DBI)

library(dplyr)

library(nycflights13)

library(Lahman)

library(ggplot2)

library(microbenchmark)

spark_available_versions()

spark_install(version = "4.0") ## (installs hadoop et. al.)

sc <- spark_connect(master = "local", version = "4.0")

sparklyr.log.console = TRUE

sc <- spark_connect(master = "local", version = "4.0")

spark_web(sc)

flights_tbl_spark <- copy_to(sc, nycflights13::flights, "flights", overwrite = TRUE)
iris_tbl_spark <- copy_to(sc, iris, "iris", overwrite = TRUE)
batting_tbl_spark <- copy_to(sc, Lahman::Batting, "batting", overwrite = TRUE)

print(src_tbls(sc))

writeLines("=========================== flights table ======================")
print(nycflights13::flights)
#Data Engineering
#dplyr
writeLines("=========================== flights delay ======================")
delay_tbl_spark <- flights_tbl_spark %>%
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) 

delay_r = collect(delay_tbl_spark)
print(delay_r)

flight_preview_spark_tbl <- dbGetQuery(sc, "SELECT * FROM flights LIMIT 5")
flights_preview_r = collect(flight_preview_spark_tbl)
print(flights_preview_r)

writeLines("=========================== spark_apply ======================")
iris_tbl_spark2 <- spark_apply(iris_tbl_spark, function(data) {
  data[2:4] + rgamma(1,2)
})
iris_r = collect(iris_tbl_spark2)
print(iris_r)

writeLines("=========================== Plots ======================")

#Plots
delay <- flights_tbl_spark %>%
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect()


plot1 <- ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

plot1

spark_disconnect(sc)