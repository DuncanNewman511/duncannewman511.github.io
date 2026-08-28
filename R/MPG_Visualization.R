library(tidyverse)
mpg

View(mpg)

summary(mpg)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))


ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=cty, color=class))


ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=cty, color=class))


ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy,
                            linetype = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(color = drv))


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))


demo <- tribble(~cut, ~freq,
                "Fair", 1610, "Good", 4906, "Very Good", 12082,
                "Premium", 13791, "Ideal", 21551 )
ggplot(data = demo) +
  geom_bar(mapping = aes(x=cut, y=freq), stat="identity")


ggplot(data = diamonds) +
  stat_summary( mapping = aes(x = cut, y = depth),
                fun.ymin = min, fun.ymax = max, fun.y = median )


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),
             color = "blue") + facet_grid(drv ~ cyl)


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))


ggplot(data = diamonds) + geom_bar(mapping =
                                     aes(x = cut, fill = clarity), position = "fill")


ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut,
                                                 fill = clarity), position = "dodge")


bar <- ggplot(data = diamonds) +
  geom_bar( mapping = aes(x = cut, fill = cut),
            show.legend = FALSE, width = 1 ) +
  theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)


## bar, and bar+coord_flip(), and bar+coord_polar()




