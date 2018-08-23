library(tidyverse)
library(readxl)
library(janitor)
library(fs)
theme_set(theme_minimal())

files <- dir_ls("ELA", regexp = "ELA/g")
d <- map_df(files, read_excel, .id = "grade") %>% 
  mutate(grade = parse_number(grade)) %>% 
  clean_names()%>% 
  mutate(free_est = difficulty + displacement)

outside_range <- function(g3ELA) {
  g3ELA %>% 
    filter(difficulty < lwr | 
             difficulty > upr) %>% 
    select(item_id, difficulty, free_est)
}

nested <- d %>% 
  nest(-grade)

# new cols to create: data2, plot
estimates <- nested %>% 
  mutate(model = map(data, ~lm(difficulty ~ free_est, .)),
         data2 = map2(data, model, 
                      ~cbind(.x, 
                             predict(.y, 
                                     interval = "prediction"))),
         plot = map2(data2, grade, ~
           ggplot(.x, aes(free_est, difficulty)) +
             geom_point(color = "gray40") +
             geom_ribbon(aes(ymin = lwr, ymax = upr),
                         fill = "cornflowerblue", 
                         alpha = 0.2) +
              geom_line(aes(y = fit), 
                        color = "magenta", 
                        lwd = 1.2) +
               labs(title = paste("Grade", .y,
                                  "Item-Parameter Drift"))),
         remove = map(data2, outside_range))       

estimates %>% 
  select(grade, remove) %>% 
  unnest()%>%
 write_csv ("ela_drifted_items.csv")

dir_create("plots")
plot_titles <- paste0("plots/g", estimates$grade, "ELAipdrift.pdf")
walk2(plot_titles, estimates$plot,
     ~ggsave(.x, .y, width = 6.5, height = 8))
