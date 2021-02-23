

# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# question 1 --------------------------------------------------------------

question1_table <- rename (iris, sepal_length = Sepal.Length, sepal_width = Sepal.Width, petal_length = Petal.Length, petal_width = Petal.Width, species = Species)

question1_table

# question 2 --------------------------------------------------------------

question2_table <- mutate(question1_table,sepal_length_cm = sepal_length/10, sepal_width_cm = sepal_width/10, petal_length_cm = petal_length/10, petal_width_cm = petal_width/10 ) 

question2_table


# question 3 --------------------------------------------------------------

t3 <- mutate(question2_table, sepal_area = sepal_length * sepal_width, petal_area = petal_length * petal_width )

question3_table <- select(t3, sepal_area, petal_area, species)

question3_table


# question 4 --------------------------------------------------------------

t4 <- select(question1_table, sepal_length)

question4_table <-summarize( t4,sampl_size = n(),
    max_value = max(sepal_length),
    min_value = min(sepal_length), 
    range = max_value - min_value,
    median = median(sepal_length),
    q1 = quantile(sepal_length, probs = 0.25),
    q2 = quantile(sepal_length, probs = 0.75),
    IQR =  IQR(sepal_length)
  )

question4_table


# question 5 --------------------------------------------------------------

iris_grouped <- question1_table %>% group_by(species)%>%
  summarize(
    sample_size = n(),
    mean = mean(petal_width),
    standar_deviation = sd(petal_width),
    variance = var(petal_width), 
    sem = standar_deviation/sqrt(sample_size),
    ci_upper_limit = mean + 2 * sem,
    ci_lower_limit = mean - 2 * sem
  )

iris_grouped


# question 6 --------------------------------------------------------------

tab_6 <- select(question1_table, species , petal_width)

tab_6

ggplot(data = question1_table) +
  geom_jitter(mapping = aes(x = species, y = petal_width))


# question 7 --------------------------------------------------------------

ggplot(data =question1_table) +
  geom_jitter(mapping = aes(x = species, y = petal_width)) +
  geom_crossbar(
    data = iris_grouped, 
    mapping = aes(x = species, y = mean, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red"
  )


# question 8 --------------------------------------------------------------

ggplot(data = question1_table ) +
  geom_point(mapping = aes(x = petal_length, y =petal_width , color = species))
          























