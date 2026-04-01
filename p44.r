# Install and load ggplot2
install.packages("ggplot2")
library(ggplot2)

# Sample dataset
data <- data.frame(
  a = c(1,2,3,4,5),
  b = c(2,4,6,8,10)
)

# Scatter Plot
ggplot(data) +
  geom_point(aes(x = a, y = b)) +
  labs(title = "Scatter Plot", x = "a", y = "b")

# Line Plot
ggplot(data) +
  geom_line(aes(x = a, y = b), color = "red", size = 1.5) +
  labs(title = "Line Plot")

# Bar Plot
ggplot(data) +
  geom_bar(aes(x = factor(a), y = b), stat = "identity", fill = "blue") +
  labs(title = "Bar Plot")

# Scatter Plot with Smooth Line
ggplot(data) +
  geom_point(aes(x = a, y = b), color = "green", size = 3) +
  geom_smooth(aes(x = a, y = b), method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Smooth Line")
