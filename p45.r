# Install ggplot2 if not already installed
install.packages("ggplot2")

# Load the ggplot2 library
library(ggplot2)

# Load the dataset
data("midwest", package = "ggplot2")

# View dataset
View(midwest)

# 1. Scatter Plot
ggplot(data = midwest) +
  geom_point(aes(x = percollege, y = percadultpoverty))

# 2. Scatter Plot with Smooth Line
ggplot(data = midwest) +
  geom_point(aes(x = percollege, y = percadultpoverty)) +
  geom_smooth(aes(x = percollege, y = percadultpoverty))

# 3. Bar Chart (Total population by state)
ggplot(data = midwest) +
  geom_col(aes(x = state, y = poptotal))

# 4. Hexagonal Heatmap
ggplot(data = midwest) +
  geom_hex(aes(x = percollege, y = percadultpoverty))

# 5. Shared Aesthetic Mappings
ggplot(data = midwest, aes(x = percollege, y = percadultpoverty)) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = percchildbelowpovert))

# 6. Color Mapping by State
ggplot(data = midwest) +
  geom_point(aes(x = percollege, y = percadultpoverty, color = state))

# 7. Custom Color and Transparency
ggplot(data = midwest) +
  geom_point(aes(x = percollege, y = percadultpoverty),
             color = "red",
             alpha = 0.3)
