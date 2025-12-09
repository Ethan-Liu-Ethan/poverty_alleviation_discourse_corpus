### Linear Regression Line
# Load required packages
library(ggplot2)

# Read data from CSV file
df <- read.csv("data.csv")
df

# Calculate linear regression model
model <- lm(Number_of_Reports ~ Year, data = df)

# Extract model coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Calculate R-squared
r_squared <- summary(model)$r.squared

# Create strings for regression formula and R²
regression_formula <- sprintf("y == %.2f * x - %.2f", slope, -intercept)
r_squared_str <- sprintf("R^2 == %.3f", r_squared)

# Create plot
p <- ggplot(df, aes(x=Year, y=Number_of_Reports)) +
  geom_point(color = "blue", size = 3) +  # Create scatter plot
  geom_smooth(method=lm, se=FALSE, color="red", linetype = 1) +  # Add linear regression line
  labs(x="Year", y="Number of Reports",
       title="Trend of Number of Reports over the Years") +  # Add labels
  scale_x_continuous(breaks = df$Year, labels = as.integer(df$Year)) +  # Customize X-axis display
  theme_bw(base_size = 16) + # Apply black and white theme
  theme(plot.title = element_text(hjust = 0.5)) # Center title

# Calculate appropriate year and corresponding number of reports for displaying formula and R²
year_for_label <- mean(df$Year)
reports_for_label <- year_for_label * slope + intercept

# Add linear regression formula and R² to the plot
p +
  annotate("text", x = year_for_label, y = reports_for_label + 8, label = regression_formula, parse = TRUE, hjust = 0.5) +
  annotate("text", x = year_for_label, y = reports_for_label + 6, label = r_squared_str, parse = TRUE, hjust = 0.5)










