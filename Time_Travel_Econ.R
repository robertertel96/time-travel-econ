install.packages("tidyverse")
install.packages("tibble")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("readxl")
library(tidyverse)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(readxl)

#Data Analysis
DATA <- read_excel("Desktop/Time-Travelling-Econ-Dataset.xlsx", col_types = c("text", "text", "text", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric"), na = "..")
# Reshape the data to long format
df_long <- DATA %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"), # Select columns that start with year digits
    names_to = "Year",       # Name of the new column to store the year values
    values_to = "Value"      # Name of the new column to store the values
  )
df_long$Year <- as.numeric(df_long$Year)




#Calculate MVA per capita
# Step 1: Extract Manufacturing, value added (constant 2015 US$) and Population, total
mva_data <- df_long %>%
  filter(Variable == "Manufacturing, value added (constant 2015 US$)") %>%
  select(`Country Name+A1:AK40`, Year, `Country Code`, Value)
  

population_data <- df_long %>%
  filter(Variable == "Population, total") %>%
  select(`Country Name+A1:AK40`,`Country Code`, Year, Value)
  

# Step 2: Merge MVA (USD) and Population data
merged_data <- merge(mva_data, population_data, by = c("Country Name+A1:AK40", "Year", "Country Code"))

# Step 3: Calculate MVA per capita
merged_data <- merged_data %>%
  mutate(MVA_per_capita = merged_data$Value.x / merged_data$Value.y)

# Step 4: Add MVA per capita to df_long
df_long <- df_long %>%
  filter(Variable != "MVA per capita") %>%
  bind_rows(merged_data %>%
              select(`Country Name+A1:AK40`, Year, `Country Code`, MVA_per_capita) %>%
              mutate(Variable = "MVA per capita", Value = MVA_per_capita) %>%
              select(-MVA_per_capita))



calculate_mva_per_capita_index <- function(df_long, base_year = 2005) {
  # Calculate the index based on the base year in this case 2005
  index_data <- merged_data %>%
    group_by(`Country Name+A1:AK40`, `Country Code`) %>%
    mutate(Base_Value = MVA_per_capita[Year == base_year]) %>%
    mutate(MVA_per_capita_index = (MVA_per_capita / Base_Value) * 100) %>%
    ungroup() %>%
    select(`Country Name+A1:AK40`, Year, `Country Code`, MVA_per_capita_index)
  
  # Combine the index data with the original dataframe
  df_long <- df_long %>%
    filter(Variable != "MVA per capita index") %>%
    bind_rows(index_data %>%
                mutate(Variable = "MVA per capita index", Value = MVA_per_capita_index) %>%
                select(`Country Name+A1:AK40`, Year, `Country Code`, Variable, Value))
  
  return(df_long)
}

df_long <- calculate_mva_per_capita_index(df_long, base_year = 2005)

# Function to calculate recent 5 years average and rank top 10 and bottom 10
rank_top_bottom <- function(data, indicator) {
  recent_years <- sort(unique(data$Year), decreasing = TRUE)[1:5]
  
  data %>%
    filter(Variable == indicator, Year %in% recent_years) %>%
    group_by(`Country Name+A1:AK40`) %>%
    summarise(Avg_Value = mean(Value, na.rm = TRUE)) %>%
    filter(!is.na(Avg_Value)) %>%
    arrange(desc(Avg_Value)) %>%
    mutate(Rank = row_number()) %>%
    filter(Rank <= 10 | Rank > (n() - 10))
}

# Apply the function to each indicator and print results
indicators <- unique(df_long$Variable)
for (indicator in indicators) {
  cat("Ranking for:", indicator, "\n")
  print(rank_top_bottom(df_long, indicator))
}


# Function to calculate recent 10 years average and rank top 10 and bottom 10
rank_top_bottom_10years <- function(data, indicator) {
  recent_years <- sort(unique(data$Year), decreasing = TRUE)[1:10]
  
  data %>%
    filter(Variable == indicator, Year %in% recent_years) %>%
    group_by(`Country Name+A1:AK40`) %>%
    summarise(Avg_Value = mean(Value, na.rm = TRUE)) %>%
    filter(!is.na(Avg_Value)) %>%
    arrange(desc(Avg_Value)) %>%
    mutate(Rank = row_number()) %>%
    filter(Rank <= 10 | Rank > (n() - 10))
}

# Specified indicators
specified_indicators <- c("Electricity production (kWh)", 
                          "School enrollment, secondary (% net)", 
                          "Electric power consumption (kWh per capita)")

# Apply the function to each specified indicator and print results
for (indicator in specified_indicators) {
  cat("Ranking for:", indicator, "\n")
  print(rank_top_bottom_10years(df_long, indicator))
}


# Function to calculate recent 10 years average
calculate_recent_avg <- function(data, variable_name, recent_years = 10) {
  recent_years_range <- sort(unique(data$Year), decreasing = TRUE)[1:recent_years]
  
  data %>%
    filter(Variable == variable_name, Year %in% recent_years_range) %>%
    group_by(`Country Name+A1:AK40`, `Country Code`) %>%
    summarise(Avg_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
}

# Calculate averages for required variables
electricity_consumption <- calculate_recent_avg(df_long, "Electric power consumption (kWh per capita)")
fertility_rate <- calculate_recent_avg(df_long, "Fertility rate, total (births per woman)")
bank_deposits <- calculate_recent_avg(df_long, "Bank deposits to GDP (%)")
education <- calculate_recent_avg(df_long, "School enrollment, secondary (% gross)")
mva_gdp <- calculate_recent_avg(df_long, "Manufacturing, value added (% of GDP)")
mva_per_capita <- calculate_recent_avg(df_long, "MVA per capita")
mva_per_capita_index <- calculate_recent_avg(df_long, "MVA per capita index")

# Merge data for the scatter plots
scatter_data1 <- merge(electricity_consumption, mva_gdp, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c("_electricity", "_mva_gdp"))
scatter_data2 <- merge(electricity_consumption, mva_per_capita, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c("_electricity", "_mva_per_capita"))
scatter_data3 <- merge(electricity_consumption, mva_per_capita_index, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c("_electricity", "_mva_per_capita_index"))
scatter_data4 <- merge(fertility_rate, mva_per_capita_index, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c("_fertility", "_mva_per_capita_index"))
scatter_data5 <- merge(bank_deposits, mva_per_capita_index, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c("_bank_deposits", "_mva_per_capita_index"))
scatter_data6 <- merge(education, mva_per_capita_index, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c("_education", "_mva_per_capita_index"))
# Clean data by removing rows with missing values
scatter_data1_clean <- na.omit(scatter_data1[, c("Avg_Value_electricity", "Avg_Value_mva_gdp", "Country Code")])
scatter_data2_clean <- na.omit(scatter_data2[, c("Avg_Value_electricity", "Avg_Value_mva_per_capita", "Country Code")])
scatter_data3_clean <- na.omit(scatter_data3[, c("Avg_Value_electricity", "Avg_Value_mva_per_capita_index", "Country Code")])
scatter_data4_clean <- na.omit(scatter_data4[, c("Avg_Value_fertility", "Avg_Value_mva_per_capita_index", "Country Code")])
scatter_data5_clean <- na.omit(scatter_data5[, c("Avg_Value_bank_deposits", "Avg_Value_mva_per_capita_index", "Country Code")])
scatter_data6_clean <- na.omit(scatter_data6[, c("Avg_Value_education", "Avg_Value_mva_per_capita_index", "Country Code")])

# Scatter plot 1: Electricity Consumption per Capita vs. MVA as % of GDP
ggplot(scatter_data1_clean, aes(x = Avg_Value_electricity, y = Avg_Value_mva_gdp, label = `Country Code`)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(color = "black", size = 3, max.overlaps = Inf) + # Increase max.overlaps
  labs(
    title = "Electricity Consumption per Capita vs. Manufacturing value added as % of GDP",
    x = "Electricity Consumption per Capita (kWh)",
    y = "Manufacturing value added as % of GDP",
    caption = "Source: World Bank Development Indicators"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

# Scatter plot 2: Electricity Consumption per Capita vs. MVA per Capita
ggplot(scatter_data2_clean, aes(x = Avg_Value_electricity, y = Avg_Value_mva_per_capita, label = `Country Code`)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(color = "black", size = 3, max.overlaps = Inf) + # Increase max.overlaps
  labs(
    title = "Electricity Consumption per Capita vs. Manufacturing value added per Capita",
    x = "Electricity Consumption per Capita (kWh)",
    y = "Manufacturing value added per Capita (2015 USD)",
    caption = "Source: World Bank"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

#Adjustments to held the range constant for comparison
range1 <- range(scatter_data3_clean$Avg_Value_mva_per_capita_index, na.rm = TRUE)
range2 <- range(scatter_data4_clean$Avg_Value_mva_per_capita_index, na.rm = TRUE)
range3 <- range(scatter_data5_clean$Avg_Value_mva_per_capita_index, na.rm = TRUE)
range4 <- range(scatter_data6_clean$Avg_Value_mva_per_capita_index, na.rm = TRUE)


# Combine all ranges to find the overall range
overall_range <- range(c(range1, range2, range3, range4), na.rm = TRUE)

# Scatter plot 3: Electricity Consumption per Capita vs. MVA per Capita Index
ggplot(scatter_data3_clean, aes(x = Avg_Value_electricity, y = Avg_Value_mva_per_capita_index, label = `Country Code`)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(color = "black", size = 3, max.overlaps = Inf) +
  labs(
    title = "Electricity Consumption per Capita vs. MVA per Capita Index",
    x = "Electricity Consumption per Capita (kWh)",
    y = "Index Manufacturing value added per Capita",
    caption = "Note: Electricity Consumption per Capita (kWh) and the MVA Index are mean centered over the last 10 years
    Index Value: MVA per capita in 2005 = 100 
    Source: World Bank Development Indicators"
  ) +
  scale_y_continuous(limits = overall_range) +  # Set consistent y-axis limits
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


# Scatter plot 4: Fertility Rate vs. MVA per Capita Index
ggplot(scatter_data4_clean, aes(x = Avg_Value_fertility, y = Avg_Value_mva_per_capita_index, label = `Country Code`)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(color = "black", size = 3, max.overlaps = Inf) +
  labs(
    title = "Fertility Rate vs. MVA per Capita Index",
    x = "Fertility rate, total (births per woman)",
    y = "Index Manufacturing value added per Capita",
    caption = "Note: Fertility rate, total (births per woman) and the MVA Index are mean centered over the last 10 years
    Index Value: MVA per capita in 2005 = 100 
    Source: World Bank Development Indicators"
  ) +
  scale_y_continuous(limits = overall_range) +  # Set consistent y-axis limits
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

# Scatter plot 5: Bank Deposits vs. MVA per Capita Index
ggplot(scatter_data5_clean, aes(x = Avg_Value_bank_deposits, y = Avg_Value_mva_per_capita_index, label = `Country Code`)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(color = "black", size = 3, max.overlaps = Inf) +
  labs(
    title = "Bank Deposits vs. MVA per Capita Index",
    x = "Bank deposits to GDP (%)",
    y = "Index Manufacturing value added per Capita",
    caption = "Note: Bank deposits to GDP (%) and the MVA Index are mean centered over the last 10 years  
    Index Value: MVA per capita in 2005 = 100 
    Source: World Bank Development Indicators"
  ) +
  scale_y_continuous(limits = overall_range) +  # Set consistent y-axis limits
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


# Scatter plot 6: Literacy Rate vs. MVA per Capita Index
ggplot(scatter_data6_clean, aes(x = Avg_Value_education, y = Avg_Value_mva_per_capita_index, label = `Country Code`)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(color = "black", size = 3, max.overlaps = Inf) +
  labs(
    title = "School enrollment, secondary vs. MVA per Capita Index",
    x = "School enrollment, secondary (% gross)",
    y = "Index Manufacturing value added per Capita",
    caption = "Note: Literacy rate, youth total (% of people ages 15-24) and the MVA Index are mean centered over the last 10 years  
    Index Value: MVA per capita in 2005 = 100 
    Source: World Bank Development Indicators"
  ) +
  scale_y_continuous(limits = overall_range) +  # Set consistent y-axis limits
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


#Further Analysis
# Combine datasets
combined_data <- scatter_data3_clean %>%
  left_join(scatter_data4_clean, by = "Country Code", suffix = c("_elec", "_fert")) %>%
  left_join(scatter_data5_clean, by = "Country Code") %>%
  left_join(scatter_data6_clean, by = "Country Code", suffix = c("_bank_deposits", "_education"))

# Fit linear regression model
model <- lm(Avg_Value_mva_per_capita_index_elec ~ Avg_Value_electricity + Avg_Value_fertility + Avg_Value_bank_deposits + Avg_Value_education, data = combined_data)
summary(model)
# Fit general linear regression model
model <- glm(Avg_Value_mva_per_capita_index_elec ~ Avg_Value_electricity + Avg_Value_fertility + Avg_Value_bank_deposits + Avg_Value_education, data = combined_data)
summary(model)





# Function to calculate period average
calculate_period_avg <- function(data, variable_name, start_year, end_year) {
  data %>%
    filter(Variable == variable_name, Year >= start_year, Year <= end_year) %>%
    group_by(`Country Name+A1:AK40`, `Country Code`) %>%
    summarise(Avg_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
}

# Function to calculate recent period average
calculate_recent_avg <- function(data, variable_name, recent_years = 5) {
  recent_years_range <- sort(unique(data$Year), decreasing = TRUE)[1:recent_years]
  
  data %>%
    filter(Variable == variable_name, Year %in% recent_years_range) %>%
    group_by(`Country Name+A1:AK40`, `Country Code`) %>%
    summarise(Avg_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
}

# Calculate averages for the periods
fertility_rate_65_70 <- calculate_period_avg(df_long, "Fertility rate, total (births per woman)", 1965, 1970)
fertility_rate_recent <- calculate_recent_avg(df_long, "Fertility rate, total (births per woman)")

savings_rate_65_70 <- calculate_period_avg(df_long, "Gross savings (% of GDP)", 1965, 1970)
savings_rate_recent <- calculate_recent_avg(df_long, "Gross savings (% of GDP)")

bank_deposits_65_70 <- calculate_period_avg(df_long, "Bank deposits to GDP (%)", 1965, 1970)
bank_deposits_recent <- calculate_recent_avg(df_long, "Bank deposits to GDP (%)")

mva_gdp_65_70 <- calculate_period_avg(df_long, "Manufacturing, value added (% of GDP)", 1965, 1970)
mva_gdp_recent <- calculate_recent_avg(df_long, "Manufacturing, value added (% of GDP)")

# Merge data for scatter plots
merge_data <- function(df1, df2, suffix1, suffix2) {
  merge(df1, df2, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c(suffix1, suffix2))
}

# Prepare data for scatter plots
plot_data_fertility_mva_65_70 <- merge_data(fertility_rate_65_70, mva_gdp_65_70, "_fertility", "_mva_gdp")
plot_data_fertility_mva_recent <- merge_data(fertility_rate_recent, mva_gdp_recent, "_fertility", "_mva_gdp")

plot_data_savings_mva_65_70 <- merge_data(savings_rate_65_70, mva_gdp_65_70, "_savings", "_mva_gdp")
plot_data_savings_mva_recent <- merge_data(savings_rate_recent, mva_gdp_recent, "_savings", "_mva_gdp")

plot_data_bank_deposits_mva_65_70 <- merge_data(bank_deposits_65_70, mva_gdp_65_70, "_bank_deposits", "_mva_gdp")
plot_data_bank_deposits_mva_recent <- merge_data(bank_deposits_recent, mva_gdp_recent, "_bank_deposits", "_mva_gdp")

# Function to create scatter plots
plot_scatter <- function(data, x_var, y_var, x_label, y_label, title) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), label = `Country Code`)) +
    geom_point(color = "#1f77b4", size = 4, alpha = 0.8) +
    geom_text_repel(size = 3, box.padding = 0.5) +
    labs(title = title,
         x = x_label,
         y = y_label,
         caption = "Source: World Bank Development Indicators") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color = "black"),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_line(color = "gray90"))
}

# Generate and display scatter plots
plot_fertility_mva_65_70 <- plot_scatter(plot_data_fertility_mva_65_70, "Avg_Value_fertility", "Avg_Value_mva_gdp", "Fertility Rate (1965-70)", "MVA as % of GDP (1965-70)", "Fertility Rate vs MVA (1965-70)")
plot_fertility_mva_recent <- plot_scatter(plot_data_fertility_mva_recent, "Avg_Value_fertility", "Avg_Value_mva_gdp", "Fertility Rate (Recent 5 Years)", "MVA as % of GDP (Recent 5 Years)", "Fertility Rate vs MVA (Recent 5 Years)")

plot_savings_mva_65_70 <- plot_scatter(plot_data_savings_mva_65_70, "Avg_Value_savings", "Avg_Value_mva_gdp", "Savings Rate (% of GDP) (1965-70)", "MVA as % of GDP (1965-70)", "Savings Rate vs MVA (1965-70)")
plot_savings_mva_recent <- plot_scatter(plot_data_savings_mva_recent, "Avg_Value_savings", "Avg_Value_mva_gdp", "Savings Rate (% of GDP) (Recent 5 Years)", "MVA as % of GDP (Recent 5 Years)", "Savings Rate vs MVA (Recent 5 Years)")

plot_bank_deposits_mva_65_70 <- plot_scatter(plot_data_bank_deposits_mva_65_70, "Avg_Value_bank_deposits", "Avg_Value_mva_gdp", "Bank Deposits (% of GDP) (1965-70)", "MVA as % of GDP (1965-70)", "Bank Deposits vs MVA (1965-70)")
plot_bank_deposits_mva_recent <- plot_scatter(plot_data_bank_deposits_mva_recent, "Avg_Value_bank_deposits", "Avg_Value_mva_gdp", "Bank Deposits (% of GDP) (Recent 5 Years)", "MVA as % of GDP (Recent 5 Years)", "Bank Deposits vs MVA (Recent 5 Years)")



# Calculate averages for the periods
literacy_rate_70_80 <- calculate_period_avg(df_long, "Literacy rate, adult total (% of people ages 15 and above)", 1970, 1980)
literacy_rate_recent <- calculate_recent_avg(df_long, "Literacy rate, adult total (% of people ages 15 and above)")

mva_per_capita_70_80 <- calculate_period_avg(df_long, "MVA per capita", 1970, 1980)
mva_per_capita_recent <- calculate_recent_avg(df_long, "MVA per capita")

mva_gdp_70_80 <- calculate_period_avg(df_long, "Manufacturing, value added (% of GDP)", 1970, 1980)
mva_gdp_recent <- calculate_recent_avg(df_long, "Manufacturing, value added (% of GDP)")

# Merge data for scatter plots
merge_data <- function(df1, df2, suffix1, suffix2) {
  merge(df1, df2, by = c("Country Name+A1:AK40", "Country Code"), suffixes = c(suffix1, suffix2))
}

# Prepare data for scatter plots
plot_data_literacy_mva_per_capita_70_80 <- merge_data(literacy_rate_70_80, mva_per_capita_70_80, "_literacy", "_mva_per_capita")
plot_data_literacy_mva_per_capita_recent <- merge_data(literacy_rate_recent, mva_per_capita_recent, "_literacy", "_mva_per_capita")

plot_data_literacy_mva_gdp_70_80 <- merge_data(literacy_rate_70_80, mva_gdp_70_80, "_literacy", "_mva_gdp")
plot_data_literacy_mva_gdp_recent <- merge_data(literacy_rate_recent, mva_gdp_recent, "_literacy", "_mva_gdp")

# Function to create scatter plots with consistent scales
plot_scatter <- function(data, x_var, y_var, x_label, y_label, title, x_lim = NULL, y_lim = NULL) {
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), label = `Country Code`)) +
    geom_point(color = "#1f77b4", size = 4, alpha = 0.8) +
    geom_text_repel(size = 3, box.padding = 0.5) +
    labs(title = title,
         x = x_label,
         y = y_label,
         caption = "Source: World Bank Development Indicators") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color = "black"),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_line(color = "gray90"))
  
  if (!is.null(x_lim)) {
    p <- p + scale_x_continuous(limits = x_lim)
  }
  
  if (!is.null(y_lim)) {
    p <- p + scale_y_continuous(limits = y_lim)
  }
  
  return(p)
}

# Define consistent scales
x_lim_literacy <- range(c(literacy_rate_70_80$Avg_Value, literacy_rate_recent$Avg_Value), na.rm = TRUE)
x_lim_bank_deposits <- range(c(bank_deposits_65_70$Avg_Value, bank_deposits_recent$Avg_Value), na.rm = TRUE)
x_lim_fertility <- range(c(fertility_rate_65_70$Avg_Value, fertility_rate_recent$Avg_Value), na.rm = TRUE)
y_lim_mva_per_capita <- range(c(mva_per_capita_70_80$Avg_Value, mva_per_capita_recent$Avg_Value), na.rm = TRUE)
y_lim_mva_gdp <- range(c(mva_gdp_70_80$Avg_Value, mva_gdp_recent$Avg_Value), na.rm = TRUE)
y_lim_mva_gdp_65_70 <- range(c(mva_gdp_65_70$Avg_Value, mva_gdp_recent$Avg_Value), na.rm = TRUE)

# Generate and display scatter plots
plot_literacy_mva_per_capita_70_80 <- plot_scatter(plot_data_literacy_mva_per_capita_70_80, "Avg_Value_literacy", "Avg_Value_mva_per_capita", "Adult Literacy Rate (1970-80)", "MVA per Capita (1970-80)", "Adult Literacy Rate vs MVA per Capita (1970-80)", x_lim_literacy, y_lim_mva_per_capita)
plot_literacy_mva_per_capita_recent <- plot_scatter(plot_data_literacy_mva_per_capita_recent, "Avg_Value_literacy", "Avg_Value_mva_per_capita", "Adult Literacy Rate (Recent 5 Years)", "MVA per Capita (Recent 5 Years)", "Adult Literacy Rate vs MVA per Capita (Recent 5 Years)", x_lim_literacy, y_lim_mva_per_capita)

plot_fertility_mva_65_70 <- plot_scatter(plot_data_fertility_mva_65_70, "Avg_Value_fertility", "Avg_Value_mva_gdp", "Fertility Rate (1965-70)", "MVA as % of GDP (1965-70)", "Fertility Rate vs MVA (1965-70)", x_lim_fertility, y_lim_mva_gdp_65_70)
plot_fertility_mva_recent <- plot_scatter(plot_data_fertility_mva_recent, "Avg_Value_fertility", "Avg_Value_mva_gdp", "Fertility Rate (Recent 5 Years)", "MVA as % of GDP (Recent 5 Years)", "Fertility Rate vs MVA (Recent 5 Years)",x_lim_fertility, y_lim_mva_gdp_65_70)

plot_savings_mva_65_70 <- plot_scatter(plot_data_savings_mva_65_70, "Avg_Value_savings", "Avg_Value_mva_gdp", "Savings Rate (% of GDP) (1965-70)", "MVA as % of GDP (1965-70)", "Savings Rate vs MVA (1965-70)")
plot_savings_mva_recent <- plot_scatter(plot_data_savings_mva_recent, "Avg_Value_savings", "Avg_Value_mva_gdp", "Savings Rate (% of GDP) (Recent 5 Years)", "MVA as % of GDP (Recent 5 Years)", "Savings Rate vs MVA (Recent 5 Years)")

plot_bank_deposits_mva_65_70 <- plot_scatter(plot_data_bank_deposits_mva_65_70, "Avg_Value_bank_deposits", "Avg_Value_mva_gdp", "Bank Deposits (% of GDP) (1965-70)", "MVA as % of GDP (1965-70)", "Bank Deposits vs MVA (1965-70)", x_lim_bank_deposits, y_lim_mva_gdp_65_70)
plot_bank_deposits_mva_recent <- plot_scatter(plot_data_bank_deposits_mva_recent, "Avg_Value_bank_deposits", "Avg_Value_mva_gdp", "Bank Deposits (% of GDP) (Recent 5 Years)", "MVA as % of GDP (Recent 5 Years)", "Bank Deposits vs MVA (Recent 5 Years)", x_lim_bank_deposits, y_lim_mva_gdp_65_70)

# Display plots
print(plot_literacy_mva_per_capita_70_80)
print(plot_literacy_mva_per_capita_recent)
print(plot_fertility_mva_65_70)
print(plot_fertility_mva_recent)
print(plot_savings_mva_65_70)
print(plot_savings_mva_recent)
print(plot_bank_deposits_mva_65_70)
print(plot_bank_deposits_mva_recent)


# Define countries of interest
countries <- c("South Africa", "Mauritius", "Morocco", "Ethiopia", "Ghana", 
               "Kenya", "Angola", "Zimbabwe", "Bangladesh", "Vietnam")

# Filter data for fertility rate and countries of interest
filtered_data <- subset(df_long, `Country Name+A1:AK40` %in% countries & 
                          Variable == "Fertility rate, total (births per woman)")


# Define a color palette (adjust colors as needed)
custom_color_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# Plot fertility rates over time for each country
ggplot(filtered_data, aes(x = Year, y = Value, color = `Country Name+A1:AK40`)) +
  geom_line(size = 1) +
  labs(title = "Fertility Rate Over Time",
       x = " ", y = "Fertility Rate (births per woman)",
       color = " ") +
  scale_color_manual(values = custom_color_palette) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))


# Filter data for fertility rate and countries of interest
filtered_data <- subset(df_long, `Country Name+A1:AK40` %in% countries & 
                          Variable == "MVA per capita")
# Plot fertility rates over time for each country
ggplot(filtered_data, aes(x = Year, y = Value, color = `Country Name+A1:AK40`)) +
  geom_line(size = 1) +
  labs(title = "MVA per capita Over Time",
       x = " ", y = "MVA per capita in 2015USD",
       color = " ") +
  scale_color_manual(values = custom_color_palette) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))

# Filter data for fertility rate and countries of interest
filtered_data <- subset(df_long, `Country Name+A1:AK40` %in% countries & 
                          Variable == "Electric power consumption (kWh per capita)")

ggplot(filtered_data, aes(x = Year, y = Value, color = `Country Name+A1:AK40`)) +
  geom_line(size = 1) +
  labs(title = "Electric power consumption Over Time",
       x = " ", y = "Electric power consumption (kWh per capita)",
       color = " ") +
  scale_color_manual(values = custom_color_palette) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))

# Filter data for fertility rate and countries of interest
filtered_data <- subset(df_long, `Country Name+A1:AK40` %in% countries & 
                          Variable == "Bank deposits to GDP (%)")

ggplot(filtered_data, aes(x = Year, y = Value, color = `Country Name+A1:AK40`)) +
  geom_line(size = 1) +
  labs(title = "Bank deposits to GDP (%) Over Time",
       x = " ", y = "Bank deposits to GDP (%)",
       color = " ") +
  scale_color_manual(values = custom_color_palette) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))

# Filter data for fertility rate and countries of interest
filtered_data <- subset(df_long, `Country Name+A1:AK40` %in% countries & 
                          Variable == "Manufacturing, value added (% of GDP)")

ggplot(filtered_data, aes(x = Year, y = Value, color = `Country Name+A1:AK40`)) +
  geom_line(size = 1) +
  labs(title = "Manufacturing, value added (% of GDP) Over Time",
       x = " ", y = "Manufacturing, value added (% of GDP)",
       color = " ") +
  scale_color_manual(values = custom_color_palette) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))



