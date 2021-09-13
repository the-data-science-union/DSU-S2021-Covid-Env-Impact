so2_data <- read.csv("SO2_EDA_Data.csv")

so2_data$NG_Con <- as.numeric(gsub(",","",so2_data$NG_Con))

so2_data[64, "NG_Con"] <- mean((so2_data[1:60, "NG_Con"]))

so2_graphs <- so2_data[, c("SO2_Con", "NG_Con", "Tot_Petrol_AS", 
                           "Net_gen_AS", "Tot_.Ind_.Coal", "SUN", "WND",
                           "WAT")]

scaled_graphs <- data.frame(scale(so2_graphs))

colnames(so2_data)

## Graphs

# Natural Gas

plot(SO2_Con ~ NG_Con, data = scaled_graphs,
     xlab = "Natural Gas (Mil of Feet^3)",
     ylab = "SO2 Concentration (PPM)",
     main = "Natural Gas by SO2 Concentration Z-Scores",
     col = "chartreuse3",
     pch = 20
     )
abline(lm(SO2_Con ~ NG_Con, data = scaled_graphs),
       col = "red")
mod_ng <- summary(lm(SO2_Con ~ NG_Con, data = scaled_graphs))
text(2, -0.2, expression(r^2 == 0.0018))

# Petroleum 

plot(SO2_Con ~ Tot_Petrol_AS, data = scaled_graphs,
     xlab = "Petroleum (Thousands of Barrells)",
     ylab = "SO2 Concentration (PPM)",
     main = "Petroleum by SO2 Concentration Z-Scores",
     pch = 20,
     col = "deepskyblue"
     )
abline(lm(SO2_Con ~ Tot_Petrol_AS, data = scaled_graphs),
       col = "red")
mod_pet <- summary(lm(SO2_Con ~ Tot_Petrol_AS, data = scaled_graphs))
text(2, -0.1, expression(r^2 == 0.0089))

# Electricity

plot(SO2_Con ~ Net_gen_AS, data = scaled_graphs,
     xlab = "Electricity (Thousands of MWh)",
     ylab = "SO2 Concentration (PPM)",
     main = "Electricity by SO2 Concentration Z-Scores",
     pch = 20,
     col = "darkgoldenrod3"
     )
abline(lm(SO2_Con ~ Net_gen_AS, data = scaled_graphs),
       col = "red")
mod_ele <- summary(lm(SO2_Con ~ Net_gen_AS, data = scaled_graphs))
text(0.75, -0.5, expression(r^2 == 0.0200))

# Coal

plot(SO2_Con ~ Tot_.Ind_.Coal, data = scaled_graphs,
     xlab = "Coal (Thousands of Tons)",
     ylab = "SO2 Concentration (PPM)",
     main = "Coal by SO2 Concentration Z-Scores",
     pch = 20,
     col = "darkorchid3"
     )
abline(lm(SO2_Con ~ Tot_.Ind_.Coal, data = scaled_graphs),
       col = "red")
mod_coal <- summary(lm(SO2_Con ~ Tot_.Ind_.Coal, data = scaled_graphs))
text(-3, -0.5, expression(r^2 == 0.0062))

# Solar

plot(SO2_Con ~ SUN, data = scaled_graphs,
     xlab = "Solar Energy (Net MWh)",
     ylab = "SO2 Concentration (PPM)",
     main = "Solar Energy by SO2 Concentration Z-Scores",
     pch = 20,
     col = "darkorange3"
)
abline(lm(SO2_Con ~ SUN, data = scaled_graphs),
       col = "red")
mod_sun <- summary(lm(SO2_Con ~ SUN, data = scaled_graphs))
text(1.75, 1.75, expression(r^2 == 0.0231))

# Wind

plot(SO2_Con ~ WND, data = scaled_graphs,
     xlab = "Wind Energy (Net MWh)",
     ylab = "SO2 Concentration (PPM)",
     main = "Wind Energy by SO2 Concentration Z-Scores",
     pch = 20,
     col = "dodgerblue2"
)
abline(lm(SO2_Con ~ WND, data = scaled_graphs),
       col = "red")
mod_wnd <- summary(lm(SO2_Con ~ WND, data = scaled_graphs))
text(-1.8, 0.1, expression(r^2 == 0.0743))

# Hydro 

plot(SO2_Con ~ WAT, data = scaled_graphs,
     xlab = "Hydro Energy (Net MWh)",
     ylab = "SO2 Concentration (PPM)",
     main = "Hydro Energy by SO2 Concentration Z-Scores",
     pch = 20,
     col = "aquamarine3"
)
abline(lm(SO2_Con ~ WAT, data = scaled_graphs),
       col = "red")
mod_wat <- summary(lm(SO2_Con ~ WAT, data = scaled_graphs))
text(1.9, 1, expression(r^2 == 0.0191))

