# =================================================================================================
# Working with the Comtrade Database
# Luca De Benedictis - International Economics - Luiss - Management 2025-2026
#
# Includes: 
# comtradr library
# Comtrade API key at <https://comtradedeveloper.un.org/signin?returnUrl=%2Fapi-details#api=comtrade-v1&operation=get-get>
# Countries' exports and imports
# An example with Italian data
# 
# Week 6
# =================================================================================================

# The **UN Comtrade Database** provides free access to international trade data. You can get data by using their data extraction interface or API. In this document, we share some possible ways of downloading, preparing and plotting trade data in R. For more details on Comtrade see <https://comtradeplus.un.org/> and <https://cran.r-project.org/web/packages/comtradr/vignettes/comtradr.html>; while the super-organizational project **WITS** collects data on trade, value added, tariffs and more <http://wits.worldbank.org/visualization>.

# https://comtradeapi.un.org/files/v1/app/publicationfiles/2022/VolI2022.pdf (Italy at pages 180-181)

#--------------------------------------------

#library(rjson)
library(ggplot2)
library(dplyr)

library(comtradr)

#--------------------------------------------

set_primary_comtrade_key("cb074baa3e7d4056a3c4ee4597dde96e")
#set_primary_comtrade_key("41683b2fedef42c2a8d192969bb9d868")
get_primary_comtrade_key()	# just to check

#--------------------------------------------

#----------------------------------------------
# Figure 1 in 2022 UN Yearbook
#----------------------------------------------
# exports

Ita_exp_2013_2022 <- ct_get_data(
	reporter = 'ITA',
	partner = 'World',
	commodity_code = 'TOTAL',
	start_date = 2013,
	end_date = 2022,
	flow_direction = 'export'
)

Ita_exp_2013_2022  <- Ita_exp_2013_2022[, c("ref_year", "fobvalue")]

Ita_exp_2002_2012 <- ct_get_data(
	reporter = 'ITA',
	partner = 'World',
	commodity_code = 'TOTAL',
	start_date = 2002,
	end_date = 2012,
	flow_direction = 'export'
)

Ita_exp_2002_2012  <- Ita_exp_2002_2012[, c("ref_year", "fobvalue")]

Ita_exp_2002_2022 <- rbind(Ita_exp_2002_2012, Ita_exp_2013_2022)

#----------------------------------------------
# imports

Ita_imp_2013_2022 <- ct_get_data(
	reporter = 'ITA',
	partner = 'World',
	commodity_code = 'TOTAL',
	start_date = 2013,
	end_date = 2022,
	flow_direction = 'import'
)

Ita_imp_2013_2022  <- Ita_imp_2013_2022[, c("ref_year", "cifvalue")]

Ita_imp_2002_2012 <- ct_get_data(
	reporter = 'ITA',
	partner = 'World',
	commodity_code = 'TOTAL',
	start_date = 2002,
	end_date = 2012,
	flow_direction = 'import'
)

Ita_imp_2002_2012  <- Ita_imp_2002_2012[, c("ref_year", "cifvalue")]

Ita_imp_2002_2022 <- rbind(Ita_imp_2002_2012, Ita_imp_2013_2022)



# Put everything in a data frame
trade_data <- data.frame(
	Year <- Ita_exp_2002_2022$ref_year,
	Exports <- Ita_exp_2002_2022$fobvalue*1/1000000000,
	Imports <- Ita_imp_2002_2022$cifvalue*1/1000000000, 
	TradeBalance <- Ita_exp_2002_2022$fobvalue*1/1000000000 - Ita_imp_2002_2022$cifvalue*1/1000000000 
)

# Make sure Year is a factor
trade_data$Year <- as.factor(trade_data$Year)

# Define colors
colors <- c("Exports" = "#4664A0", "Imports" = "#AABEDC", "Trade Balance" = "darkred")

# Plot
ggplot(trade_data) +
	geom_bar(aes(x = Year, y = Exports, fill = "Exports"), stat = "identity", position = "dodge") +
	geom_bar(aes(x = Year, y = -Imports, fill = "Imports"), stat = "identity", position = "dodge") +
	geom_line(aes(x = Year, y = TradeBalance, group = 1, color = "Trade Balance"), size = 1) +
	geom_point(aes(x = Year, y = TradeBalance, color = "Trade Balance"), size = 2) +
	scale_fill_manual(values = colors) +
	scale_color_manual(values = colors) +
	scale_y_continuous(
		labels = scales::comma, 
		breaks = seq(-800, 800, by = 200),
		limits = c(-800, 800), 
		sec.axis = sec_axis(~./10, name = "Trade Balance", labels = scales::comma)
	) +
	labs(x = "Year", y = "Value (Bln US$)", fill = "", color = "") +
	theme_minimal() +
	theme(
		axis.text.x = element_text(angle = 90, hjust = 1),
		legend.position = "bottom"
	) +
	guides(fill = guide_legend(title = ""), color = guide_legend(title = ""))

#---------------------------------------------------------------------------------------------------------
# Plot the top eight destination countries/areas of Italian Wine exports, by liter (L), for 2012 - 2022.
#---------------------------------------------------------------------------------------------------------

# First, collect commodity codes related to wine
wine_codes <- ct_commodity_lookup("wine",
							 return_code = TRUE,
							 return_char = TRUE)

wine_codes_description <- ct_commodity_lookup("wine",
						    return_code = FALSE,
						    return_char = TRUE)

wine_codes <- wine_codes[22:30]

# Comtrade api query.
Italy_wine_exp_2014_2024 <- ct_get_data(
	reporter = 'ITA',
	partner = "all_countries",
	flow_direction = "export",
	start_date = 2014,
	end_date = 2024,
	commodity_code = wine_codes)

unique(Italy_wine_exp_2014_2024$partner_desc)


# Create country specific "total weight per year" dataframe for plotting.
plotdf <- Italy_wine_exp_2014_2024 %>%
	group_by(partner_desc, period) %>%
	summarise(l = as.numeric(sum(net_wgt, na.rm = TRUE))) 

# Get vector of the top 16 destination countries/areas by total volume (in liters) shipped
# across all years, then subset plotdf to only include observations related
# to those countries/areas.
top16 <- plotdf |> 
	group_by(partner_desc) |> 
	summarise(l = as.numeric(sum(l, na.rm = TRUE))) |> 
	slice_max(n = 16, order_by = l) |> 
	arrange(desc(l)) |> 
	pull(partner_desc)
plotdf <- plotdf %>% filter(partner_desc %in% top16)

# Create plots (y-axis is NOT fixed across panels, this will allow us to ID
# trends over time within each country/area individually).
ggplot(plotdf,aes(period,l/1000, group = partner_desc))+
	geom_line() + 
	geom_point() + 
	facet_wrap(.~partner_desc, nrow = 4, ncol = 4,scales = 'free_y')+
	labs(title = "Liters of Italian Wine Exports", 
		subtitle ="by Destination Area, 2014 - 2024")+
	theme_minimal()+
	theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1))


# Define the colours for each country from the prevalent colour of the flag of the country

country_colors <- c(
	"Germany" = "black", 
	"USA" = "dodgerblue", 
	"United Kingdom" = "red4", 
	"France" = "blue", 
	"Canada" = "firebrick", 
	"Switzerland" = "darksalmon", 
	"Russian Federation" = "royalblue", 
	"Sweden" = "gold", 
	"Netherlands" = "darkorange", 
	"Austria" = "darkred", 
	"Belgium" = "grey30", 
	"Japan" = "tomato", 
	"Denmark" = "lightcoral", 
	"Spain" = "red", 
	"Czechia" = "navy", 
	"China" = "maroon"
)

# Create plots
ggplot(plotdf, aes(x = period, y = l/1000, group = partner_desc, color = partner_desc)) +
	geom_line() + 
	geom_point() + 
	facet_wrap(.~partner_desc, nrow = 4, ncol = 4, scales = 'free_y') +
	scale_color_manual(values = country_colors) +
	labs(title = "Liters of Italian Wine Exports", 
		subtitle ="by Main destination markets, 2014 - 2024") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
		 legend.position = "bottom") +
	guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Note that the 'color' aesthetic is mapped to 'partner_desc' to assign the colors to lines
