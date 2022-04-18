library(dplyr)
library(msm)
library(lubridate)
S_P <- read.csv("C:/Users/Tatyana/Downloads/ÂÊÐ/year/S&P.csv")
S_P %>% summarise_all(n_distinct)
S_P = S_P[, c("Company.Name", "Date", "Curr.Rtg", "years")]
state <- statetable.msm(Curr.Rtg, Company.Name, data = S_P)
#colnames(state) = c("AAA-A", "BBB-B", "CCC-C", "D")
#rownames(state) = c("AAA-A", "BBB-B", "CCC-C")
state

calculate_years <- function(dataframe) {
  dataframe["Date"] = as.Date(dataframe$Date)
  dataframe <- dataframe %>%
    group_by(Company.Name) %>%
    mutate(Diff = Date - lag(Date))
  dataframe$Diff <- as.numeric(dataframe$Diff, units = "days")
  dataframe$Diff <- dataframe$Diff %>% replace(is.na(.), 0)
  dataframe$Diff <- dataframe$Diff / 365.25
  dataframe <- dataframe %>%
    group_by(Company.Name) %>%
    mutate(years = cumsum(Diff))
  return(dataframe)
}
S_P = calculate_years(S_P)
# Heatmap
library(pheatmap)
pheatmap(state, display_numbers = T, cluster_rows = F, cluster_cols = F)
pheatmap(state, display_numbers = T, cluster_rows = F, cluster_cols = F, scale = "row")

Q <- rbind(c(1, 1, 1, 1, 1),
           c(1, 1, 1, 1, 1),
           c(1, 1, 1, 1, 1),
           c(1, 1, 1, 1, 1),
           c(0, 0, 0, 0, 0))
          
Q.crude <- crudeinits.msm(Curr.Rtg ~ years, Company.Name, data = S_P, qmatrix = Q)
cav.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P, qmatrix = Q.crude,
               deathexact = 5, control = list(fnscale = 4000, trace = 2, REPORT = 1,
                                               maxit = 300))
pmatrix.msm(cav.msm)
pheatmap(pmatrix.msm(cav.msm), display_numbers = T, cluster_rows = F, cluster_cols = F)

S_P["Year"] <- format(as.Date(S_P$Date), format = "%Y")
# p matrices for each year
years <- seq(2000, 2021)
matrix_norms <- c()
c_to_default <- c()
b_to_default <- c()
a_to_default <- c()
bbb_to_default <- c()
pmatrix_difference <- 0
for (x in years) {
  print(x)
  S_P_year = S_P[S_P["Year"] == x, ]
  S_P_year
  S_P_year <- calculate_years(S_P_year)
  cav_year.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_year, qmatrix = Q.crude,
                      deathexact = 5, control = list(fnscale = 24000, trace = 2, REPORT = 1,
                                                      maxit = 500))
  pmatr <- pmatrix.msm(cav_year.msm)
  state_rowsums <- rowSums(statetable.msm(Curr.Rtg, Company.Name, data = S_P_year))
  state_rowsums <- c(state_rowsums, "5" = 0)
  pmatrix_difference = pmatrix_difference + 
    state_rowsums * (pmatrix.msm(cav_year.msm) - pmatrix.msm(cav.msm))^2 / 
    (pmatrix.msm(cav.msm) + 1e-20) * (pmatrix.msm(cav_year.msm) != 0 & pmatrix.msm(cav.msm))
  a_to_default <- c(a_to_default, pmatr[1, 5])
  b_to_default <- c(b_to_default, pmatr[3, 5])
  c_to_default <- c(c_to_default, pmatr[4, 5])
  bbb_to_default <- c(bbb_to_default, pmatr[2, 5])
  matrix_norms <- c(matrix_norms, norm(pmatr))
}

library(PEIP)
print(chi2inv(0.95, 4 * (5 - 1) * (22 - 1)))
print(sum(pmatrix_difference))

years <- seq(2000, 2021)
matrix_years <- list(
  year = years,
  matrix_norm = matrix_norms,
  a_default = a_to_default,
  b_default = b_to_default,
  bbb_default = bbb_to_default,
  c_default = c_to_default
)

norms_df = as.data.frame(matrix_years)
norms_df

library(ggplot2)
ggplot(norms_df, aes(x=year, y=matrix_norm)) +
  geom_line() +
  geom_point() +
  ggtitle("Transition probability matrix norms")

colors <- c("AAA-A to D" = "blue", "BBB-B to D" = "red", 
            "CCC-C to D" = "orange", "BBB to D" = "green")
ggplot(norms_df, aes(x=years)) +
  geom_line(aes(y=a_default, color = "AAA-A to D"), alpha = 0.8) +
  geom_line(aes(y=bbb_default, color = "BBB to D"), alpha = 0.8) +
  geom_line(aes(y=b_default, color = "BB-B to D"), alpha = 0.8) + 
  geom_line(aes(y=c_default, color = "CCC-C to D"), alpha = 0.8) +
  geom_point(aes(y = a_default, color = "AAA to D")) +
  geom_point(aes(y = bbb_default, color = "BBB to D")) +
  geom_point(aes(y = b_default, color = "BB-B to D")) +
  geom_point(aes(y = c_default, color = "CCC-C to D")) + 
  ggtitle("Default Probabilities") + 
  labs(x = "Year",
       y = "Default Probability",
       color = "Legend") +
  scale_color_manual(values = colors)

# GDP
S_P_GDP <- read.csv("C:/Users/Tatyana/Downloads/ÂÊÐ/year/S&P_GDP.csv")
S_P_GDP["Year"] <- format(S_P_GDP[, "Date"], format = "%Y")
yrs <- 2018
yr <- as.Date(as.character(yrs), format = "%Y")
y <- year(yr)
y
S_P_GDP_train = S_P_GDP[S_P_GDP$Year < y, ]
S_P_GDP_train = S_P_GDP_train[order(S_P_CDS$Company.Name, S_P_CDS$Year), ]
S_P_GDP_train = calculate_years(S_P_GDP_train)
S_P_GDP_train
Q.crude <- crudeinits.msm(Curr.Rtg ~ years, Company.Name, data = S_P_GDP_train, qmatrix = Q)
cav_gdp.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_GDP_train, 
               qmatrix = Q.crude, deathexact = 5,
               control = list(fnscale = 4000, trace = 2, REPORT = 1, maxit = 3000, 
                              reltol = 1e-12),
               covariates = ~GDP)
cav_gdp.msm

S_P_GDP["Year"] <- format(as.Date(S_P_GDP$Date), format = "%Y")
abs_diff = 0
yrs <- c(2018, 2019, 2020, 2021)
yr <- as.Date(as.character(yrs), format = "%Y")
test_years <- year(yr)
for (x in test_years) {
  print(x)
  S_P_GDP_year = S_P_GDP[S_P_GDP["Year"] == x,]
  S_P_GDP_year = calculate_years(S_P_GDP_year)
  cav_year.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_GDP_year, qmatrix = Q.crude,
                      deathexact = 5, control = list(fnscale = 24000, trace = 2, REPORT = 1,
                                                     maxit = 500))
  pmatr_true = pmatrix.msm(cav_year.msm)
  gdp = S_P_GDP_year[1, "GDP"]
  pmatr_hat = pmatrix.msm(cav_gdp.msm, covariates = list(GDP = gdp))
  abs_diff = abs_diff + mean(abs(pmatr_true - pmatr_hat))
}
print(abs_diff / 4)

S_P_CDS <- read.csv("C:/Users/Tatyana/Downloads/ÂÊÐ/year/S&P_CDS.csv")
S_P_CDS["Year"] <- format(S_P_CDS[, "Date"], format = "%Y")
S_P_CDS <- S_P_CDS[order(S_P_CDS$Company.Name, S_P_CDS$Year), ]
S_P_CDS = calculate_years(S_P_CDS)
S_P_CDS
yrs <- 2018
yr <- as.Date(as.character(yrs), format = "%Y")
y <- year(yr)
y
S_P_CDS_train = S_P_CDS[S_P_CDS$Year < y, ]
Q.crude <- crudeinits.msm(Curr.Rtg ~ years, Company.Name, data = S_P_CDS_train, qmatrix = Q)
cav_cds.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_CDS_train, 
               qmatrix = Q.crude, deathexact = 5, 
               control = list(fnscale = 4000, trace = 2, REPORT = 1, maxit = 300),
               covariates = ~PX_LAST)
cav_cds.msm
S_P_CDS["Year"] <- format(as.Date(S_P_CDS$Date), format = "%Y")
abs_diff = 0
test_years <- c(2018, 2019, 2020, 2021)
for (x in test_years) {
  print(x)
  S_P_CDS_year = S_P_CDS[S_P_CDS["Year"] == x,]
  S_P_CDS_year = calculate_years(S_P_CDS_year)
  cav_year.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_CDS_year, qmatrix = Q.crude,
                      deathexact = 5, control = list(fnscale = 24000, trace = 2, REPORT = 1,
                                                     maxit = 500))
  px = S_P_CDS_year[1, "PX_LAST"]
  pmatr_true = pmatrix.msm(cav_year.msm)
  pmatr_hat = pmatrix.msm(cav_cds.msm, covariates = list(PX_LAST = px))
  abs_diff = abs_diff + mean(abs(pmatr_true - pmatr_hat))
}
print(abs_diff / 4)

S_P_GDP["Year"] <- format(as.Date(S_P_GDP$Date), format = "%Y")
abs_diff = 0
yrs <- c(2008, 2021)
yr <- as.Date(as.character(yrs), format = "%Y")
test_years <- year(yr)
pmatrices = c()
for (x in test_years) {
  print(x)
  S_P_GDP_year = S_P_GDP[S_P_GDP["Year"] == x,]
  S_P_GDP_year = calculate_years(S_P_GDP_year)
  cav_year.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_GDP_year, qmatrix = Q.crude,
                      deathexact = 5, control = list(fnscale = 24000, trace = 2, REPORT = 1,
                                                     maxit = 500))
  print(pmatrix.msm(cav_year.msm))
}

#2021
S_P_GDP_year = S_P_GDP[S_P_GDP["Year"] == 2021,]
S_P_CDS_year = S_P_CDS[S_P_CDS["Year"] == 2021,]
S_P_GDP_year = calculate_years(S_P_GDP_year)
S_P_CDS_year = calculate_years(S_P_CDS_year)
cav_year.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_GDP_year, qmatrix = Q.crude,
                    deathexact = 5, control = list(fnscale = 24000, trace = 2, REPORT = 1,
                                                   maxit = 500))
gdp = S_P_GDP_year[1, "GDP"]
px_last = S_P_CDS_year[1, "PX_LAST"]
print(pmatrix.msm(cav_year.msm))
print(pmatrix.msm(cav_gdp.msm, covariates = list(GDP = gdp)))
print(pmatrix.msm(cav_cds.msm, covariates = list(PX_LAST = px_last)))

print(qmatrix.msm(cav_year.msm))
print(qmatrix.msm(cav_gdp.msm, covariates = list(GDP = gdp)))
print(qmatrix.msm(cav_cds.msm, covariates = list(PX_LAST = px_last)))

#2020
S_P_GDP_year = S_P_GDP[S_P_GDP["Year"] == 2020,]
S_P_CDS_year = S_P_CDS[S_P_CDS["Year"] == 2020,]
S_P_GDP_year = calculate_years(S_P_GDP_year)
S_P_CDS_year = calculate_years(S_P_CDS_year)
cav_year.msm <- msm(Curr.Rtg ~ years, subject = Company.Name, data = S_P_GDP_year, qmatrix = Q.crude,
                    deathexact = 5, control = list(fnscale = 24000, trace = 2, REPORT = 1,
                                                   maxit = 500))
gdp = S_P_GDP_year[1, "GDP"]
px_last = S_P_CDS_year[1, "PX_LAST"]
print(pmatrix.msm(cav_year.msm))
print(pmatrix.msm(cav_gdp.msm, covariates = list(GDP = gdp)))
print(pmatrix.msm(cav_cds.msm, covariates = list(PX_LAST = px_last)))

print(qmatrix.msm(cav_year.msm))
print(qmatrix.msm(cav_gdp.msm, covariates = list(GDP = gdp)))
print(qmatrix.msm(cav_cds.msm, covariates = list(PX_LAST = px_last)))

lrtest.msm(cav.msm, cav_cds.msm)
lrtest.msm(cav.msm, cav_gdp.msm)
