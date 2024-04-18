## Parameters
RR = 3.8 # Forcing at 2x CO2
C0 = 280 # Pre-industrial CO2
c = 0.098 # reaction rate of atmosphere
ECS = 3 # Equilibrium climate sensitivity
alpha = 1.0038 # Damage function scale
c.1960 = 3585.427 # GDP p.c. in 1960
c.2020 = 10542.301 # GDP p.c. in 2020
eta = 1.45 # Elasticity of marginal consumption

## Load the CO2 concentrations
co2 = read.csv("~/Dropbox/Climate Change Economics 2024/labs/lab3 - social cost of carbon/co2_ppm.csv")
## Select the SSP2-4.5 scenario for concentrations after 1960
co2.ssp245 = subset(co2, scenario == 'ssp245' & year > 1960)

## Set up the initial values: T_1960 = 0.2 C.
df = data.frame(year=1960, T=0.2, D=NA, cbar=NA, u=NA)
## Loop over all years from 1960 to 2300
for (tt in 1961:2300) {
  ## Retrieve the CO2 concentration in year tt
  C1 = co2.ssp245$value[co2.ssp245$year == tt]
  ## Calculate the radiative foring
  F = (RR / log(2)) * log(C1 / C0)
  ## Update the warming level, using the last row of df for the previous year
  T = df$T[nrow(df)] + c * (F - (RR / ECS) * df$T[nrow(df)])
  ## Compute the damages
  D = alpha * T^2
  ## Compute social welfare
  cbar = (c.2020 - c.1960) * (tt - 1960) / (2020 - 1960) + c.1960
  cc = cbar * (1 - D / 100)
  uu = (cc^(1 - eta)) / (1 - eta)
  ## Add on the results for the new year to df
  df = rbind(df, data.frame(year=tt, T=T, D=D, cbar, u=uu))
}

