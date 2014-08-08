## Methods

### @worm2009rebuilding methods
Defined the exploitation rate for year $t$ $u_t$ as 

$$u_t = \frac{C_t}{B_t} = \frac{\text{catch in year }t}{\text{Biomass in year }t}$$

When $B_{msy}$ was unavailable, fit a surplus production model to time series of annual total biomass $B_t$ and total catch or landings $C_t$ from the assessments. The surplus production model was applied only where $\geq 20$ years of catch or landings and biomass data were available. 


#### Surplus production model
$P_t$ is the surplus production in year $t$

$$P_t = B_{t+1} - B_t + C_t$$
$$\text{Surplus production} = \text{next yr's biomass} - \text{this yr's biomass} - \text{this yr catch}$$

Fit a Schaefer surplus-production model, based on logisitic growth ot hte catch and biomass time series data. 

$$\hat{P}_t = \frac{4mB_t}{K}-4m\left(\frac{B_t}{K}\right)^2$$

where $m$ is MSY and $K$ is the carrying capacity. 

Estimate $m$ and $K$ using maximum likelihood in AD Model Builder assuming that the residuals were normally distributed ($\epsilon_t = P_t - \hat{P}_t$). 

For the Schaefer model, $B_{msy}$ is $\frac{K}{2}$, and the harvest rate to get MSY $u_{msy}$ is $\frac{m}{B_msy}$. Constrained the carrying capacity to be less than twice the maximum observed biomass. 

Then compared the surplus production model estimates of $\frac{B_{current}}{B_{msy}}$ and $\frac{u_{current}}{u_{MSY}}$ to the value of those ratios from the assessment stocks which had reference points. Replaced values of these ratios that were greater than 2 with values of 2.[^1] Then looked at the Pearson correlation between the harvest ratios from the surplus correlation for the biomass ratios. 

[^1]: Don't understand. Why??

### Definition of collapse
Compared the time series of total biomass $B$ to $B_{msy}$. The biomass was definied as collapsed if $B$ is less than 20% of $B_{msy}$. For a population growing according to a logistic growth function, this is equivalent to 10% of the carrying capacity. Used to find what proportion of stocks are collapsed in any year $t$ by LME. 

## @Martell2012A: estimating MSY from catch
Data required are

+ time series of removals 
+ prior ranges of $r$ and $K$
+ possible ranges of relative stokc sizes in the first and final years of the time series