## Model Programming
## Library
install.packages("decisionSupport")
library(decisionSupport)
install.packages("DiagrammeR")
library(DiagrammeR)

## Plot the impact pathway
# Cory
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")
#LR = Left to right; TD = Top to down
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")
#change linkStyle and stroke width
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:black, stroke-width:2px
        M(Market price)-->I; linkStyle 1 stroke:black, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke:black, stroke-width:2px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:2px
        CM(Management cost)-->F; linkStyle 4 stroke: black, stroke-width:2px")
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1px")
mermaid("graph TB
        Y(Yield)-->I(Income); style I fill:green linkStyle 0 stroke:green, stroke-width:2px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2px
        CL(Labor cost)-->F; style CL fill:red
        linkStyle 3 stroke: red, stroke-width:2px
        CM(Management cost)-->F; style CM fill:red
        linkStyle 4 stroke: red, stroke-width:2px")

## Building the model
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost"),
                              lower = c(6000, 3, 500),
                              median = NA,
                              upper = c(14000, 8, 1000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season"))

input_estimates
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", 
                                              "Management costs in a normal season"))
input_estimates

# Daten einlesen
library(readr)
urfile = "https://raw.githubusercontent.com/CWWhitney/Decision_Analysis_Course/main/data/example_decision_inputs.csv"
input_estimates<-read_csv(url(urfile))
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the final results from the model
  final_result <- income - Labor_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}
# Run the Monte Carlo simulation
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")

example_mc_simulation
model_function <- function(){
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  overall_costs <- Labor_cost + Management_cost
  
  # Estimate the final results from the model
  final_result <- income - overall_costs
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")
example_mc_simulation

## Plotting
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
## Testing with make_variables
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}
make_variables(as.estimate(input_estimates))

Market_price
make_variables(as.estimate(input_estimates))

Labor_cost + Management_cost
