# Markov chain Monte Carlo demo

This Shiny app provides a demo of an MCMC sampler.
The app is deployed here:
https://giankdiluvi.shinyapps.io/assignment-b3-giankdiluvi/.

The MCMC sampler is a random walk targeting a fixed bivariate Normal target.

## Some *feaures* of the app

- Personalize the proposal distribution via its variance and the initial point;
this allows users to understand how these design choices
affect the convergence of the sampler.
- Visualize both qualitative and quantitative convergence assessments
in separate tabs;
this allows users to compare different common MCMC convergence assessments,
and having separate tabs makes visualization more tidy.
- Switch between scatter plots and kernel density estimates (KDE) of
the MCMC samples in the qualitative assessment;
useful to understand density convergence as the number of iterations grows.