## Effects of Environmental Drivers Across Ecological Scales

### Investigators (alphabetical listing)
- Allison Case - Endocrine lab manager, SEZARC
- Jeremy A. Collings - PhD candidate, University of Oregon
- Joey Krieger Lodge - PhD student, University of Colorado, Boulder
- Evald Maceno - PhD candidate, Univeristy of Puerto Rico, Río Piedras
- Sierra B. Perez - PhD candidate, Indiana University
- Bethany L. Williams - Postdoctoral fellow, University of Missouri, St. Louis

## Project Description

### Motivation
Climate change and other anthropogenic impacts are rapidly altering historic environmental conditions, with consequences across ecological scales. For example, at the individual level, reduced body size is suggested to be a “universal response to climate change” across organisms (Gardener et al., 2011); although body size reductions cannot always be assumed (Sheridan & Bickford, 2011). Similarly, across organisms and ecosystems, declining population sizes and diversity loss are well appreciated. However, global change impacts are typically evaluated at each ecological scale independently. Understanding how global change responses at one ecological level may mediate dynamics at another is essential for accurately predicting global change impacts. Functional traits (traits related to an organism's performance or fitness), like body size, provide a mechanistic link for evaluating how individual-level responses impact higher levels of organization (Violle et al., 2007; Villéger et al., 2017). 

Moreover, the effects of single global changes are generally studied independently, despite global changes occuring simultaneously. In fish ecology, for instance, warming effects have received far more attention than other climatic stressors, like hypoxia (i.e., low dissolved oxygen (DO)), and studies rarely link multiple climatic factors (Nagelkerken et al., 2023). As coupled global change factors may amplify ecological impacts, it is crucial consider them within the context of their co-occurrence. 

### Questions
We are leveraging long-term (5+ year) datasets across aquatic ecosystems in the LTER Network, NEON, and California's Interagency Ecological Program. These records directly capture changes in environmental drivers (temperature & DO), and fish body size, population abundances, and community properties, and we are using them to explore the following:

<ol type="1">
  <li>Across aquatic systems, how are global change factors (temperature & DO) impacting fish individuals, populations & communities? </li>
  <li>Are effects consistent/scaling across ecological levels of complexity (individual, population, community)? </li>
  <li>Does variation in responses at one ecological level (i.e., intra-/interspecific variation) explain responses at higher levels of organization? </li>
  <li>Do patterns differ across marine and freshwater ecosystems? </li>
</ol>
<p align="center">
<img src="conceptual fig.png" alt="Project conceptual figure" width="65%"/>
</p>

### Approach
To estimate the effects of temperature and DO on fish individuals, populations, and communities, we are fitting hierarchical linear models with one model per each of the following scaled response variables: body size (individual-level), standardized catch per species (population-level), species diversity (community-level), and standardized catch across species (community-level). Assuming a sufficiently weak correlation between temperature and DO, we will include both variables as fixed effects in each of our four models. Additionally, each model will include spatial random intercepts to account for other sources of meaningful environmental variation among sites. Our individual and population level models will also include random slopes for species to estimate interspecific variation in temperature and DO effects. Finally, each of these models will be fit with data from either freshwater or marine sites, resulting in eight models in total. 
<br>
<br>
We will use estimates of temperature and DO effects to evaluate Question 1. Comparisons between slope parameters across our individual, population, and community-level models will be used to evaluate Question 2. For Question 3, we will compare the variation in environmental driver effects at one ecological level with the magnitude of those effects at the next higher level. Finally, each of these heuristics will be performed for freshwater and marine systems, and a comparison of conclusions made across these two ecosystems will be used to evaluate Question 4. 

## Script Explanations

Intermediate_scripts folder: There is one R script per site that is used to download, combine, and harmonize the relevant fish and environmental dataset(s) from EDI. The 01_intermediate_harmonize script is then used to combine all the datasets from each site.


## Supplementary Resources

LTER Scientific Computing Team [website](https://lter.github.io/scicomp/) & NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)
