## Overview 
Data hosted on [open science framework](https://osf.io/3mhqb/)

Code for main analyses: analysis/script (analysis/functions/ for dependency custom functions)

behaviour_analysis.R: all analysis of ecological momentary assessment (self-report) and task data, including all manuscript figures

model-fit-cmdstanr.Rmd: fit hierarchical computational models in cmdstanr  

model-analysis.R: additional model checks and statistics after fitting 

fetch-data.ipynb: python example script to retrieve data from firebase if using the task

for hierarchical modelling 
analysis/stan_models & analysis/stan_fits

(you may need to change the source paths or re-fit models due to the way [cmstandr](https://mc-stan.org/cmdstanr/) works)

## Task 
Code for the task is in /public 

Mobile compatible version of a simple reward-effort decision-making task built using [phaser3](https://phaser.io/phaser3)

The task is a mobile optimised web-app hosted on Google's [Firebase](https://firebase.google.com/) web app hosting service, and saves data in a [Firestore](https://firebase.google.com/products/firestore) NoSQL database.  

Original created by [Dr Agnes Norbury](https://www.agnesnorbury.com/); [now published in Science Advances](https://www.science.org/doi/full/10.1126/sciadv.adk3222?af=R)

The game world was compiled using [Tiled](https://www.mapeditor.org/) using art assets by [kenney](https://kenney.nl/).

UI functionality was built using [rexUI plugins](https://rexrainbow.github.io/phaser3-rex-notes/docs/site/ui-overview/) for phaser3.

This version is set up to connect with Brain Explorer app in the front-end in an ema study

People were sent notifications which would prompt them to open the link and an in app browser would load the game. 

You can play a demo of the game [here](https://ema-motivation.web.app/) (on your mobile!)

questions: s.hewitt.17@ucl.ac.uk

## Useful functions for firebase CLI
[see firebase docs for more info](https://firebase.google.com/docs/build)
### examples: host game in local browser via emulators 
```
cd Documents/Github/reward-effort-2afc-firebase-BEX-FU
firebase emulators:start 
```

#### preview game version via a channel (online)
```
cd Documents/Github/reward-effort-2afc-firebase-BEX-FU
firebase hosting:channel:deploy new-awesome-feature --expires 7d
```

#### push the channel (version) to live project url: 
```
firebase hosting:clone ema-motivation:new-awesome-feature ema-motivation:live
```

#### delete a channel 
```
firebase hosting:channel:delete new-awesome-feature
```
 

### delete data in cloud firestore 
```
firebase firestore:delete /rew-eff-ema/study1 -r
```




