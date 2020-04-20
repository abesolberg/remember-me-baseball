# Helpers

## Things to do:
  # Chaos Mode
  # Fix Players Getting Cut off
  # Toggle Front-Back
  # Background
  # Strip at Bottom (pending background)

library(tidyverse)

# Read Data ----

data <- read_rds('data.RDS')
types <- read_rds('type_hier.RDS')

## Scoreboard: Created by https://codepen.io/jakeculp/pen/vZWwYJ
# https://codepen.io/jakeculp/pen/vZWwYJ



css <- '
.count {
display:flex;
flex-direction: row;
font-size: 1.5rem;
padding-top: .5rem;
align-items:center;
}

#BALL {
padding-right: 1rem;
display: flex;
flex-direction: row;
align-items: center;
}

#STRIKE {
padding-right: 1rem;
display: flex;
flex-direction: row;
align-items: center;
}

#OUT {
padding-right: 1rem;
display: flex;
flex-direction: row;
align-items: center;
}

#BALL:after {
content: "●●●";
color: #184848;
}

#STRIKE:after {
content: "●●";
color: red;
}

#OUT:after {
content: "●●";
color: #184848;
}
'


