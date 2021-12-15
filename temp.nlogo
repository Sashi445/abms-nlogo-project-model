breed [posts post]

breed [users user]

breed [influencers influencer]

turtles-own [ current visited gender ]

posts-own [ inf-val infl-gender  infl-who ]

users-own [ approached start endl  happy ]

influencers-own [ influencer-val  created ]

undirected-link-breed [ irels irel  ]
undirected-link-breed [ urels urel  ]
undirected-link-breed [ iurels iurel ]

;; gender
;; 0 - male 1 - female

to setup
  clear-all

  ;; creating influences
  create-influencers max-influencers [
    set color orange
    set shape "person"
    set influencer-val random 40
    set size 2
    set gender random 2
    set created false
  ]

  ask patches [
    set pcolor white
  ]

  ;;creating users
  create-users total-users [
    set color blue
    set shape "person"
    set size 1.5
    set gender random 2
    set approached false
    set start [ ]
    set endl [ ]
    set happy false
    set-user-interests
  ]

  ;;creating network
  create-network

   ask irels[
    set color brown
    set thickness 0.4
  ]

  ask urels[
    set color black
    set thickness 0.2
  ]

  ask iurels [
    set color yellow
    set thickness 0.2
  ]

  ask turtles [
    set current false
    set visited false
  ]

  ;; pick an influener randomly and create post
  pick-influencer-and-post

  reset-ticks
end

to go

  ;;if ( ticks = 0  ) [ pick-influencer-and-post ]

  ;; checking whether any neighbours are present or not
  if ( ( count users with [ current = true ] ) = 0 ) [ stop ]

  ;; post posted by influencer will go to his neighbopurs and so on
  go-once

  tick

end

to go-once

  ask users with [ visited = false and current = true ] [

    let bias-flag false

    if ( bias-type = "random-bias" ) [

      set bias-flag random-bias

    ]

    if ( bias-type = "gender-bias" ) [

      set bias-flag gender-bias

    ]

    if ( bias-type = "influencer-bias" ) [

      set bias-flag influencer-bias

    ]

    set happy measure-happiness

    follow-or-unfollow-infl

    if-else ( bias-flag = true and happy = true ) [

      set color green

      ask urel-neighbors with [ visited = false ] [
        set current true
      ]

    ]
    [
      set color red
    ]

    set current false

    set visited true

  ]
end


;; follow and unfollow based on happiness

to follow-or-unfollow-infl

  let infl-who-val 0

  ask posts with [ current = true ] [
    set infl-who-val infl-who
  ]

  let is-neighbor member? influencer infl-who-val  iurel-neighbors

  if-else ( happy = true ) [

    if ( is-neighbor = false ) [
      create-iurel-with influencer infl-who-val [
        set color red
        set thickness 0.3
      ]
    ]

  ]
  [

    if ( is-neighbor = true ) [

      ask iurel who infl-who-val [
        die
      ]

    ]
  ]

end


;; bias methods

;; random bias
to-report random-bias

  let prob random 100

  if ( ( prob / 100 ) > random-bias-threshold ) [
    report true
  ]

  report false

end

;; gender-bias
to-report gender-bias
  let flag false

  let infl-gender-here 0

  ask posts with [ current = true ] [
    set infl-gender-here infl-gender
  ]

  if ( infl-gender-here != gender ) [
    set flag true
  ]

  report flag
end

;; influencer bias
to-report influencer-bias

  let flag false

  let infl-who-val 0

  ask posts with [ current = true ] [
    set infl-who-val infl-who
  ]

  let is-neighbor member? influencer infl-who-val iurel-neighbors

  if-else ( is-neighbor = true) [
    set flag true
  ]
  [
    if ( any? iurel-neighbors ) [
      ask one-of iurel-neighbors [

      let my-val influencer-val

      ask posts with [ current = true ] [

        if ( ( my-val < ( inf-val + 5 ) ) or ( my-val > ( inf-val - 5 ) ) ) [

          set flag true

        ]

      ]

    ]
    ]

  ]

  report flag

end

;; Happiness measure
to-report measure-happiness
  let flag false

  let u-start start

  let u-end endl

  ask posts with [ current = true ] [

    let results [ ]

    ( foreach u-start u-end [ [a b]  -> set results lput ( (a < inf-val) and (b > inf-val) ) results ] )

    let r-size length (filter [ i -> (i = true) ] results)

    if ( r-size  > 0 ) [
      set flag true
    ]

  ]
  report flag
end

;; picking influencer and creating post by that influencer
to pick-influencer-and-post
  let picked-infl-val 0
  let picked-infl-who 0
  let picked-infl-gen 1

  ask one-of influencers with [ (count irel-neighbors) > 1 ] [
    set picked-infl-val influencer-val
    set color violet
    set size size * 1.2
    set picked-infl-who who
    set picked-infl-gen gender
    set current false
    set visited true

    ask iurel-neighbors [
      set current  true
    ]

  ]

  create-posts 1 [
    set color green
    set shape "square"
    set size 2
    set inf-val picked-infl-val
    set infl-gender picked-infl-gen
    set current true
    set infl-who picked-infl-who
    move-to influencer picked-infl-who
  ]

end

;; creating network between users and influencers
to create-user-influencer-network
  ask users [
    let my-id who
    ask n-of (random 5) irels [
      ask one-of both-ends [
        create-iurel-with user my-id
      ]
    ]
  ]
end

;; creating network among influencer based on their interests
to create-influencer-network
  ask influencers [
    let my-val influencer-val
    let my-id who
    set created true
    ask other influencers with [ created = false ] [
      let dom-diff abs (my-val - influencer-val)
      if (dom-diff / 100) < 0.05 [
        create-irel-with influencer my-id
      ]
    ]
  ]
end

;; creating network among users based on their interests
to create-users-network

  let user-interests [ ]

  let user-who 0

  ask users [

    set user-interests start

    set user-who who

    let threshold random max-connections

    let another-users n-of threshold other users

    ask another-users [

      let similar check-interests user-interests

      let is-neighbor urel-neighbor? user user-who

      if ( similar = true and is-neighbor = false ) [

        create-urel-with user user-who

      ]

    ]
  ]


end

to-report check-interests [ user-interests ]

  let flag false

  let results [ ]

  ( foreach start user-interests [ [a b]  -> set results lput (abs ( (a + 5) - b) < 5 ) results ] )

  let trues reduce [ [ total-so-far  next-item ] -> ifelse-value ( next-item = true  ) [ total-so-far + 1 ] [ total-so-far ]  ] ( fput 0 results )

  if ( ( trues / ( length results ) ) > 0.5  ) [

    set flag true

  ]

  report flag

end


to create-network
  create-influencer-network
  create-users-network
  create-user-influencer-network
end

;; layout
to layout
  layout-spring (turtles with [ breed != posts ]) links 0.2 8 1
  display
end

;; assigning interests to the users
to set-user-interests

  let u-start random 5

    while [ u-start < 40  ] [

      set start lput u-start start

      set endl lput (u-start + 5) endl

      set u-start u-start + 10

    ]
end
@#$#@#$#@
GRAPHICS-WINDOW
217
48
688
520
-1
-1
14.030303030303031
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
77
57
140
90
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
745
55
917
88
max-influencers
max-influencers
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
746
104
918
137
total-users
total-users
0
1000
1000.0
1
1
NIL
HORIZONTAL

BUTTON
75
91
141
124
NIL
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
77
126
140
159
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
749
250
887
295
bias-type
bias-type
"random-bias" "gender-bias" "influencer-bias"
2

MONITOR
962
105
1095
150
total-likes-and-shares
count users with [ (visited = true) and (color = green)  ]
17
1
11

MONITOR
958
154
1101
199
happiness-percenntage
100 * (count users with [ visited = true and happy = true ]) / (count users with [visited = true])
2
1
11

SLIDER
749
201
921
234
random-bias-threshold
random-bias-threshold
0
1
0.2
0.1
1
NIL
HORIZONTAL

MONITOR
965
201
1099
246
like-share-percentage
100 * (count users with [ visited = true and color = green  ])/(count users with [ visited = true ])
2
1
11

PLOT
751
313
1091
521
Happiness-Like-Share-Plot
NIL
NIL
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"happiness" 1.0 1 -955883 true "" "let den (count users with [visited = true])\n\nif ( den = 0 )[\n    set den 1\n]\n\nplot 1000 * (count users with [ visited = true and happy = true ]) / den "
"pen-1" 1.0 1 -13840069 true "" "let den (count users with [visited = true])\n\nif (den  = 0 ) [\n     set den 1\n]\n\nplot (1000 * (count users with [ visited = true and color = green ]) / den )"

MONITOR
1026
55
1083
100
females
count turtles with [ gender = 1 ]
1
1
11

MONITOR
968
55
1025
100
males
count turtles with [ gender = 0 ]
1
1
11

SLIDER
747
152
919
185
max-connections
max-connections
0
total-users
50.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
# Overview

## Purpose

The purpose of this model is to know how different kinds of biases play a role in a user's decision of reacting to posts. And how these different kinds of biases make significant differences in getting reach for the posts shared by different influencers.The ultimate purpose of the model, which will be presented in follow-up work, is to explore different biases.

## Entity, State variables and scales

Model entities are users, influencers and posts. All state variables characterizing these entities are listed in the below image

![state variables of entites](https://res.cloudinary.com/dcjfrnxqn/image/upload/v1639563144/entity_variables_fqmamr.png)

### Global-variables

1. total-users - total no of users in the model

2. max-influencers - total no of influencers in the model

3. random-bias-threshold - value that lies between 0-1 if the values is greater than random-bias-threshold there is an high chance of user liking the post

4. bias-type - its a chooser that contains
    1. Random-bias
    2. Gender-bias
    3. Influencer-bias

5. max-connections - maximum number of friends an user can have.

### Scale

In this model one tick is equal to one level of graph traversal.

## Process overviewing and scheduling

![process](https://res.cloudinary.com/dcjfrnxqn/image/upload/c_scale,w_900/v1639557951/ABMS-2_5_abya7q.png)

### influencer creates post

Picking the influencers randomly those who has more than one neighboring influencers. That particular influencer post their content. The state variables of post get updated here

### Neighboring users and influencers

sharing the post to its neighbour users and influencers 

### measure-happiness

The value of happy depends on the users. Every user will have some set of interests whenever any post reached to the user depending on the infl-val of post comparing with user interests it return a flag that can be either true or false

### bias

Based on the bias-type that particular submodel get executed and returns a flag that can be either be true or false 

### like/dislike 

The decision of user to like or dislike the post is based on above 2 factors
1. Bias
2. Happiness

![like-dislike](https://res.cloudinary.com/dcjfrnxqn/image/upload/c_scale,w_900/v1639555856/ABMS-1_blyru6.png)

# Design

## Basic principles

In the current tenure of social media,liking or disliking a post is one's choice.and there will be some factors which influences the decision to like or dislike a post.Hence some bias like influencer bias ,gender bias and random bias comes into picture.The effect of these biases are spot on and cannot be neglected.

## Emergence

The count of post likes emerges according to bias choosed and percentage of likes and rate of happiness can be known.

## Adaptation

When the post reaches any user or influencer,they can like or dislike the post.It is assumed that if liked then the user/influencer will share it with their neighbors.and the behavior of user and influencer completely modeled with a set of rules and assumptions.

## Objectives

## Predictions

During the initialization of model i.e. creating users and influencers which are different breeds,users connect to influencers randomly.when we run the model an influencer posts the content it reaches to users based on that content posted by influencer, users can decide to follow or unfollow the influencer

## Sensing

## Interactions

## Stochasticity

## Observations

Results to be observed in this model
1. Count of likes and its percentage after every run indicates the response of different users to that post.
2. Statistical analysis to know the significance between biases.


# Details

## Initialization

Initially there are users, influencers and a post at tick = 0. Most of the values chosen for the initial state are arbitrarily taken. Initial values for users, influencers and post at the time of initialization 

1. Users
	1. Gender - randomly choosing their gender
	2. Current - false 
	3. Visited - false
	4. Approached - false
	5. Start - randomly choosing some starting ranges of the user interest
	6. Endl - based on starting ranges the end range is been designed
	7. Happy - false

2. Influencers
	1. Gender - randomly choosing their gender
	2. Current - false 
	3. Visited - false
	4. Influencer-val - random number between [0-40]

3. Post
	1. infl-who - id of influencer who posted the post 
	2. infl-gender - gender of influencer who posted the post
	3. inf-val - interest of influencer who posted the post

## Input data

The current model does not use input from external sources

## Submodels

### Random-bias

Randomly taking some value in between 0-100 and dividing with 100 and comparing with random-bias-threshold, if the value is greater than the random-bias-threshold the user has more chance of liking the post.

```
to-report random-bias
  let prob random 100
  if ( ( prob / 100 ) > random-bias-threshold ) [
    report true
  ]
  report false
end
```

### Gender-bias

In this bias, the liking of the post depends on gender which gender influencer posted the content. If the user gender is opposite to the gender of the influencer there is a chance of the user liking the post

```
to-report gender-bias
  let flag false
  let infl-gender-here 0
  ask posts with [ current = true ] [
    set infl-gender-here infl-gender
  ]
  if ( infl-gender-here != gender ) [
    set flag true
  ]
  report flag
end
```

### Influencer-bias

In this bias, we are checking whether the post posted by influencer is neighbour to the user or not if the influencer is neighbour to the user we are not doing anything, if the influencer is not the neighbour then we are checking the nearest influencer of the user and comparing the interests of that influencer and inf-val of influencer who posted the post if the value lies in some range then user likes the post or else the user will not like the post

```
to-report influencer-bias
  let flag false
  let infl-who-val 0
  ask posts with [ current = true ] [
    set infl-who-val infl-who
  ]
  let is-neighbor member? influencer infl-who-val iurel-neighbors
  if-else ( is-neighbor = true) [
    set flag true
  ]
  [
    if ( any? iurel-neighbors ) [
      ask one-of iurel-neighbors [
      let my-val influencer-val
      ask posts with [ current = true ] [
        if ( ( my-val < ( inf-val + 5 ) ) or ( my-val > ( inf-val - 5 ) ) ) [
          set flag true
        ]
      ]
    ]
    ]
  ]
  report flag
end
```

### Follow-Or-Unfollow

At initialization the users are linked to the influencers based on their interests. But when any influencer posted a content the users may or may not like the content depending on the user happiness the users can follow or unfollow the influencers

```
to follow-or-unfollow-infl
  let infl-who-val 0
  ask posts with [ current = true ] [
    set infl-who-val infl-who
  ]
  let is-neighbor member? influencer infl-who-val  iurel-neighbors
  if-else ( happy = true ) [
    if ( is-neighbor = false ) [
      create-iurel-with influencer infl-who-val [
        set color red
        set thickness 0.3
      ]
    ]
  ]
  [
    if ( is-neighbor = true ) [
      ask iurel who infl-who-val [
        die
      ]
    ]
  ]
end
```

### Measure-Happiness

Every user will have a state variable called happy, the value of happy depends on the users. Every user will have some set of interests whenever any post reached to the user depending on the type of post the value of happiness is been calculated.

```
to-report measure-happiness
  let flag false
  let u-start start
  let u-end endl
  ask posts with [ current = true ] [
    let results [ ]
    ( foreach u-start u-end [ [a b]  -> set results lput ( (a < inf-val) and (b > inf-val) ) results ] )
    let r-size length (filter [ i -> (i = true) ] results)
    if ( r-size  > 0 ) [
      set flag true
    ]
  ]
  report flag
end
```

## Experiment

Using the **BehaviorSpace tool**, we exported the results to a spreadsheet that contains like-share percentage for each bias type separately with the same characteristics.That includes number of users, number of repetitions and maximum influencers count. Considering this we have taken users of 500 and 15 influencer counts,100 repetitions were done and exported those as spreadsheets separately.

To infer that data,**ttest** have been done to test the significance between Random Bias-Gender bias and Random bias-Influencer Bias each pair of biases for Like percentage.

## Results

1. TTest between Random and Gender biases
	1. TTest for Gender and random biases using like percentage:0.0000000006623605061

2. TTest between Random and Influencer biases	
	1. TTest for Influence and random biases using like percentage:0.006565572736

## Citation:
1. Wilensky, U. (2005). NetLogo Preferential Attachment model. http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
2. Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL


@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="random-bias" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count users with [ color = green and visited = true ] / count turtles with [ visited = true ]</metric>
    <metric>count users with [ visited = true and happy = true ] / count turtles with [ visited = true ]</metric>
    <enumeratedValueSet variable="max-influencers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-bias-threshold">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-type">
      <value value="&quot;random-bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-users">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-connections">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="gender-bias" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count users with [ color = green and visited = true ] / count turtles with [ visited = true ]</metric>
    <metric>count users with [ visited = true and happy = true ] / count turtles with [ visited = true ]</metric>
    <enumeratedValueSet variable="max-influencers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-type">
      <value value="&quot;gender-bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-users">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-connections">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="influencer-bias" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count users with [ color = green and visited = true ] / count turtles with [ visited = true ]</metric>
    <metric>count users with [ visited = true and happy = true ] / count turtles with [ visited = true ]</metric>
    <enumeratedValueSet variable="max-influencers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-type">
      <value value="&quot;influencer-bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-users">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-connections">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="influencer-bias" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count users with [ color = green and visited = true ] / count turtles with [ visited = true ]</metric>
    <metric>count users with [ visited = true and happy = true ] / count turtles with [ visited = true ]</metric>
    <enumeratedValueSet variable="max-influencers">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-type">
      <value value="&quot;influencer-bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-users">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-connections">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
