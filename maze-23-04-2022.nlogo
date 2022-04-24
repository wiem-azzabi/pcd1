extensions [Nw matrix]

;; defining global variables
globals
[;;globals of maze
  ;steps
  sum-rewards-per-episode
  acc-reward
  tiles
  new-links-red
  reward-per-episode
  mean-epoch-error
  total-epoch-error
  episode
  ;;;;;globals of neural network
   data-list    ; List of pairs [Input Output] to train the network
    ;inputs       ; List with the binary inputs in the training
    ;outputs      ; List with the binary output in the training
    ;output-id   ; list of "who" of each output node
    ;epoch-error  ; error in every epoch during training
    ;genotype
  ;linput ;list of weights(input-hidden) after training
  ;lhidden ;list of weights(hidden-output) after training
  ;lbias ;list of weights(biais) after training
  mi ;compteur tebaa input-neurons taa maze runner
  linki ;compteur tebaa mr-network-links
]

;; defining breeds
breed [nodes node]
breed [builders builder]
breed [maze-runners mr]
maze-runners-own [prev-node current-node next-node next-path visited-nodes visited-nodes2
                  visited-hubs I-found-exit?
  ;;;network atributes
  ;data-list    ; List of pairs [Input Output] to train the network
    inputs       ; List with the binary inputs in the training
    outputs      ; List with the binary output in the training
    output-id   ; list of "who" of each output node
    epoch-error  ; error in every epoch during training

  linput ;list of weights(input-hidden) after training (kenet li w lezem tkoun list of list)
  lhidden ;list of weights(hidden-output) after training
  lbias ;list of weights(biais) after training
  ;;;;
  mr-bias-neurons
  mr-input-neurons ;liste
  mr-output-neurons
  mr-hidden-neurons
  mr-network-links ;;all links of a maze-runner's network
  ;;; new
  input-links ;list of input-hidden links ( mch les valeurs) (liste simple)
  hidden-links ;hidden-output
  bias-links ;bias-hidden + bias-output
  ;hidden-links2 ;hidden links list of lists
  ;input-links2 ;input links list of lists
  ;bias-links2 ;bias links list of lists
  oin
  oout
  steps

  genotype
  fitness
  dist-finale ;distance finale %exit
  nbre-visited-nodes ;nbre visited-nodes finale
]

;; defining breeds variable
nodes-own [node-id maze-entrance maze-exit exit? corner? reward distance-exit  ]
builders-own [stack]

patches-own [ preward pentrance  num-possible-actions possible-actions]
undirected-link-breed [liens lien]
;;;neural network breeds ;;
breed [bias-neurons bias-neuron]
bias-neurons-own [activation grad dropped?]

breed [input-neurons input-neuron]
input-neurons-own [activation grad dropped?]

breed [output-neurons output-neuron]
output-neurons-own [activation grad dropped? ]


breed [hidden-neurons hidden-neuron]
hidden-neurons-own [activation grad dropped? ]

;;;v0: links-own [weight] sans breed
directed-link-breed [network-links network-link]
network-links-own [weight]


;; all functions defined here

;; setup button
to quit
  clear-all
end
to setup
  clear-all
  ;setup-Reseau
  build-tiles
  init-nodes
  set episode 1
  build-maze
  set-entrance-exit

  setup-maze-runners ;;contient setup reseau et train reseau


    ask nodes with [label = "exit"][set reward 100 set plabel reward]
    ask nodes with [maze-exit = False and exit? = True  ][set reward -20 set plabel reward set color blue]
    ask nodes with [label = "entrance"][set reward -20 set plabel reward]
    ask patches [set preward 0]
  ask nodes with [label = "entrance"][set color green]

  ask nodes [
          let xn xcor
          let yn ycor
          let xexit [xcor] of nodes with [reward = 100]
          let yexit  [ycor] of nodes with [reward = 100]
          set xexit item 0 xexit
          set yexit item 0 yexit
          let dist sqrt( (xn - xexit) * (xn - xexit)  + (yn - yexit) * (yn - yexit) )
    set distance-exit dist
  ]

  ask patches [set preward [reward] of nodes-here  ]
  ask patches with [pcolor = 82] [set preward -200]

  ask patches [ set num-possible-actions  count patches at-points [[-2  0] [ 2  0 ] [0  -2] [0  2]] with [pcolor = 9.91]]
  ask patches [set possible-actions [-1 -1 -1 -1 ]]

  ask patches with [ pcolor = 9.91]  [if ( count patches at-points [[-2  0]]  with [pcolor = 9.91] = 1) [set possible-actions replace-item 0 possible-actions 270]]
  ask patches with [ pcolor = 9.91]  [if ( count patches at-points [[ 2  0]]  with [pcolor = 9.91] = 1) [set possible-actions replace-item 1 possible-actions 90]]
  ask patches with [ pcolor = 9.91]  [if ( count patches at-points [[ 0  2]]  with [pcolor = 9.91] = 1) [set possible-actions replace-item 2 possible-actions 0]]
  ask patches with [ pcolor = 9.91]  [if ( count patches at-points [[ 0 -2]]  with [pcolor = 9.91] = 1) [set possible-actions replace-item 3 possible-actions 180]]



  ask patches with[ pcolor = 9.91]  [ask patches at-points [[1 0]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[-1 0]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[0 -1]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[0 1]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[1 0]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[-1 0]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[0 -1]] [set pcolor  9.91]]
  ask patches with[ pcolor = 9.91]  [ask patches at-points [[0 1]] [set pcolor  9.91]]


;;0 up
;;90 ymin
;; 180down
;;270 ysar
 ;; (90 180 270 0)

   clear-all-plots

  reset-ticks
end

;; use in order to run simulation on the same maze several times
to reset-maze-runners
  reset-ticks
  set episode 1
  ask links [set color black set thickness 0]
  ask maze-runners [die]
  setup-maze-runners
  clear-all-plots
end


;; build orderd white tiles in the world
;; according to the spacing (their distance)
to build-tiles
  ask patches [set pcolor 82]
  set tiles patches with
  [ pxcor mod spacing = 0
    and pycor mod spacing = 0
    and abs pxcor +  spacing < max-pxcor
    and abs pycor +  spacing < max-pycor
    and abs pxcor -  spacing > min-pxcor
    and abs pycor - spacing > min-pycor
  ]
  ask tiles [ set pcolor white ]
  set new-links-red 0
end

;; Init nodes of given color,size and shape on each nodes
;; All boolear variables are set false
to init-nodes
  let index 1
  ask tiles
  [
    sprout-nodes 1
    [
       set color black
       set size 1
       set shape "circle"
       set node-id index
       set exit? false
       set maze-entrance false
       set maze-exit false
       set corner? false
     ]
     set index index + 1
  ]
  ask nodes [set reward -1 set plabel reward ]

end



;; Build maze
to build-maze

  create-builders  1
  [ ;; choose a random starting point
    let start one-of tiles
    set xcor [pxcor] of start
    set ycor [pycor] of start

    ;; set heading and color
    set heading 0
    set color blue
    ask patches in-radius 1 [ set pcolor [color] of myself ]
    set stack []
  ]
  ask builders
  [ ;; store starting point
    set stack fput ( list xcor ycor ) stack
    while [ length stack > 0 ]
    [ ;; in this while the maze building process
      let target 0
      let left-right 0
      let straight 0
      let running 0
      let paths find-open-paths
      ifelse any? paths
      [ ;; ifelse any? paths --> paths is not-empty
        set straight patch-ahead spacing
        set left-right paths with [ self != straight ]
        let nd 0
        if (any? nodes-on patch-here)
        [ ask one-of nodes-on patch-here [set nd self] ]
        ifelse (any? left-right ) or not is-open straight
        [
          set target one-of left-right
          ;; record stack
          set stack fput ( list xcor ycor ) stack
          set heading towards target
          draw-move
        ]
        [
          set running true
          while [ running ]
          [
            set heading towards straight
            draw-move
            set straight patch-at ( dx * spacing) ( dy * spacing )
            set running ( random-float 1.0 >= 1 and is-open straight )
          ]
        ]
        if (any? nodes-on patch-here)
        [ask one-of nodes-on patch-here
          [create-lien-with nd [set color black]]]

       ]
      [ ;; ifelse any? paths --> path is empty
        ifelse length stack > 0
        [ ;; start the building process
          setxy (item 0 (item 0 stack)) (item 1 (item 0 stack))
          ;; removing first element from stack
          set stack but-first stack
         ]
         [ stop ]
    ]
 ]
    let i  0
    while[ i < 7 ]
    [let start one-of nodes
      set xcor [pxcor] of start
      set ycor [pycor] of start
      set heading 0
    ask patches in-radius 1 [ set pcolor [color] of myself ]
    let fin one-of nodes with [(pxcor = [pxcor ]of start and pycor =[pycor] of start + spacing  ) or (pxcor = [pxcor ]of start and pycor =[pycor] of start - spacing  ) or
   (pxcor = [pxcor ]of start + spacing  and pycor =[pycor] of start ) or (pxcor = [pxcor ]of start - spacing and pycor =[pycor] of start   )]
    ask  start [create-lien-with fin [set color black]]
    let straight patches with [pxcor = [xcor] of fin  and pycor = [ycor] of fin  ]

    let running true
          while [running]
          [
            set heading towards fin
            draw-move2
            set straight patch-at ( dx * spacing) ( dy * spacing )
            set running ( random-float 1.0 >= 1 and is-open straight )
    ] set i i + 1]
    ;;close while
    die
  ];; close ask builders
end

;; draw move
to draw-move
  let start-spot patch-here
  ask start-spot [ ask patches in-radius 1  [ set pcolor 9.91 ] ]
  repeat spacing [ ask patches in-radius 1 [ set pcolor 9.91 ] jump 1  ]
 end
to draw-move2
  let start-spot patch-here
  ask start-spot [ ask patches in-radius 1 [ set pcolor 9.91 ] ]
  repeat spacing + 1  [ ask patches in-radius 1 [ set pcolor 9.91 ] jump 1 ]
 end

;;;;;;;;;;;;;;;;;;;;;;;;line to keep code in 80 columns;;;;;;;;;;;;;;;;;;;;;;;;

;; find maze entrance and exit
to set-entrance-exit

  let set-nodes-exit false
  let minx min [xcor] of nodes
  let miny min [ycor] of nodes
  let maxx max [xcor] of nodes
  let maxy max [ycor] of nodes
  let edge-nodes nodes with [
    pxcor = minx or pxcor = maxx or pycor = miny or pycor = maxy ]
  ask edge-nodes
  [
    ;set color black
    if (pxcor = minx and pycor = miny) [set corner? true]
    if (pxcor = minx and pycor = maxy) [set corner? true]
    if (pxcor = maxx and pycor = miny) [set corner? true]
    if (pxcor = maxx and pycor = maxy) [set corner? true]
  ]


  ask nodes
  [
    let exit-found? false
    ask patch-here
    [
       if (count neighbors with [pcolor = 82 ] = 5 ) [set exit-found? true]
       if (count neighbors with [pcolor = 82 ] = 2
        and count neighbors with [pcolor = 9.91] = 6 ) [
        set exit-found? true]
    ]
    if exit-found? = true  [set color black set size 2 set exit? true]
  ]
  let minx-exit min [xcor] of nodes with [exit? = true]
  let miny-exit min [ycor] of nodes with [exit? = true]
  let maxx-exit max [xcor] of nodes with [exit? = true]
  let maxy-exit max [ycor] of nodes with [exit? = true]
  ;; let's define two possible exit, one in the edge, the other even in the middle
  let edge-inout-nodes edge-nodes with [exit? = true]
  let inout-nodes nodes with [exit? = true]
  let possible-entrance one-of edge-inout-nodes

  if possible-entrance = nobody
  [ while [possible-entrance = nobody]
    [ set possible-entrance one-of inout-nodes]
  ]
  ask possible-entrance
  [ set maze-entrance true
    set label-color black
    set label "entrance"
    ask patch-here [set pentrance "entrance"]
    set color green
    set size 3

    (ifelse
    pxcor = minx-exit
    [
      if debug >= 1 [print "pxcor = minx-exit"]
      let possible-exit one-of edge-inout-nodes with [pxcor = maxx-exit]
      ifelse possible-exit != nobody
        [
          ask possible-exit
          [
            set maze-exit true set color red set size 3
            set label-color black set label "exit"
          ]
        ]
        [
          set possible-exit one-of inout-nodes with [label != "entrance"]
          ifelse possible-exit != nobody
          [
            ask possible-exit
            [
              set maze-exit true set color red set size 3
              set label-color black set label "exit"
            ]
          ]
          [
            print "Unable to find and entrance"
            print "Check spacing or other parameters"
          ]
        ]
      ]

    pxcor = maxx-exit
    [
        if debug >= 1 [print "pxcor = maxx-exit"]
        let possible-exit one-of edge-inout-nodes with [pxcor = minx-exit]
      ifelse possible-exit != nobody
        [ ask possible-exit
          [ set maze-exit true set color red set size 3
              set label-color black set label "exit"
          ]
        ]
        [ set possible-exit one-of inout-nodes with [label != "entrance"]
          ifelse possible-exit != nobody
          [
            ask possible-exit
            [
              set maze-exit true set color red set size 3
              set label-color black set label "exit"
            ]
          ]
          [
            print "Unable to find and entrance"
            print "Check spacing or other parameters"
          ]
        ]
      ]

    pycor = miny-exit
    [
        if debug >= 1 [print "pycor = miny-exit"]
        let possible-exit one-of edge-inout-nodes with [pycor = maxy-exit]
      ifelse possible-exit != nobody
        [
          ask possible-exit
          [
            set maze-exit true set color red set size 3
            set label-color black set label "exit"
          ]
        ]
        [ set possible-exit one-of inout-nodes with [label != "entrance"]
          ifelse possible-exit != nobody
          [
            ask possible-exit
            [
              set maze-exit true set color red set size 3
              set label-color black set label "exit"
            ]
          ]
          [
            print "Unable to find an entrance"
            print "Check spacing or other parameters"
          ]
        ]
      ]

    pycor = maxy-exit
    [
      if debug >= 1 [print "pycor = maxy-exit"]
      let possible-exit one-of edge-inout-nodes with [pycor = miny-exit]
      ifelse possible-exit != nobody
        [
          ask possible-exit
          [
            set maze-exit true set color red set size 3
            set label-color black set label "exit"
          ]
        ]
        [
          set possible-exit one-of inout-nodes with [label != "entrance"]
          ifelse possible-exit != nobody
          [
            ask possible-exit
            [
              set maze-exit true set color red set size 3
              set label-color black set label "exit"
            ]
          ]
          [ print "Unable to find and entrance"
            print "Check spacing or other parameters"
          ]
        ]
      ]
    )


  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;line to keep code in 80 columns;;;;;;;;;;;;;;;;;;;;;;;;

;; setup maze runners
to setup-maze-runners
  ask one-of nodes with [label = "entrance"]
  [ let present-node self
    ask patch-here
    [ sprout-maze-runners population
      [ set size 10
        set color yellow - 1
        set current-node present-node
        set visited-nodes []
        set visited-nodes2 []
        set visited-hubs []
        set I-found-exit? false
        setup-Reseau

        ;set heading 0
      ]
    ]
    ;setup-Reseau
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;line to keep code in 80 columns;;;;;;;;;;;;;;;;;;;;;;;;

to find-exit

  ask maze-runners
  [
    set visited-nodes lput current-node visited-nodes
    ;;
    ifelse [label] of current-node = "entrance"
    [ ;; current-node is entrance
      if debug >= 1 [print "current node is entrance"]
      set next-path one-of [my-links] of current-node
      ifelse [color] of next-path = black
      [ ;;next path black
        if debug >= 1 [print "next path is black"]
        ifelse current-node = [end1] of next-path
        [set next-node [end2] of next-path] [set next-node [end1] of next-path]
        color-link-green
        forward-maze-runner
      ]
      [ ;;next path NOT black
        print "next black not black at entrance not defined"
      ]
    ]
    [ ;; current-node is NOT entrance
;      if debug >= 1 [print "current node is NOT entrance"]
      ifelse [exit?] of current-node = true
      [ ;; current node is a blind spot, could be an exit
        if debug >= 1 [print "current node is a blind spot"]
        ifelse [maze-exit] of current-node = true
        [ ;; exit found
          set I-found-exit? true
          if [color] of next-path != green
          [color-best-path]
          if debug >= 1 [print "exit found"]
        ]
        [ ;;exit NOT found
          color-link-red
          go-back
        ]
      ]
      [ ;; current node is NOT a blind spot
;        if debug >= 1 [print "current node NOT is entrance"]
        ifelse [color] of link [who] of prev-node
                               [who] of current-node = green
        [ ;; previous path is green
          if debug >= 1 [print "previous path is green"]
          ifelse count [my-links] of current-node > 2
          [ ;; node is a hub
            if debug >= 1 [print "node is a hub"]
            found-new-hub
            set next-path search-link green
            ifelse next-path != nobody
            [ ;; next path is green
              if debug >= 1 [print "next path is green"]
              ifelse current-node = [end1] of next-path
              [set next-node [end2] of next-path]
              [set next-node [end1] of next-path]
              forward-maze-runner
;;;;;;;;;;;;;;;;;;;;;;;;line to keep code in 80 columns;;;;;;;;;;;;;;;;;;;;;;;;
            ]
            [discover-unknown-hub]
          ]
          [ ;; node is NOT a hub
            set next-path search-link black
              ifelse next-path != nobody
              [ ;; next path is black
              if debug >= 1 [print "next-path is black"]
                ifelse current-node = [end1] of next-path
                 [ set next-node [end2] of next-path ]
                 [ set next-node [end1] of next-path ]
                color-link-green
                forward-maze-runner
             ]
            [print "next-path not black after green not defined"]
          ]
        ]
        [ ;; previous path is NOT green
          ifelse [color] of link [who] of prev-node [who] of current-node = yellow
          [ ;; previous path is yellow
            if debug >= 1 [print "prev-path is yellow"]
            ifelse count [my-links] of current-node > 2
            [ ;; node is a hub
              if debug >= 1 [print "node is hub"]
              found-new-hub
              discover-unknown-hub
            ]
            [ ;; node is NOT a hub
              set next-path search-link black
              ifelse next-path != nobody
              [ ;; next path is black
              ifelse current-node = [end1] of next-path
                [set next-node [end2] of next-path]
                [set next-node [end1] of next-path]
              color-link-yellow
              forward-maze-runner
              ]
              [ ;;next path is NOT black
                print "previous yellow next not black not defined"
              ]
            ]
          ]
          [print "previous path is not green and yellow not defines"]
        ]
    ]
   ]
  ]
;  tick
  ifelse new-links-red != 0
  [tick-advance new-links-red set new-links-red 0]
  [ if not mr-found-exit? [tick] ]
  if debug >= 1 [print ticks]
  if mr-found-exit? [stop]

end

;;;;;;;;;;;;;;;;;;;;;;;;line to keep code in 80 columns;;;;;;;;;;;;;;;;;;;;;;;;

to forward-maze-runner
  if debug >= 1 [print "forward"]

  fd [link-length] of link [who] of current-node [who] of next-node
  set prev-node current-node
  set current-node next-node
end

to go-back
  if debug >= 1 [print "go-back"]
  set current-node last visited-nodes
  set visited-nodes remove current-node visited-nodes
  set visited-hubs remove current-node visited-hubs
  set prev-node last visited-nodes
  set xcor [xcor] of current-node
  set ycor [ycor] of current-node
  set next-path link [who] of prev-node [who] of current-node
  set heading report-mr-direction + 180
end

to color-link-green
  ask lien first [who] of current-node first [who] of next-node
    [set color red set thickness 1]
end

to color-link-yellow
  ask lien [who] of current-node [who] of next-node [set color yellow]
end


to color-link-red
  let last-node last visited-nodes
  let before-last-node item (length visited-nodes - 2) visited-nodes
  if last-node = last visited-hubs
  [ ;;this happens when mr is in a hub and all branch are red
    ;;in order to go back we need to remove the last visited-hubs
    set visited-hubs remove last visited-hubs visited-hubs
  ]
  if debug >= 2
  [
    print "color-link-red"
    print "last visited hub"
    print last visited-hubs
    print "last-node in visited-nodes"
    print last-node
    print "before-last-node in visited-nodes"
    print before-last-node
    print "link color red:"
  ]
  while [last-node != last visited-hubs]
  [
    ask lien [who] of last-node [who] of before-last-node [set color red]
    ;if debug >= 2 [print link [who] of last-node [who] of before-last-node]
    set visited-nodes remove last-node visited-nodes
    set last-node last visited-nodes
    set before-last-node
      item (position last-node visited-nodes - 1) visited-nodes
    set new-links-red new-links-red + 1
  ]
end

;to go-up
 ; ask maze-runners [set heading 90]
;end

to color-best-path
  let last-node last visited-nodes
  let before-last-node item (length visited-nodes - 2) visited-nodes
  if debug >= 2
  [
    print "color-best-path"
    print "last visited hub"
    print last visited-hubs
    print "last-node in visited-nodes"
    print last-node
    print "before-last-node in visited-nodes"
    print before-last-node
    print "link color green:"
  ]
  while [last-node != first visited-hubs]
  [
    ask link [who] of last-node [who] of before-last-node
    [set color green set thickness 1]
    if debug >= 2 [print link [who] of last-node [who] of before-last-node]
    set visited-nodes remove last-node visited-nodes
    set last-node last visited-nodes
    set before-last-node
      item (position last-node visited-nodes - 1) visited-nodes
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;line to keep code in 80 columns;;;;;;;;;;;;;;;;;;;;;;;;

to discover-unknown-hub
  if debug >= 1 [print "discovery new hub"]
  set next-path search-link green
  ifelse next-path != nobody
  [ ;;next path is green
    if debug >= 1 [print "next-path is green"]
    forward-maze-runner
  ]
  [ ;; next path is NOT green
    set next-path search-link black
    ifelse next-path != nobody
    [ ;; next path is black
      if debug >= 1 [print "next path is black"]
      ifelse found-best-path?
      [ ;;one next-path is red and all others red
        if debug >= 1 [print "one next-path is black and all others red"]
        set visited-hubs remove last visited-hubs visited-hubs
        ifelse current-node = [end1] of next-path
        [set next-node [end2] of next-path][set next-node [end1] of next-path]
        color-link-green
        forward-maze-runner
      ]
      [ ;; more next-path black
        ifelse current-node = [end1] of next-path
        [set next-node [end2] of next-path][set next-node [end1] of next-path]
        color-link-yellow
        forward-maze-runner
      ]
    ]
    [ ;; there are not next path black
      set next-path search-link yellow
      ifelse next-path != nobody
      [ ;; next-path is yellow
        if debug >= 1 [print "next-path is yellow"]
        forward-maze-runner
      ]
      [ ;; there are not next path yellow
        set next-path search-link red
        ifelse next-path != nobody
        [ ;; next-path is red
          if debug >= 1 [print "there are only red path"]
          color-link-red
          go-back
        ]
        [ ;; next-path is NOT red]
          print "Error, this scenario should not happen"
        ]
      ]
    ]
  ]
end

to found-new-hub
  if debug >= 1 [print "found new hub"]
  set visited-hubs lput current-node visited-hubs
end

;; all to-report functions defined here

;; find open path
to-report find-open-paths
  let paths
  ( patches at-points
    (map [ [a b] -> ;;procedure anonyme
      ( list(a * spacing ) (b * spacing) ) ] [ 0 0 1 -1 ] [1 -1 0 0 ])
   ) with [ pcolor = white ]
  report paths
end

;; check if path is open
to-report is-open
  [ a-patch ]
   report ([pcolor] of a-patch = white)
end

to-report report-mr-direction ;; trajaa direction li supposée yemchi feha
  let lh 45
  ifelse current-node = [end1] of next-path
  [set lh [link-heading] of next-path]
  [set lh [link-heading] of next-path + 180]
  report lh

end

to-report search-link [link-color]
  let new-link nobody
  let temp-prev-node prev-node
  ask current-node
  [ set new-link one-of
    (my-links with [color = link-color and other-end != temp-prev-node])
  ]
  report new-link
end

to-report found-best-path?
  if debug >= 1 [print "search for best bath"]
  let temp-prev-node prev-node
  let count-prev-path-green 0
  let count-next-path-black 0
  let count-next-path-red 0
  let total-path 0
  ask current-node
  [
    set count-prev-path-green count my-links with [color = green]
    set count-next-path-black count my-links with [other-end != temp-prev-node and color = black]
    set count-next-path-red count my-links with [other-end != temp-prev-node and color = red]
    set total-path count my-links
  ]
  if debug >= 2
  [
    print "count-prev-path-green"
    show count-prev-path-green
    print "count-next-path-black"
    show count-next-path-black
    print "count-next-path-red"
    show count-next-path-red
    print "total links"
    show total-path
  ]
  ifelse (count-next-path-black = 1) and
         (total-path = count-prev-path-green +
                            count-next-path-black + count-next-path-red )
  [ report true ][report false]
end

to-report mr-found-exit?
  let a-mr-found-exit? false
  let mr-on-exit one-of maze-runners with [I-found-exit? = true]
  if mr-on-exit != nobody [set a-mr-found-exit? true]
  report a-mr-found-exit?
end



to navigate
  set-current-plot "steps-per-episode"
  set-plot-pen-color white
  set episode num-episodes   ;; boucle marra bark qqsoit num-episodes
  plot-pen-down
  let xx [pxcor] of patches with [pentrance = "entrance"]
  let yy  [pycor] of patches with [pentrance = "entrance"]
  ;show(sort maze-runners)
  ask maze-runners [
    show(sort maze-runners)
    ;while [episode <= num-episodes]
  ;[
    ;;change link weights a partir du genotype
  let li-flat reduce sentence linput
  let lh-flat reduce sentence lhidden
  connect2 sort mr-input-neurons sort mr-hidden-neurons li-flat
  connect2 sort mr-hidden-neurons sort mr-output-neurons  lh-flat
  connect2 sort mr-bias-neurons   sort mr-hidden-neurons  item 0 lbias
  connect2 sort mr-bias-neurons sort mr-output-neurons  item 1 lbias
    set steps 0
    ask links  [set color black set thickness 0 ]
    ;ask maze-runners [setxy  item 0 xx item 0 yy ]
    setxy  item 0 xx item 0 yy
    set visited-nodes2 []
    pen-down
    ;ask maze-runners [ while[  [preward] of patch-here != [100] ] ;; when episode ends ;;nbadloha algo yeqef waqt steps tousel aadad mou3ayen
    ;ask maze-runners [ while [ steps != max-steps and [preward] of patch-here != [100]]
   while [ steps != max-steps and [preward] of patch-here != [100]]
        [ set current-node nodes-on patch-here

            ifelse (member? [who] of current-node visited-nodes2) [] [ set visited-nodes2 lput [who] of current-node visited-nodes2] ;;visited-nodes2 contient des noeuds distincts

      plotxy episode steps
    set-plot-pen-color red
    set episode episode + 1
          ;;distance %exit ;;
          let xn xcor ;;x du maze runner
          let yn ycor ;; y du maze runner
          let xexit [xcor] of nodes with [reward = 100]
          let yexit  [ycor] of nodes with [reward = 100]
          set xexit item 0 xexit
          set yexit item 0 yexit
          set dist-finale sqrt( (xn - xexit) * (xn - xexit)  + (yn - yexit) * (yn - yexit) )
          let dirp 0
          let dir -1
            ;ask maze-runners[
            ; convertir possible-actions en format input res neuro
            let input []
            let vl item 0 nature-voisins
            let dl calcul-distance 270
            let vr item 1 nature-voisins
            let dr calcul-distance 90
            let vu item 2 nature-voisins
            let du calcul-distance 0
            let vd item 3 nature-voisins
            let dd calcul-distance 180
            set input lput vl input
            set input lput dl input
            set input lput vr input
            set input lput dr input
            set input lput vu input
            set input lput du input
            set input lput vd input
            set input lput dd input

            ;;;;active-inputs
            (foreach (mr-input-neurons) input [ [n x] -> ask n [set activation x]])
            ;;;;result
            Forward-Propagation ;[genotype] ;;a partir du génotype

            ;;; inspect activations of output +  dégager dir a partir de index-direction :

            let i index-direction
            ifelse (item i possible-actions  != -1 )
            [if i = 0 [set dir 270]
            if i = 1 [set dir 90]
            if i = 2 [set dir 0]
            if i = 3 [set dir 180]
           set heading dir ] ;;ken reseau aatani direction valide

           [while [dir = -1 ][set dir one-of possible-actions]   ;;ch while dir = -1
            set heading dir ] ;;ken output yaati output invalide naatihh ena direction

           set next-node nodes-on patch-ahead spacing

           ;ask maze-runners [ fd spacing ]
           fd spacing
           ;color-link-green
           set steps steps + 1

        ]
show("visited") show(visited-nodes2)
  pen-erase
    ;]while num-ep
 plot-pen-up]
  ;;nchouf enehou maze runner aandou visited nodes = max


tick
;update-plots
end

to connect2 [neurons1 neurons2 liste]
  show("neurons1 connect2")
  show(neurons1)
  show(neurons2)
  show(liste)
  ;ask neurons1 [create-network-links-to neurons2 [  set weight random-float 0.2 - 0.1 hide-link]] ;kenet create-links-to
let f 0
  (foreach neurons1 [ n1 ->
    (foreach neurons2 [ n2 -> ask network-link [who] of n1 [who] of n2 [ set weight item f liste hide-link set f f + 1  ]
     ] )
    ]
  )

end

to add-exit
  let inout-nodes nodes with [reward = -20]
  let po-exit one-of inout-nodes
  ask po-exit
          [
            set reward 100
            set maze-exit true set color red set size 5
            set label-color black set label "exit"
          ]
  ask patches [set preward [reward] of nodes-here  ]
  ask patches with [pcolor = 82] [set preward -200]
end


to-report calcul-distance [d]  ;ditance entre noeud voisin (situé dans la direction d ) et noeud exit
  let dist 0
  ;ask maze-runners[
      set current-node nodes-on patch-here
      set heading d

      set next-node nodes-on patch-ahead spacing

      let x [xcor] of next-node
      let y [ycor] of next-node

      let xexit [xcor] of nodes with [reward = 100]
      let yexit [ycor] of nodes with [reward = 100]
      set xexit item 0 xexit
      set yexit item 0 yexit

      ifelse (x != [] and y != [] and ([pcolor] of patch-ahead (spacing / 2 ) != 82 ))
      [set x item 0 x
      set y item 0 y
      set dist  (sqrt( (x - xexit) * (x - xexit) + (y - yexit ) * ( y - yexit)))]
    [set dist 0]
  ;] ask maze-runners
    report dist
end

to-report nature-voisins
  ;; retourne liste binaire [i,j,k,l]
  ;;tel que
  ;;i=1 si noeud gauche est impasse
  ;;j=1 si noeud droite est impasse
  ;;k=1 si noeud up est impasse
  ;;l=1 si noeud down est impasse
  ;; -1 si ce noeud n'existe pas
  let L []
  ;ask maze-runners[
    set current-node nodes-on patch-here

    let direction heading

    ;;gauche
    set heading 270
    set next-node nodes-on patch-ahead spacing
    ifelse ([pcolor] of patch-ahead (spacing / 2 ) != 82 )
    [let a [exit?] of next-node
      ifelse (a = [true]) [set L lput 1 L][set L lput 0 L]
      ;show (a )
    ]
    [ set L lput -1 L ]
   ;;droite
    set heading 90
    set next-node nodes-on patch-ahead spacing
    ifelse [pcolor] of patch-ahead (spacing / 2 ) != 82
    [let a [exit?] of next-node
      ifelse (a = [true]) [set L lput 1 L][set L lput 0 L]
      ;show (a )
    ]
    [ set L lput -1 L ]
    ;;up
     set heading 0
    set next-node nodes-on patch-ahead spacing
    ifelse [pcolor] of patch-ahead (spacing / 2 ) != 82
    [let a [exit?] of next-node
      ifelse (a = [true]) [set L lput 1 L][set L lput 0 L]
     ; show (a )

    ]
    [ set L lput -1 L ]
    ;;down
     set heading 180
    set next-node nodes-on patch-ahead spacing
    ifelse [pcolor] of patch-ahead (spacing / 2 ) != 82
    [let a [exit?] of next-node
      ifelse (a = [true]) [set L lput 1 L][set L lput 0 L]
      ;show (a )
    ]
    [ set L lput -1 L ]
    set heading direction
  ;] ask maze-runners
  report L
  ;;
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;code du reseau de neuronnes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to setup-Reseau
  ;clear-all
  ; Building the network

 ; ask maze-runners [

  setup-neurons

  set linput []
  set lhidden []
  set lbias []
  setup-links
    ;;show agent show links
    ;show(who)

  ; Initializing global variables
  set epoch-error 0
  set data-list []
  set inputs []
  set outputs []
  create-samples
  let l1 []
  let l2 []
  let l3 []
  set l1 liste_input
  set l2 liste_output
  set l3 data-list
  ;show(l1)
  ;show(l2)
  ;show(l3)
  ;show(sort(input-neurons))
  ;show(sort(hidden-neurons))
  ;show(sort(output-neurons))
  ; Reset timer
  ;reset-ticks
  ;]
  ;reset-ticks
  train
end

; Auxiliary Procedure to setup neurons
to setup-neurons
  ;ask maze-runners[
  show("setting up neural network")
  let id []  ; list of "who" of each output node
  ; Create Input neurons
  repeat Neurons-Input-Layer [
    hatch-input-neurons 1 [
      set activation random-normal 0 0.1
      hide-turtle
       ]
    ;set mr-input-neurons []
    ;set l1 "sublist sort(input-neurons)  i*8 -> fin (i+1 * 8)
    ;set mr-input-neurons lput (sort input-neurons ) mr-input-neurons
      ;show(mr-input-neurons)
    ]
    repeat Neurons-Hidden-Layer [
      ;hatch-hidden-neurons 1 [
       hatch-hidden-neurons 1 [
      set activation random-normal 0 0.1
      set dropped? false
      hide-turtle

    ]   ]
  ; Create Output neurons
  repeat Neurons-Output-Layer [
    hatch-output-neurons 1 [
      set activation random-normal 0 0.1
      set id lput who id
      hide-turtle

  ]]
  ; Create Bias Neurons
  hatch-bias-neurons 1
  ask bias-neurons [ set activation 1
                    hide-turtle]
  let liste []
  set liste sublist sort(input-neurons) (8 * mi) (8 * (mi + 1))
  set mr-input-neurons liste
  set liste sublist sort(hidden-neurons) (Neurons-Hidden-Layer * mi) (Neurons-Hidden-Layer * (mi + 1))
  set mr-hidden-neurons liste
  set liste sublist sort(output-neurons) (4 * mi) (4 * (mi + 1))
  set mr-output-neurons liste
  set liste sublist sort(bias-neurons) (1 * mi) (1 * (mi + 1))
  set mr-bias-neurons liste
  set mi (mi + 1 )
  set output-id id
  ;]
end

; Auxiliary Procedure to create connections between neurons
to setup-links
  connect mr-input-neurons mr-hidden-neurons
  connect mr-hidden-neurons mr-output-neurons
  connect mr-bias-neurons   mr-hidden-neurons
  connect mr-bias-neurons mr-output-neurons

  set input-links sublist sort network-links 0 (8 * neurons-hidden-layer) ;list of input links (liste simple )
  set hidden-links sublist sort network-links (8 * neurons-hidden-layer) (12 * neurons-hidden-layer) ;list of hidden links vers output (liste simple)
  set bias-links sublist sort network-links (12 * neurons-hidden-layer) (length sort network-links)

  ;;set lhidden (liste de listes)
  let si 0
  let hidden-links2 [] ;liste de listes des liens
  repeat Neurons-Hidden-Layer [set hidden-links2 lput sublist hidden-links (si * 4) (si * 4 + 4) hidden-links2 set si si + 1 ]
  let gh 0
  let lh2[]
  (repeat length hidden-links [ set lh2 lput [weight] of item gh hidden-links lh2 set gh (gh + 1)]);;lh2 =list of weights (simple)
  let si2 0
  repeat Neurons-Hidden-Layer [set lhidden lput sublist lh2 (si2 * 4) (si2 * 4 + 4) lhidden set si2 si2 + 1 ]
  ;;set li
  set si 0
  let input-links2 [] ;liste de listes des liens
  repeat 8 [set input-links2 lput sublist input-links (si * Neurons-Hidden-Layer) (si * Neurons-Hidden-Layer + Neurons-Hidden-Layer) input-links2 set si si + 1 ]
  show("input-hidden links")
  show(input-links2)
  set gh 0
  let li2 []
  (repeat length input-links [ set li2 lput [weight] of item gh input-links li2 set gh (gh + 1)]); li2 list simple =weights des liens input-hidden
  set si2 0
  repeat 8 [set linput lput sublist li2 (si2 * Neurons-Hidden-Layer) (si2 * Neurons-Hidden-Layer + Neurons-Hidden-Layer) linput set si2 si2 + 1 ]
  ;;set lb
  set si 0
  let bias-links2 [] ;liste de listes des liens
  set bias-links2 lput sublist bias-links 0 Neurons-Hidden-Layer bias-links2
  set bias-links2 lput sublist bias-links Neurons-Hidden-Layer length bias-links bias-links2
  show("bias-(hidden / output) links")
  show(bias-links2)
  set gh 0
  let lb2 []
  (repeat length bias-links [ set lb2 lput [weight] of item gh bias-links lb2 set gh (gh + 1)]); li2 list simple =weights des liens input-hidden
  set si2 0
  set lbias lput sublist lb2 0 Neurons-Hidden-Layer lbias
  set lbias lput sublist lb2 Neurons-Hidden-Layer length bias-links lbias
  show("lb")
  show(lbias) ;liste de  list des weights input-hidden
end



; Auxiliary procedure to totally connect two groups of neurons
to connect [neurons1 neurons2]
  ;ask neurons1 [create-network-links-to neurons2 [  set weight random-float 0.2 - 0.1 hide-link]] ;kenet create-links-to
show("neurons1 connect ")show(neurons1)
  (foreach neurons1 [ n1 -> show("neuron 1 connect") show(n1)
  (foreach neurons2 [ n2 -> ask n1 [ create-network-link-to n2 [  set weight random-normal 0.2  0.1 hide-link  ]
      ;show my-links
    ] ] )
    ]
  )
end


; Step Function
to-report step [x]
  ifelse x > 0.5
    [ report 1 ]
    [ report 0 ]
end


; Forward Propagation of the signal along the network
to Forward-Propagation
  ;;procedure de changment des weights a partir du genotype

  (foreach mr-hidden-neurons [n -> ask n [if not dropped?  [set activation compute-activation] ]]) ;;layer index
  ;(foreach mr-output-neurons [n -> ask n [set activation compute-activation ]])
  (foreach mr-output-neurons [n ->
    set oin []
    ;(foreach sort(output-neurons) [a -> ask a [set oin lput sum [ [activation] of end1 * weight] of my-in-links  oin]]) ;oin = liste des Z de chaque output output neuron
    (foreach sort(output-neurons) [a -> set oin lput sum [ [activation] of end1 * weight] of [my-in-links] of a  oin ]) ;oin = liste des Z de chaque output output neuron
    let oin2 []
    (foreach oin [x -> carefully [ set oin2 lput (e ^ x) oin2][set oin2 lput (2 ^ 31 - 1) oin2 ]])
    let t sum oin2
    set oout []
    ;;;;;;
    set oin lput sum [ [activation] of end1 * weight] of [my-in-links] of n  oin ;liste des z de tous les output neurons ( somme (activation of hidden prec * weights) y compris bias)
    set oout lput softmax t sum [ [activation] of end1 * weight] of [my-in-links] of n oout
    ;;;;;;
    ask n [set activation compute-activation2 t  ]])
end
to-report compute-activation2 [t]
  ;set oin lput sum [ [activation] of end1 * weight] of my-in-links  oin ;liste des z de tous les output neurons ( somme (activation of hidden prec * weights) y compris bias)
  ;set oout lput softmax t sum [ [activation] of end1 * weight] of my-in-links oout
  report softmax t sum [ [activation] of end1 * weight] of my-in-links
end

to-report softmax [t x]
  let s 0
  carefully [ set s e ^ x / t ][set s (2 ^ 31 - 1 ) / t ]
  ;carefully [ set s e ^ x / t ][set s 0 ]
  report s
end

to Back-propagation
  let error-sample 0
  ; Compute error and gradient of every output neurons
  (foreach (mr-output-neurons) outputs [
    [ n y] -> ask n [ set grad activation * (1 - activation) * (y - activation) ]
    set error-sample error-sample + ( (y - [activation] of n) ^ 2 )])

  ; Average error of the output neurons in this epoch
  set epoch-error epoch-error + (error-sample / length mr-output-neurons)
  ; Compute gradient of hidden layer neurons
  ;show("hidden")
  ;show(mr-hidden-neurons)
  (foreach mr-hidden-neurons  [n -> ask n [ if not dropped? [
    set grad activation * (1 - activation) * sum [weight * [grad] of end2 ] of my-links ]
    ]]
  ) ;;kenet my-out-links

  ; Update link weights
  ask network-links [ set weight weight + Learning-rate * [grad] of end2 * [activation] of end1 ]  ;;kenet ask links

  set epoch-error epoch-error / 2
end


to Back-propagation2
  let a1 0
  let a2 0
  let a3 0
  let a4 0
  let x1 0
  let x2 0
  let x3 0
  let x4 0
  ;In order to derive the back-propagation math,
  ;we will first have to compute the total error across
  ;all the output neurons of our neural network and only then can we start with our derivatives and back-propagation.
  ;softmax error:
  let cross-entropy-error 0
  let t 0   ;t= somme( exp( zi )) avec zi = activation de output-neuron i
  (foreach sort mr-output-neurons[ n -> set t t + e ^ [activation ] of n ])

  (foreach (sort mr-output-neurons) outputs [
    [n y] -> set cross-entropy-error cross-entropy-error - (y * ln softmax t [activation] of n ) ] ) ;cross entropy /loss function


  ;;;;;;;;update weights :

  carefully[ set a1 ( - 1 ) * ( ( item 0 outputs * ( 1 / item 0 oout)) + ( 1 - item 0 outputs ) * ( 1 / (1 - item 0 oout )))][set a1 0] ;;dE1
  carefully[ set a2 ( - 1 ) * ( ( item 1 outputs * ( 1 / item 1 oout)) + ( 1 - item 1 outputs ) * ( 1 / (1 - item 1 oout )))][set a2 0] ;;dE2
  carefully[ set a3 ( - 1 ) * ( ( item 2 outputs * ( 1 / item 2 oout)) + ( 1 - item 2 outputs ) * ( 1 / (1 - item 2 oout )))][set a3 0]
  carefully[ set a4 ( - 1 ) * ( ( item 3 outputs * ( 1 / item 3 oout)) + ( 1 - item 3 outputs ) * ( 1 / (1 - item 3 oout )))][set a4 0]

  ;partie 2 de grad dOoutk /dOink
  carefully[set x1 (e ^ item 0 oin * ( e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin)) / (( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ) * ( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ))][]
  carefully[set x2 (e ^ item 1 oin * ( e ^ item 0 oin + e ^ item 2 oin + e ^ item 3 oin)) / (( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ) * ( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ))][]
  carefully[set x3 (e ^ item 2 oin * ( e ^ item 0 oin + e ^ item 1 oin + e ^ item 3 oin)) / (( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ) * ( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ))][]
  carefully[set x4 (e ^ item 3 oin * ( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin)) / (( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ) * ( e ^ item 0 oin + e ^ item 1 oin + e ^ item 2 oin + e ^ item 3 oin ))][]


  let l1 []
  let l2 []
  set l1 (list a1 a2 a3 a4)
  set l2 (list x1 x2 x3 x4)
  ;partie 3
  ;;activation of hidden layer
  let l3 []
  (foreach sort(mr-hidden-neurons) [ n -> set l3 lput [activation] of n l3] )
  ;matrices
  let prod1 []
  (foreach l1 l2 [ [i1 i2] -> set prod1 lput (i1 * i2) prod1 ])

;;matrice des gradients
  let matrice [] ;matrice des grad
  let col1 []
  let col2 []
  let col3 []
  let col4 []
  (foreach l3 [ l -> set col1 lput (item 0 prod1 * l) col1])
  show("col1")
  show(col1)
  set matrice lput col1 matrice
  (foreach l3 [ l -> set col2 lput (item 1 prod1 * l) col2])
  set matrice lput col2 matrice
  (foreach l3 [ l -> set col3 lput (item 2 prod1 * l) col3])
  set matrice lput col3 matrice
  (foreach l3 [ l -> set col4 lput (item 3 prod1 * l) col4])
  set matrice lput col4 matrice ; matrice : liste de liste des grad

  let matrice2 matrix:from-row-list matrice ;matrice2 = "matrice" des gradienst (maqlouba)
  set matrice2 matrix:transpose matrice2
  show("dim matrice2 ")
  show(matrix:dimensions matrice2)
  set matrice2 matrix:to-row-list matrice2 ;matrice2 liste de liste de grad kima fl taswira (shiha) ;;porqouoi???????
  show("matrice2")
  show(matrice2)

  ;;matrice after update
  let mh (matrix:from-row-list lhidden) ;matrice de lh (weights)
  show("dim mh")
  show(matrix:dimensions mh)

  let m2 (matrix:from-row-list matrice2) ;;m2 matrice a partir de matrice2
   show("dim m2")
  show(matrix:dimensions m2)

  let m4 (matrix:times-scalar (Learning-rate * -1 ) m2) ;;(-alpha)*grad
   show("dim m4")
  show(matrix:dimensions m4)

  let m5 (matrix:map + mh m4);;  w + (-alpha)*grad
  show("matrice weight finale")
  show(m5) ;matrice de weights finale item0 = weights sortants de hidden neuron 1
  show("dim m5")
  show(matrix:dimensions m5)



  ;;final list of weights <= list(matrice m5)
  set lhidden (matrix:to-row-list m5) ;list of weights (liste de listes)
  let lh-flattened []
  set lh-flattened reduce sentence lhidden ;list of weights(liste simple)
  ;update weights of links (output - hidden ) "weight of hidden-links( <= lh"
  let joujou 0
  repeat (4 * neurons-hidden-layer )[ ask item joujou  hidden-links  [set weight item joujou lh-flattened set joujou (joujou + 1)]]

  ;backprop sigmoid hidden-output
  ; set epoch-error epoch-error + (error-sample / count output-neurons)

  ; Compute gradient of hidden layer neurons
  ;ask mr-hidden-neurons with [not dropped?] [
    ;set grad activation * (1 - activation) * sum [weight * [grad] of end2] of my-out-links ]
 (foreach mr-hidden-neurons  [n -> ask n [ if not dropped? [
    set grad activation * (1 - activation) * sum [weight * [grad] of end2 ] of my-links ]
    ]]
  )
  ; Update link weights
  (foreach input-links [ wiwi -> ask wiwi [ set weight weight - Learning-rate * [grad] of end2 * [activation] of end1 ]])

  ;set epoch-error epoch-error / 2

show("sorted links")
show sort links

end

to-report compute-activation
  report sigmoid sum [ [activation] of end1 * weight] of my-links ;;kenet my-in-links
end

; Sigmoid Function
to-report sigmoid [x]
  let s 0
    ;report 1 / (1 + e ^ (- x))
  carefully [set s 1 / (1 + e ^ (- x))] [set s 0]
  report s
end

to create-samples
  set inputs liste_input
  set outputs liste_output
  set data-list (map [ [x y] -> (list x y)] inputs outputs) ; data-list = [ (x,y) tq x dans inputs et y outputs ]
end


to train
  set total-epoch-error 0
  set mean-epoch-error 0
  set linki 0
  ;ask maze-runners [

  set mr-network-links []
  let io []
  let i 0
  set epoch-error 0
  repeat 36 [
      (foreach mr-hidden-neurons [n -> ask n[ if ( (random 100 ) < dropout-rate ) [set dropped? true ]]])
    set io item i data-list
    set inputs first io
    set outputs last io
    ; Load input on input-neurons
    (foreach mr-input-neurons (inputs) [ [n x] -> ask n [set activation x] show("inputs init") show(inputs)] )
    ; Forward Propagation of the signal
    Forward-Propagation

      ;;;;;;;;;;;new ;;;;;;;;;;;;;;
  set lhidden []
  let si 0
  let hidden2-links []
  repeat Neurons-Hidden-Layer [set hidden2-links lput sublist hidden-links (si * 4) (si * 4 + 4) hidden2-links set si si + 1 ]
  let gh 0
  let lh2[]
  (repeat length hidden-links [ set lh2 lput [weight] of item gh hidden-links lh2 set gh (gh + 1)]);;lh2 liste feha weights (mch list of lists)  ;;??????????????????????,lh lezem tkoun liste de liste kol hiddenneuron yaati liste  ,,,,,,,,,
  let si2 0
  repeat Neurons-Hidden-Layer [set lhidden lput sublist lh2 (si2 * 4) (si2 * 4 + 4) lhidden set si2 si2 + 1 ]
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



    ; Back Propagation from the output error
    Back-propagation2
      (foreach mr-hidden-neurons [n -> ask n [set dropped? false]])
    set i i + 1]
  ;]

  ;;genotype final ;;

  ;ask turtles [
   ; if (breed = input-neurons ) [set linput lput [weight] of my-links  linput]
   ; if (breed = hidden-neurons) [set lhidden lput [weight] of my-links lhidden ]
   ; if (breed = bias-neurons) [set lbias lput [weight] of my-links lbias]
  ;]
  ;network-links

  ;set mr-network-links sublist sort (network-links)  (linki * 82) ((linki + 1 ) * 82)
   ;set linki linki + 1
  ;set liste sublist sort(hidden-neurons) (Neurons-Hidden-Layer * mi) (Neurons-Hidden-Layer * (mi + 1))

  ;set genotype mr-network-links
  create-gene


  set total-epoch-error total-epoch-error + epoch-error
  ;] ;;ask maze runners
  set mean-epoch-error total-epoch-error / population
  ;tick
end



; Activate input neurons with read inputs
to active-inputs
  (foreach (sort input-neurons) inputs [
              [n x] -> ask n [set activation x]])
end

to result ;;;zeyda
    let inp one-of liste_input  ;;inp <= ( liste nature voisins + distances )
    show(inp)
    set inputs inp
    active-inputs
    Forward-Propagation
  ;;+ tirer les valeurs de output (index direction) => bch taatina dir

end


to-report index-direction  ;; indice du noeud output maximal
  ;let max2 max [activation] of mr-output-neurons
  ;;mr-output-neurons = liste != agent set
  let maxi 0
  let w 0
  ;let wval 0
  (foreach mr-output-neurons [ o -> if ( [activation] of o > maxi ) [set maxi [activation] of o set w [who] of o ]])
  let index 0
  ;let w 0
  ;let wval 0
  ;set w [who] of mr-output-neurons with [activation = max2]
  ;set wval item 0 w ;;12   ;; w lezem tkoun valeur mch liste w ahna ketbin [ who] yaatni liste
  set index position w output-id
  report index
end


to-report liste_input
  let l3 []
  ;[left(impasse ou pas + distance%exit) , right ,up ,down) rq: 1 si impasse
 set l3 [[1 176 1 206 -1 2000 0 184][-1 2000 0 39 0 62 0 51][0 121 0 87 0 102 -1 2000][0 25 -1 2000 0 34 -1 2000][0 102 -1 2000 0 87 -1 2000][-1 2000 0 35 0 160 -1 2000][0 99 -1 2000 -1 2000 0 96][-1 2000 0 109 -1 2000 0 85 ][-1 2000 0 85 0 91.5 0 61.2][0 38 1 38 -1 2000 0 17][-1 2000 -1 2000 0 137 0 123 ][0 85 0 119 -1 2000 -1 2000][-1 2000 -1 2000 0 153 0 154][0 24 -1 2000 1 0 0 34][1 34 1 0 0 24 -1 2000][1 0 1 34 -1 2000 0 24][1 34 1 34 1 0 0 34][0 76 1 108 -1 2000 1 87][0 17 -1 2000 0 38 0 38][-1 2000 0 137 1 119 0 124][-1 2000 1 38 0 17 -1 2000][-1 2000 -1 2000 -1 2000 0 76][0 17 -1 2000 -1 2000 -1 2000][0 174 1 141 -1 2000 -1 2000][-1 2000 -1 2000 0 34 1 0][-1 2000 -1 2000 1 0 0 34][-1 2000 0 24 -1 2000 1 0][0 34 1 0 0 24 -1 2000][1 0 0 34 -1 2000 0 24][-1 2000 0 100 -1 2000 0 77 ][0 62 -1 2000 -1 2000 0 38][-1 2000 0 86 0 68 -1 2000][-1 2000 0 85 0 62 -1 2000][-1 2000 0 91 -1 2000 0 85][0 49 -1 2000 -1 2000 0 73][0 72 -1 2000 0 77 -1 2000]]
;show(length l3)
  report l3
end

to-report liste_output
  let l3 []
  ;[left(impasse ou pas + distance%exit) , right ,up ,down) rq: 1 si impasse
  set l3 [[0 0 0 1][0 1 0 0][0 1 0 0][1 0 0 0][0 0 1 0][0 0 1 0][1 0 0 0][0 0 0 1][0 0 0 1][1 0 0 0][0 0 0 1][1 0 0 0][0 0 1 0][0 0 1 0][0 1 0 0][1 0 0 0][0 0 1 0][1 0 0 0][1 0 0 0][0 0 0 1][0 0 1 0][0 0 0 1][1 0 0 0][1 0 0 0][0 0 0 1][0 0 1 0][0 0 0 1][0 1 0 0][1 0 0 0][ 0 0 0 1][0 0 0 1][0 0 1 0][0 0 1 0][0 0 0 1][1 0 0 0][1 0 0 0]]
;show(length l3)
  report l3
end


to create-gene
  set genotype []
  set genotype lput linput genotype
  set genotype lput lhidden genotype
  set genotype lput lbias genotype
  show("genotype")show( genotype)
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fin code du reseau de neuronnes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;code genetique;;;;;;;;;;;;;;;;;;;;;;;;
to go
  if [fitness] of min-one-of maze-runners [fitness] = 0  ;fitness = nbre de 1 dans la liste
    [ stop ]


  create-next-generation

  ;tick
end

to create-next-generation
  let old-generation maze-runners with [true]     ;;strategie choix bin 3 bch nkhaliwha
  let crossover-count  (floor (population * crossover-rate / 100 / 2))
  ;navigate
  repeat crossover-count
  [
    let parent1 min-one-of (n-of 3 old-generation) [fitness]
    let parent2 min-one-of (n-of 3 old-generation) [fitness]
    let child-genotype crossover ([genotype] of parent1) ([genotype] of parent2)

     ask one-of nodes with [label = "entrance"]
   [let present-node self
    ask patch-here
    [
      sprout-maze-runners 1
      [ set size 10
        set color blue
        set current-node present-node
        set visited-nodes []
        set visited-nodes2 []
        set visited-hubs []
        set I-found-exit? false
        set genotype item 0 child-genotype
      ]
        sprout-maze-runners 1
      [ set size 10
        set color pink
        set current-node present-node
        set visited-nodes []
        set visited-nodes2 []
        set visited-hubs []
        set I-found-exit? false
        set genotype item 1 child-genotype
      ]

  ]]]
  repeat (population - crossover-count * 2)
  [ask min-one-of (n-of 3 old-generation) [fitness]
      [ set color green hatch 1 ]]
  ask old-generation [ die ]
  ask maze-runners [ mutate ] ; there's a chance of mutations occurring
  navigate
  ask maze-runners[calculate-fitness ]
end

to-report crossover [genotype1 genotype2]
  let g1 reduce sentence reduce sentence genotype1
  let g2 reduce sentence reduce sentence genotype2
  let split-point 1 + random (length g1 - 1)


  set genotype1 convert g1
  set genotype2 convert g2

 let lchild list (sentence (sublist g1 0 split-point)
                        (sublist g2 split-point length g2))
              (sentence (sublist g2 0 split-point)
                        (sublist g1 split-point length g1))
  let genotype-child convert lchild ;convertir a partir de la liste lchild
  report genotype-child
end




to-report convert [g]

  let input-genotype sublist g 0 (8 * neurons-hidden-layer) ;list of input-hidden links (liste simple )
  let hidden-genotype sublist g (8 * neurons-hidden-layer) (12 * neurons-hidden-layer) ;list of hidden-output weights (liste simple)
  let bias-genotype sublist  g (12 * neurons-hidden-layer) (length g)

  let input-genotype2 []
  let hidden-genotype2 []
  let bias-genotype2 []

  let si2 0
  repeat 8 [set input-genotype2 lput sublist input-genotype (si2 * Neurons-Hidden-Layer) (si2 * Neurons-Hidden-Layer + Neurons-Hidden-Layer) input-genotype2 set si2 si2 + 1 ] ;liste de liste

  set si2 0
  repeat Neurons-Hidden-Layer [set hidden-genotype2 lput sublist hidden-genotype (si2 * 4) (si2 * 4 + 4) hidden-genotype2 set si2 si2 + 1 ]


  set bias-genotype2 lput sublist bias-genotype 0 Neurons-Hidden-Layer bias-genotype2
  set bias-genotype2 lput sublist bias-genotype Neurons-Hidden-Layer length bias-genotype bias-genotype2

  let gene lput input-genotype2 genotype
  set gene lput hidden-genotype2 genotype
  set gene lput bias-genotype genotype
 report gene
end

to calculate-fitness       ;; turtle procedure
  ;; min distance %exit puis longeur visitednodes

          let xn xcor ;;x du maze runner
          let yn ycor ;; y du maze runner
          let xexit [xcor] of nodes with [reward = 100]
          let yexit  [ycor] of nodes with [reward = 100]
          set xexit item 0 xexit
          set yexit item 0 yexit
          set dist-finale sqrt( (xn - xexit) * (xn - xexit)  + (yn - yexit) * (yn - yexit) ) ;distance % exit
          set nbre-visited-nodes length visited-nodes2
          set fitness dist-finale

end

to mutate   ;; turtle procedure
  let g reduce sentence reduce sentence genotype
  set g map [ b ->
    ifelse-value random-float 100.0 < mutation-rate
      [ b * 0.01 ]  ;;b <= b * epsilon
      [ b ]
  ] g
  set genotype convert g
end

@#$#@#$#@
GRAPHICS-WINDOW
225
14
896
469
-1
-1
2.6414343
1
8
1
1
1
0
0
0
1
0
250
0
168
0
0
1
ticks
30.0

BUTTON
116
12
194
53
Setup
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
31
97
152
130
spacing
spacing
3
20
17.0
1
1
NIL
HORIZONTAL

BUTTON
13
10
91
51
Reset
reset-maze-runners
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
35
509
156
542
debug
debug
0
2
0.0
1
1
NIL
HORIZONTAL

BUTTON
47
283
176
316
NIL
navigate
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
43
69
193
87
maze parameters
14
125.0
1

SLIDER
38
587
158
620
exploration
exploration
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
37
633
157
666
step-size
step-size
0
1
0.95
0.05
1
NIL
HORIZONTAL

PLOT
241
498
582
688
steps-per-episode
NIL
NIL
0.0
70.0
-20.0
10.0
true
true
"" ""
PENS
"sum rewards per ep" 1.0 0 -15040220 true "" "plotxy episode steps"

SLIDER
37
663
157
696
discount
discount
0
1
0.95
0.01
1
NIL
HORIZONTAL

TEXTBOX
769
530
919
572
                0\n        270        90\n               180
11
105.0
1

SLIDER
37
695
157
728
decay
decay
0
1
0.02
0.01
1
NIL
HORIZONTAL

TEXTBOX
15
239
165
257
Algorithms 
14
125.0
1

BUTTON
24
423
102
456
NIL
add-exit\n
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
38
548
158
581
num-episodes
num-episodes
0
50
43.0
1
1
NIL
HORIZONTAL

SLIDER
11
165
183
198
max-steps
max-steps
0
2000
46.0
1
1
NIL
HORIZONTAL

SLIDER
1133
38
1318
71
NEURONS-INPUT-LAYER
NEURONS-INPUT-LAYER
0
8
8.0
1
1
NIL
HORIZONTAL

SLIDER
1132
102
1306
135
Neurons-Hidden-Layer
Neurons-Hidden-Layer
0
10
6.0
1
1
NIL
HORIZONTAL

SLIDER
1134
188
1309
221
Neurons-Output-Layer
Neurons-Output-Layer
0
4
4.0
1
1
NIL
HORIZONTAL

SLIDER
1135
147
1307
180
num-samples
num-samples
0
100
36.0
1
1
NIL
HORIZONTAL

SLIDER
1147
269
1319
302
Learning-rate
Learning-rate
0
10
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
1149
314
1321
347
dropout-rate
dropout-rate
0
1
1.0
0.1
1
NIL
HORIZONTAL

MONITOR
931
265
1044
310
NIL
mean-epoch-error
17
1
11

BUTTON
159
78
222
111
NIL
quit
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
935
84
1107
117
population
population
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
990
428
1162
461
crossover-rate
crossover-rate
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
992
478
1164
511
mutation-rate
mutation-rate
0
2
1.6
0.1
1
NIL
HORIZONTAL

SWITCH
1016
536
1150
569
plot-diversity?
plot-diversity?
0
1
-1000

PLOT
744
634
944
784
Diversity Plot
gen #
diversity
0.0
20.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plot-diversity? [ plot diversity ]"

@#$#@#$#@
## WHAT IS IT?


A NetLog script written for version 6.1.1. A model of agent trying to find exit. 

## HOW IT WORKS

As a initial step it generates a maze according to the spacing set by the slider. It creates a network with nodes along the path. 
After agent try to find exit. At every hub it colors the path according to history. 
Green if it's the shortest path from the entrance. 
Yellow in case it still has to very if there is a blind spot at the end of the road. 
Red for those path who takes nowhere. 
In the meanwhile agent explores the world some monitors and a plot show statistics on the left side of the interface. 

## HOW TO USE IT

Press "Setup" to start
Press "Find exit" to make agent find exit 
Press "Find exit step-by-step to make agent stop after each node.


## THINGS TO NOTICE

Pay attention of how agent comes back when it finds a blind spot. 

## THINGS TO TRY

Adjust the spacing to create smaller or bigger maze.
Set debug to 1 or 2 in order to print a logger. 

## EXTENDING THE MODEL

Algorithm takes into accounts that more agents could explore the maze at the same time in order to find exit faster. Future versions could support the creation of more maze runners.
A future study could create a second maze runner that take into accounts path already explored by the first maze runner. A cost function could estimate the perfect time the second maze runner needs to wait to find exit faster. 

## RELATED MODELS

This work is based on script created as a case study for a the graduation thesis: "Cooperative and optimization strategies in bio-based agents model" by C. Crespi and A. Rapisarda, A. Pluchino as supervisor. 

## CREDITS AND REFERENCES

NetLogo model developed by R. Rotondo (riccardo.rotondo@phd.unict.it) as an assignment of a PhD course. 
A copy, along with some documentation and screenshots, is available on github at: https://github.com/rrotondo/maze-escape 
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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
