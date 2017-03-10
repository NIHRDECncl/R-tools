library(DiagrammeR)
grViz("
digraph boxes_and_circles {
      
      # a 'graph' statement
      graph [overlap = true, fontsize = 10, layout = dot]
      
      # several 'node' statements
      node [shape = box,
      fontname = Helvetica]
      A [label = 'Patient with COPD and previously hospitalized',  fontcolor = brown, shape = plaintext]; 
      B [label = 'Selfcare', shape = square]; 
      C [label = 'Early respiratory symptoms', shape = diamond];
      D [label = 'Headstart baseline testing n/wk', shape = diamond];
      E [label = 'Wait and re-test', shape = square, style = filled, fillcolor = blue, fontcolor = white]; 
      F [label = 'Start rescue pack', shape = square, style = filled, fillcolor = blue, fontcolor = white]; 
      G [label = 'Primary care/\\ncommunity care suppoprt', shape = square, style = filled, fillcolor = green, fontcolor = white]; 
      H [label = 'Visit to A&E/\\nhospital-based clinic', shape = square, style = filled, fillcolor = red, fontcolor = white];
      I [label = 'Admission to ward', shape = square, style = filled, fillcolor = red, fontcolor = white];
      J [label = 'Death', shape = oval, style = filled, fillcolor = SeaGreen, fontcolor = white] 

      subgraph cluster0 {A -> B -> C -> D}
      subgraph cluster1 {C -> D} 
      subgraph cluster2 {E -> F -> G -> H -> I}

      # several 'edge' statements
      edge [color = white, arrowtail = none]
      A -> B label = 'label'

      edge [color = blue, arrowtail = none]
      B -> {C D}
      D -> {B C}
      C -> {B E F G H}
      E -> C
      F -> C
      G -> {E F H}
      H -> {B I}
      I -> {B J}
      }
      ")
