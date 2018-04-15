deleteEdgeSegments[initmask_, selectedpts_] := Module[{pts = selectedpts, mask = initmask, whitepixpos, nearest},
    whitepixpos = PixelValuePositions[mask, 1];
    nearest = Nearest[whitepixpos, DistanceFunction -> EuclideanDistance];
    pts = pts /. patt : {_, _} /; ! MemberQ[whitepixpos, patt] :> Flatten[nearest[patt], 1]; 
    (* Replace[pts,{x__}/; !MemberQ[whitepixpos,{x}]\[RuleDelayed]First@nearest[{x}],{1}] *)
    Pruning@DeleteSmallComponents[ReplacePixelValue[mask, pts -> 0]]
    ] /; Length@selectedpts > 0;

interpPoints[selectedpts_, initmask_, interpthresh_] := Module[{pts = selectedpts, ptsToChange, positionReversal, 
     reverselist, xrange, yrange, xs, ys, ascendListOrder, interpolatedList, threshold = interpthresh, whitepixpos,
     nearest, extractends, mask = initmask},
    
    whitepixpos = PixelValuePositions[mask, 1];
    nearest = Nearest[whitepixpos, DistanceFunction -> EuclideanDistance];
    extractends = {First@pts, Last@pts};
    
    MapThread[Replace[extractends[[#1]], x_ /; ! MemberQ[whitepixpos, x] :> (#2[pts, First@nearest[x]])] &,
    {{1, 2}, {PrependTo, AppendTo}}]; 
    
    pts = Partition[DeleteDuplicates@pts, 2, 1];
    positionReversal = Position[pts, {{x_, _}, {x_, _}}];
     reverselist = MapAt[Map[Reverse, #] &, pts, positionReversal];
    ascendListOrder = Map[SortBy[#, First] &, reverselist];
    
    interpolatedList = Map[Function[{x},
        xs = x[[All, 1]];
        ys = x[[All, 2]];
        xrange = Range[Sequence @@ xs, threshold];
        yrange = Subdivide[Sequence @@ ys, Length[xrange] - 1];
        ptsToChange = Thread@{xrange, yrange}
        ], ascendListOrder] // Round;
    
    DeleteDuplicates@Flatten[MapAt[Map[Reverse, #] &, interpolatedList, positionReversal], 1]
    ] /; Length@selectedpts > 0;

correctSegmentation[image_?ImageQ, correspondingMask_?ImageQ, saveDir_?StringQ] := With[{boxwidth = 10, interp = 0.01, magmask = 5,
magimage = 10, imgdim = ImageDimensions@image},
  Module[{img = image, imgmask = correspondingMask, saveLink = saveDir},
   zoomMask[x_, y_, immask_] := Module[{row1, row2, col1, col2},
     CompoundExpression[If[imagedim[[2]] - y - boxwidth <= 0, row1 = 0, 
       row1 = imagedim[[2]] - y - boxwidth]; 
      If[imagedim[[2]] - y + boxwidth >= imagedim[[2]], 
       row2 = imagedim[[2]], row2 = imagedim[[2]] - y + boxwidth]; 
      If[x - boxwidth <= 0, col1 = 0, col1 = x - boxwidth];
      If[x + boxwidth >= imagedim[[1]], col2 = imagedim[[1]], 
       col2 = x + boxwidth];
      Magnify[ImageTake[HighlightImage[immask, {Red, AbsolutePointSize[3], Point[Round@{x, y} - 1]}], {row1, row2}, {col1, col2}], 
       magmask]
      ]
     ];
   
   DynamicModule[{list = {}, temp, mouse, mask = imgmask, 
     reset = imgmask, (placeholder = HighlightImage[img, Dynamic@mask]), rule, undo = imgmask},
    
    Framed[Column@{EventHandler[
        Column@{
          Row@{Magnify[Dynamic@placeholder, magimage],
            Manipulate[
             If[temp =!= None,
              Row@{(*Style["pixel val"\[Rule] PixelValue[mask,Round@temp],Bold,FontSize\[Rule]18,FontFamily\[Rule]"Courier"], *)
                zoomMask[Sequence @@ Flatten@{temp, mask}]
                }, "Magnify Mask"
              ], {pt, Dynamic[temp = MousePosition["Graphics"]], 
              Locator, Appearance -> None}]
            },
          Dynamic@list},
        {"MouseClicked", 1} :> (list = If[(mouse = MousePosition["Graphics"]) =!= None, AppendTo[list, mouse], list])],
       
       ButtonBar[{
         "Delete Edge" :> 
          If[Length@list > 0, (undo = mask; mask = deleteEdgeSegments[mask, list]; list = {};)],
         "Add Edge" :> If[Length@list > 0, (undo = mask; 
         mask = ReplacePixelValue[mask, interpPoints[list, mask, interp] -> 1]// Thinning; list = {};)],
         "Undo Operation" :> (mask = undo; list = {};),
         "Reset Mask" :> (mask = reset; list = {}; undo = mask),
         "Show/Hide points" :> If[Length@list > 0, placeholder = placeholder /. {(rule = HighlightImage[img, Dynamic@mask] -> 
                 HighlightImage[Rasterize@HighlightImage[img, Dynamic@mask], {RGBColor[0.2,0.6, 0.9], AbsolutePointSize[1],
                 Point@list}]), Reverse@rule}],
         "Reset points" :> (list = {};),
         "Save" :> (Export[saveLink, mask]; list = {};)
         },
        Background -> LightBlue, ImageMargins -> 5]},
     Background -> LightPink]
    ]
   ]
  ]
