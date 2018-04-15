(* the scheme for segmentation *)

files = "C:\\Users\\aliha\\Desktop\\scripts-codes\\miscellaneous codes\\edge detect\\steven -ali - mesh\\8\\A000";

filesave = "C:\\Users\\aliha\\Desktop\\scripts-codes\\miscellaneous codes\\edge detect\\steven -ali - mesh\\processed\\p ";

processBatch[fileprenom_, filenum_, cropArea_, ridge_, gaussian_, localBin_, small_, closing_, filesave_] := Module[{processedImages,
fileNames = fileprenom, image, length = filenum, imagemask},
  processedImages = Table[(image = Import[fileNames <> ToString[i] <> ".tif"];
     image = ImageTake[image, Sequence @@ cropArea];
     imagemask = ImageAdjust@RidgeFilter[image, ridge] // GaussianFilter[#, gaussian] & // LocalAdaptiveBinarize[#, localBin] & // 
         DeleteSmallComponents[#, small] & // Closing[#, closing] & // Thinning;
     FixedPoint[Pruning, imagemask]), {i, 0, length - 1}];
     
  MapThread[Export[filesave <> ToString[#1] <> ".tiff", #2, "TIFF"] &, {Range[0, Length[processedImages] - 1], processedImages}];
  ]

processBatch[files, 10, {{50, 700}, {100,All}}, 7, 10, 53, 20000, 12, filesave]
