# gl.spatial.autoCorr reference-data behaviour is unchanged

    Code
      print(summarise_sac(result))
    Output
      $class
      [1] "list"
      
      $names
      [1] "pooled"
      
      $tables
      $tables$pooled
            Bin   N      r.uc Correction         r       L.r       U.r L.r.null.uc
      1  345900 145  0.544750  0.0333333  0.578083  0.557515  0.603156  -0.0625059
      2  691800 110 -0.266513  0.0333333 -0.233180 -0.243251 -0.225939  -0.1260030
      3 1037700 210 -0.305999  0.0333333 -0.272666 -0.283464 -0.261333  -0.0584998
         U.r.null.uc   L.r.null  U.r.null p.one.tail
      1  2.41036e-02 -0.0291726 0.0574369       0.05
      2  5.25817e-05 -0.0926693 0.0333859       0.05
      3 -1.59924e-02 -0.0251665 0.0173409       0.05
      
      

