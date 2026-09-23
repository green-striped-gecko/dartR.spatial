# gl.spatial.autoCorr reference-data behaviour is unchanged

    Code
      print(summarise_sac(result))
    Output
      $class
      [1] "list"
      
      $names
      [1] "EmmacMaclGeor"
      
      $tables
      $tables$EmmacMaclGeor
        Bin  N r.uc Correction           r        L.r       U.r L.r.null.uc
      1   0  0  NaN        0.1         NaN         NA        NA          NA
      2   0  0  NaN        0.1         NaN         NA        NA          NA
      3   0 55 -0.1        0.1 1.31839e-15 -0.0677441 0.0865791        -0.1
        U.r.null.uc    L.r.null    U.r.null p.one.tail
      1          NA          NA          NA         NA
      2          NA          NA          NA         NA
      3        -0.1 1.38778e-16 9.71445e-16       0.05
      
      

