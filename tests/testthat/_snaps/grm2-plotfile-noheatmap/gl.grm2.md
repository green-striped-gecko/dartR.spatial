# gl.grm2 reference-data behaviour is unchanged

    Code
      print(if (inherits(plot_file, "error")) conditionMessage(plot_file) else class(
        plot_file))
    Output
      [1] "matrix" "array" 

