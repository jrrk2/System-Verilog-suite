# Test RTLIL file
autoidx 1
attribute \top 1

module \test_and
  wire width 1 input 1 \a
  wire width 1 input 2 \b
  wire width 1 output 3 \y

  cell $_AND_ $and_inst
    connect \A \a
    connect \B \b
    connect \Y \y
  end
end

module \test_add
  wire width 4 input 1 \a
  wire width 4 input 2 \b
  wire width 4 output 3 \sum

  cell $add $add_inst
    parameter \A_WIDTH 4
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 4
    connect \A \a
    connect \B \b
    connect \Y \sum
  end
end

module \test_dff
  wire input 1 \clk
  wire input 2 \d
  wire output 3 \q

  cell $_DFF_P_ $ff_inst
    connect \C \clk
    connect \D \d
    connect \Q \q
  end
end
