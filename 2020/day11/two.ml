let run () =
  let seats = Lib.In_channel.read_lines "day11/test" in
  string_of_int (One.churn ~tolerance:5 seats 0)
