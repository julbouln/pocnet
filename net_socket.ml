let data_to_chan d oc=
(*  output_value oc d;; *)
  output_string oc d;;

let chan_to_data ic=
(*  input_value ic;; *)
input_line ic;;
