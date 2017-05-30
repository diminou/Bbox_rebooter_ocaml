open Core.Std

module Test_times = struct
  type t =
    { connection_timeout: float;
      test_spacing: float;
      box_offline_delay: float
    }

  let test_times_of_timeout_spacing_delay connection_timeout test_spacing box_offline_delay =
    { connection_timeout;
      test_spacing;
      box_offline_delay}

  let test_times_of_json_file file =
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_file file in
    let ct = json |> member "connection_timeout" |> to_float in
    let test_spacing  = json |> member "test_spacing" |> to_float in
    let box_offline_delay = json |> member "box_offline_delay" |> to_float in
    test_times_of_timeout_spacing_delay ct test_spacing box_offline_delay
end
