open Core.Std
open Cohttp

module Boygues_uris = struct
  type t =
    { base_uri: Uri.t;
      login_uri: Uri.t;
      token_uri: Uri.t;
      reboot_uri: string -> Uri.t;
      test_uri: Uri.t;
      login_payload: string
    }

  let robust_tail lst = match lst with
    | [] -> []
    | [last_element] -> lst
    | (hd :: tl) -> tl

  let strip_3_chars str =
      match String.length str with
        | 0 | 1 -> ""
        | len -> String.sub str 1 (len - 1)

  let boygues_uris_of_base_url_test_url_login_payload base_url test_url login_payload =
    let base_uri = Uri.of_string base_url in
    let test_uri = Uri.of_string test_url in
    let login_uri = (base_url ^ "/api/v1/login") |> Uri.of_string in
    let token_uri = (base_url ^ "/api/v1/device/token") |> Uri.of_string in
    let reboot_uri token =  (base_url ^ "/api/v1/device/reboot?btoken=" ^ token) |> Uri.of_string in
    { base_uri;
      login_uri;
      token_uri;
      reboot_uri;
      test_uri;
      login_payload;
    }

  let boygues_uris_of_json_file file =
    let open Yojson.Basic.Util in
    let open Yojson.Basic in
    let json = Yojson.Basic.from_file file in
    let bu = json |> member "base_url" |> to_string |> strip_3_chars in
    let tu = json |> member "test_url" |> to_string |> strip_3_chars in
    let lp = json |> member "login_payload" |> to_string |> strip_3_chars in
    boygues_uris_of_base_url_test_url_login_payload bu tu lp

end
