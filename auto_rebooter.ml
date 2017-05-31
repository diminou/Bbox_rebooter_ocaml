open Core.Std
open Cohttp
open Boygues_uris
open Test_times
open Lwt
open Lwt_unix
open Cohttp_lwt
open Cohttp_lwt_unix

module Auto_rebooter = struct
  type config = Boygues_uris.t * Test_times.t
  
  let time_file = "times.conf"
  
  let bbox_file = "bbox.conf"

  let test_times = Test_times.test_times_of_json_file time_file
  
  let bbox_conf = Boygues_uris.boygues_uris_of_json_file bbox_file
  
  let conf = (bbox_conf, test_times)

  let test_connectivity uri =
    Client.get uri
      >|= fun (resp, body) ->
        Cohttp_lwt_body.drain_body body |> Lwt_main.run;
        match (Response.status resp) with
        | `OK -> Some(true)
        | _ -> Some(false)

  let idle sleep_time =
      let helper  () = Lwt_unix.sleep sleep_time in
        helper () >>=
          fun () -> Lwt.return None

  let is_connected uri timeout =
      let timeout_probe = idle timeout in
        let conn_test = test_connectivity uri in
        let either = Lwt.pick [timeout_probe; conn_test] in
        let res = Lwt_main.run either in
        match res with
          | None -> false
          | Some x -> x

  let lookup_cookies lst =
      let rec lookup_cookies' accum lst = match lst with
            | [] -> accum
                  | (title, value)::tl ->
                    if (String.equal title "set-cookie")
                    then (lookup_cookies' (value::accum) tl)
                    else (lookup_cookies' accum tl)
      in lookup_cookies' [] lst

  let build_cookies lst =
      let rec build_cookeies' accum lst = match lst with
            | [] -> accum
            | hd::tl -> build_cookeies' (("Cookie", hd)::accum) tl
      in build_cookeies' [] lst

  let check_base = fun () -> is_connected bbox_conf.base_uri test_times.connection_timeout

  let check_test = fun () -> is_connected bbox_conf.test_uri test_times.connection_timeout

  let say_rebooted = fun () ->
    let open Core.Time in
    let tm = Core.Time.now () in
    let str_tm = Core.Time.to_string tm in
    let message = str_tm ^ " Rebooted\n" in
    print_endline message

  let reboot_full = fun () ->
    let run_resp rquest =
      let (rsp, body) = Lwt_main.run rquest in
      (rsp, body)
    in
    let content_headers = Header.of_list [("Content-Type", "application/x-www-form-urlencoded");("Content-Length", string_of_int (Bytes.length bbox_conf.login_payload))] in
    let reboot token hdrs = bbox_conf.reboot_uri token |> Client.post ~headers: (Header.of_list (("Content-Length", "0")::hdrs)) in
    let full_reboot req =
        let (rsp, body) = run_resp req in
        Cohttp_lwt_body.drain_body body |> Lwt_main.run;
        let hdr_list = Response.headers rsp
          |> Header.to_list
          |> lookup_cookies
          |> build_cookies in
        let hdr = hdr_list
          |> Header.of_list in
        let req2 = Client.get ~headers: hdr bbox_conf.token_uri in
        let (rsp2, body2) = run_resp req2 in
        let tok = body2
          |> Cohttp_lwt_body.to_string
          |> Lwt_main.run
          |> Yojson.Basic.from_string in
        let token = tok
          |> Yojson.Basic.Util.to_list
          |> List.map ~f: (Yojson.Basic.Util.member "device")
          |> List.map ~f:(Yojson.Basic.Util.member "token")
          |> List.map ~f: Yojson.Basic.Util.to_string
          |> String.concat in
        reboot token hdr_list >>= fun (rsp3, body3) -> Cohttp_lwt_body.drain_body body3
    in
    let req =
      Client.post ~body: (Cohttp_lwt_body.of_string bbox_conf.login_payload) ~headers: content_headers bbox_conf.login_uri
    in
    say_rebooted ();
    full_reboot req

  type status = Offline | Disconnected | OK

  let check_then_reboot = fun () ->
    let base_status = check_base () in
    let test_status = check_test () in
    match (base_status, test_status) with
      | (false, _) -> Offline
      | (true, false) -> Disconnected
      | _ -> OK

  let rec loop_check_reboot = fun () ->
    let task = check_then_reboot () |> Lwt.return >|=
      fun result -> match result with
        | OK -> Lwt_unix.sleep test_times.test_spacing |> Lwt_main.run
        | Offline -> Lwt_unix.sleep test_times.box_offline_delay |> Lwt_main.run
        | Disconnected -> reboot_full () |> Lwt_main.run
    in
    Lwt_main.run task;
    loop_check_reboot ()

  let () =
    loop_check_reboot ()
end
