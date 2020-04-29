[%%import "config.h"]

[%%if LOGGING = 0]
  (* If logging has been entirely disabled during compilation *)
  let fatal _ = ()
  let error _ = ()
  let warn  _ = ()
  let info  _ = ()
  let debug _ = ()

  let empty_line () = ()
  let push_indent () = ()
  let pop_indent () = ()

  let disable () = ()

  let [@warning "-27"] enable ?msg ?level _ = ()
[%%else]
  (* If logging has not been disabled, a user may still choose not to log
   * during a particular execution. Logging functions therefore accept `lazy`
   * strings that are forced only when they are actually logged. *)

  open Core

  type level = Debug | Error | Info
  let level_str = function Debug -> "( debug )"
                         | Error -> "< ERROR >"
                         | Info  -> "(  info )"

  let log_chan = ref stderr
  let log_level = ref Debug
  let log_indent = ref 0

  let is_enabled = ref false
  let should_log level =
    if not !is_enabled then false
    else match level, !log_level with
         | Error , _ | Info , Info | _ , Debug -> true
         | _ -> false

  let do_log level lstr =
    if should_log level
    then begin
      let now_ns = Time_now.nanoseconds_since_unix_epoch ()
       in Out_channel.fprintf
            !log_chan
            "%s.%3d  %s  %s%s\n"
            Time.(format (now ()) "%d-%b-%y  %T" ~zone:(Lazy.force Zone.local))
            Int63.(to_int_trunc ((now_ns / (of_int 100000)) % (of_int 10000)))
            (level_str level)
            (String.make !log_indent ' ')
            (Lazy.force lstr)
    end

  let info lstr = do_log Info lstr
  let debug lstr = do_log Debug lstr
  let error lstr = do_log Error lstr

  let empty_line () = do_log Info (lazy "")
  let push_indent () = log_indent := !log_indent + 2
  let pop_indent () = log_indent := !log_indent - 2

  let disable () = is_enabled := false

  let enable ?(msg = "") ?(level = Debug) = function
    | None -> ()
    | Some filename
      -> log_chan := Out_channel.create ~append:true filename
       ; log_level := level
       ; is_enabled := true
       ; info (lazy "")
       ; info (lazy msg)
       ; info (lazy (String.(make (128 - (length msg)) '=')))
[%%endif]
