open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform_improved ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    let is_blue =
      Pixel.blue pixel > Pixel.red pixel + Pixel.green pixel
      && Pixel.blue pixel + Pixel.red pixel + Pixel.green pixel > 10000
    in
    match is_blue with true -> Image.get background ~x ~y | false -> pixel)
;;

let _transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    let is_blue = Pixel.blue pixel > Pixel.red pixel + Pixel.green pixel in
    match is_blue with true -> Image.get background ~x ~y | false -> pixel)
;;

(* let transform_improved ~foreground ~background = let new_foreground =
   transform ~foreground ~background in transform ~foreground:new_foreground
   ~background ;; *)

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform_improved ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

type compare_image = { mutable equivalent : bool }

let%expect_test "bluescale" =
  let source_img =
    Image.load_ppm ~filename:"../images/oz_bluescreen_vfx.ppm"
  in
  let reference_img =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let comparator = { equivalent = true } in
  let _compare_images =
    Image.mapi source_img ~f:(fun ~x ~y src_pixel ->
      let ref_pixel = Image.get reference_img ~x ~y in
      match Pixel.equal src_pixel ref_pixel with
      | true ->
        ();
        src_pixel
      | false ->
        comparator.equivalent <- false;
        src_pixel)
  in
  print_s [%message (comparator.equivalent : bool)];
  [%expect {|
   (comparator.equivalent true) |}]
;;
