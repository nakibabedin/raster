open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun pixel ->
    let average_pixel_value =
      (Pixel.red pixel + Pixel.blue pixel + Pixel.green pixel) / 3
    in
    Pixel.of_int average_pixel_value)
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

type compare_image = { mutable equivalent : bool }

let%expect_test "gray_scale" =
  let source_img =
    Image.load_ppm ~filename:"../images/beach_portrait_gray.ppm"
  in
  let reference_img =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
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
  [%expect {| (comparator.equivalent true) |}]
;;
