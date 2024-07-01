open Core

(* given a coordinate, will return the appropriate bounds needed to make a
   sqaure of a certain radius in a tuple *)
let get_valid_coords image x y radius =
  let x_start = Int.max 0 (x - radius) in
  let x_end = Int.min (Image.width image - 1) (x + radius) in
  let y_start = Int.max 0 (y - radius) in
  let y_end = Int.min (Image.height image - 1) (y + radius) in
  x_start, x_end, y_start, y_end
;;

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _ ->
    let x_start, x_end, y_start, y_end = get_valid_coords image x y radius in
    let raidus_around_pixel =
      Image.slice image ~x_start ~x_end ~y_start ~y_end
    in
    Image.mean_pixel raidus_around_pixel)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

type compare_image = { mutable equivalent : bool }

let%expect_test "blur" =
  let source_img =
    Image.load_ppm ~filename:"../images/beach_portrait_blur.ppm"
  in
  let reference_img =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
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
