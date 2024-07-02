open Core

let add_error image x y (error : float) =
  if x >= 0 && x < Image.width image && y >= 0 && y < Image.height image
  then (
    let original_pixel = Image.get image ~x ~y in
    let og_pixel_val = Pixel.red original_pixel in
    let new_pixel_val =
      Int.min
        (Int.max (og_pixel_val + Int.of_float error) 0)
        (Image.max_val image)
    in
    let new_pixel = Pixel.of_int new_pixel_val in
    Image.set image ~x ~y new_pixel)
;;

(* This should look familiar by now! *)
let transform image =
  (* Convert all values to black/white values *)
  let benchmark = Image.max_val image / 2 in
  let img = Grayscale.transform image in
  Image.foldi img ~init:img ~f:(fun ~x ~y img pixel ->
    let og_pixel_val = Pixel.red pixel in
    (* let new_pixel_val = match og_pixel_val > benchmark with | true ->
       Image.max_val image | false -> 0 in *)
    match og_pixel_val > benchmark with
    | true ->
      let new_pixel_val = Image.max_val img in
      let new_pixel = Pixel.of_int new_pixel_val in
      Image.set img ~x ~y new_pixel;
      let error = og_pixel_val - new_pixel_val in
      let adjacent_right = Int.to_float error *. (7.0 /. 16.0) in
      let bottom_left = Int.to_float error *. (3.0 /. 16.0) in
      let adjacent_below = Int.to_float error *. (5.0 /. 16.0) in
      let bottom_right = Int.to_float error *. (1.0 /. 16.0) in
      add_error img (x + 1) y adjacent_right;
      add_error img (x - 1) (y + 1) bottom_left;
      add_error img x (y + 1) adjacent_below;
      add_error img (x + 1) (y + 1) bottom_right;
      img
    | false ->
      let new_pixel_val = 0 in
      let new_pixel = Pixel.of_int new_pixel_val in
      Image.set img ~x ~y new_pixel;
      let error = og_pixel_val - new_pixel_val in
      let adjacent_right = Int.to_float error *. (7.0 /. 16.0) in
      let bottom_left = Int.to_float error *. (3.0 /. 16.0) in
      let adjacent_below = Int.to_float error *. (5.0 /. 16.0) in
      let bottom_right = Int.to_float error *. (1.0 /. 16.0) in
      add_error img (x + 1) y adjacent_right;
      add_error img (x - 1) (y + 1) bottom_left;
      add_error img x (y + 1) adjacent_below;
      add_error img (x + 1) (y + 1) bottom_right;
      img)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
