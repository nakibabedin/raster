open! Core

(* given a coordinate, will return the appropriate bounds needed to make a
   sqaure of a certain radius in a tuple *)
let get_valid_coords image x y radius =
  let x_start = Int.max 0 (x - radius) in
  let x_end = Int.min (Image.width image - 1) (x + radius) in
  let y_start = Int.max 0 (y - radius) in
  let y_end = Int.min (Image.height image - 1) (y + radius) in
  x_start, x_end, y_start, y_end
;;

let find_magnitude image =
  let gradient_x : int array array =
    [| [| -1; 0; 1 |]; [| -2; 0; 2 |]; [| -1; 0; 1 |] |]
  in
  let gradient_y : int array array =
    [| [| -1; -2; -1 |]; [| 0; 0; 0 |]; [| 1; 2; 1 |] |]
  in
  (* Array.iter gradient_x ~f:(fun row -> Array.iter row ~f:(printf "%d ");
     printf "\n"); *)
  let x_direction_kernel =
    Image.mapi image ~f:(fun ~x ~y pixel ->
      let curr_pixel_val = Pixel.red pixel in
      let new_pixel_val =
        curr_pixel_val * Array.get (Array.get gradient_x y) x
      in
      Pixel.of_int new_pixel_val)
  in
  let y_direction_kernel =
    Image.mapi image ~f:(fun ~x ~y pixel ->
      let curr_pixel_val = Pixel.red pixel in
      let new_pixel_val =
        curr_pixel_val * Array.get (Array.get gradient_y y) x
      in
      Pixel.of_int new_pixel_val)
  in
  let x_kernel_sum = 0 in
  let x_kernel =
    Image.foldi
      x_direction_kernel
      ~init:x_kernel_sum
      ~f:(fun ~x:_ ~y:_ x_kernel_sum pixel -> x_kernel_sum + Pixel.red pixel)
  in
  let y_kernel_sum = 0 in
  let y_kernel =
    Image.foldi
      y_direction_kernel
      ~init:y_kernel_sum
      ~f:(fun ~x:_ ~y:_ y_kernel_sum pixel -> y_kernel_sum + Pixel.red pixel)
  in
  let float_magnitude =
    Float.sqrt
      (Float.square (Float.of_int x_kernel)
       +. Float.square (Float.of_int y_kernel))
  in
  (* printf "%d" (Int.of_float float_magnitude); print_endline ""; *)
  Int.of_float float_magnitude
;;

(* Given an image, returns the detected edges of the said image *)
let _edge_detection ~image ~threshold_percentage =
  let threshold_int =
    Int.of_float (threshold_percentage *. Float.of_int (Image.max_val image))
  in
  let gray_img = Blur.transform (Grayscale.transform image) ~radius:2 in
  let img_with_magnitudes =
    Image.mapi gray_img ~f:(fun ~x ~y _ ->
      let x_start, x_end, y_start, y_end = get_valid_coords image x y 1 in
      let kernel =
        Image.slice
          image
          ~x_start
          ~x_end:(x_end + 1)
          ~y_start
          ~y_end:(y_end + 1)
      in
      Pixel.of_int (find_magnitude kernel))
  in
  Image.mapi img_with_magnitudes ~f:(fun ~x:_ ~y:_ pixel ->
    let curr_pixel_val = Pixel.red pixel in
    let new_pixel_val =
      match curr_pixel_val > threshold_int with
      | true -> Image.max_val image
      | false -> 0
    in
    Pixel.of_int new_pixel_val)
;;

let _solarize ~image ~threshold_percentage =
  let threshold_int =
    Int.of_float (threshold_percentage *. Float.of_int (Image.max_val image))
  in
  Image.map image ~f:(fun pixel ->
    let curr_red_val = Pixel.red pixel in
    let curr_green_val = Pixel.green pixel in
    let curr_blue_val = Pixel.blue pixel in
    let new_red_val =
      match curr_red_val > threshold_int with
      | true -> Image.max_val image - curr_red_val
      | false -> curr_red_val
    in
    let new_green_val =
      match curr_green_val > threshold_int with
      | true -> Image.max_val image - curr_green_val
      | false -> curr_green_val
    in
    let new_blue_val =
      match curr_blue_val > threshold_int with
      | true -> Image.max_val image - curr_blue_val
      | false -> curr_blue_val
    in
    new_red_val, new_green_val, new_blue_val)
;;

let get_random_region ~regions =
  Random.self_init ();
  let n = List.length regions in
  let random_region = List.nth regions (Random.int n) in
  match random_region with
  | None -> failwith "No regions found"
  | Some region -> region
;;

let divide_regions ~image ~width ~height =
  let height_divides = Image.height image / height in
  let width_divides = Image.width image / width in
  let grid_coords =
    List.init height_divides ~f:(fun height_number ->
      List.init width_divides ~f:(fun width_number ->
        width_number * width, height_number * height))
  in
  List.concat_map grid_coords ~f:(fun row ->
    List.map row ~f:(fun coord ->
      let x_start, y_start = coord in
      let x_end = Int.min (x_start + width) (Image.width image - 1) in
      let y_end = Int.min (y_start + height) (Image.height image - 1) in
      ( Image.slice image ~x_start ~x_end ~y_start ~y_end
      , x_start
      , x_end
      , y_start
      , y_end )))
;;

let squared_diff image1 image2 =
  let squared_diff_val = 0 in
  Image.foldi
    image1
    ~init:squared_diff_val
    ~f:(fun ~x ~y squared_diff_val pixel_1 ->
      let pixel_2 = Image.get image2 ~x ~y in
      squared_diff_val
      + Int.pow (Pixel.red pixel_2 - Pixel.red pixel_1) 2
      + Int.pow (Pixel.blue pixel_2 - Pixel.blue pixel_1) 2
      + (Int.pow (Pixel.green pixel_2 - Pixel.green pixel_1)) 2)
;;

let rec moasic ~image ~width ~height ~moves =
  (* printf "%d" moves; print_endline "reached"; *)
  match equal moves 0 with
  | true -> image
  | false ->
    let divided_regions = divide_regions ~image ~width ~height in
    let random_region = get_random_region ~regions:divided_regions in
    (* MSE, (x_start, x_end, ... , ...) *)
    let min_diff = Int.max_value, (0, 0, 0, 0) in
    let matched_region =
      List.fold divided_regions ~init:min_diff ~f:(fun min_diff region ->
        let region_image, x_start, x_end, y_start, y_end = region in
        let random_image, _, _, _, _ = random_region in
        let diff, _ = min_diff in
        let curr_diff = squared_diff region_image random_image in
        match curr_diff <= diff && not (equal curr_diff 0) with
        | true -> curr_diff, (x_start, x_end, y_start, y_end)
        | false -> min_diff)
    in
    let _, matched_coords = matched_region in
    let x_start, _, y_start, _ = matched_coords in
    let random_image, _, _, _, _ = random_region in
    let _ =
      List.init (Image.width random_image) ~f:(fun x_idx ->
        List.init (Image.height random_image) ~f:(fun y_idx ->
          Image.set
            image
            ~x:(x_idx + x_start)
            ~y:(y_idx + y_start)
            (Image.get random_image ~x:x_idx ~y:y_idx)))
    in
    moasic ~image ~width ~height ~moves:(moves - 1)
;;

let command =
  Command.basic
    ~summary:"Convert an image to mosaic"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let res_image = moasic ~image ~width:9 ~height:9 ~moves:10000 in
        Image.save_ppm
          res_image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;

(* let command = Command.basic ~summary:"Detect edges in an image"
   [%map_open.Command let filename = flag "filename" (required
   Command.Param.string) ~doc:"IMAGE_FILE the PPM image file" and threshold =
   flag "threshold" (required Command.Param.float) ~doc:"threshold for which
   to edge detect upon" in fun () -> let image = Image.load_ppm ~filename in
   let image' = solarize ~image ~threshold_percentage:threshold in
   Image.save_ppm image' ~filename: (String.chop_suffix_exn filename
   ~suffix:".ppm" ^ "_solarize.ppm")] ;; *)

(* Use the complement with the max_val to do the shifting from dark to light
   and light to dark *)
(* let transform image = let max_val = Image.max_val image in let gray_img =
   Grayscale.transform image in Image.map gray_img ~f:(fun pixel -> let
   current_pixel_val = Pixel.red pixel in

   )

   ;; *)
