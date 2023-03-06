open OUnit2
open Feistel.BitSet

let () = Random.self_init ()
let nb_repetitions = 100

let rec repeat times fct =
  if times > 0 then (
    fct ();
    repeat (times - 1) fct
  )

let test_zeros =
  assert_equal true (is_empty (zeros 0));
  assert_raises (Invalid_argument "Negative size provided") (fun () ->
      zeros (-3));
  repeat nb_repetitions (fun () ->
      let rnd_int = Random.int 1024 in
      assert_equal rnd_int (size (zeros rnd_int)))

let test_to_string =
  repeat nb_repetitions (fun () ->
      let rnd_int = Random.int 1024 in
      assert_equal (String.make rnd_int '0') (to_string (zeros rnd_int)))

let test_from_int =
  assert_raises (Invalid_argument "integer must be positive") (fun () ->
      from_int (-1) 0);
  repeat nb_repetitions (fun () ->
      let size = 10 + Random.int 9 in
      let rnd_int = 2 + Random.int 1024 in
      assert_equal rnd_int
        (int_of_string ("0b" ^ to_string (from_int rnd_int size)));
      assert_equal (to_string (zeros size)) (to_string (from_int 0 size));
      assert_raises (Invalid_argument "size too small") (fun () ->
          from_int rnd_int 1))

let suite =
  "Test Stack"
  >::: [
         ("test_zeros" >:: fun _ -> test_zeros);
         ("test_clear" >:: fun _ -> test_to_string);
         ("test_pop" >:: fun _ -> test_from_int);
       ]

let () = run_test_tt_main suite
