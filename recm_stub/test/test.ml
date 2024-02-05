open OUnit2
open Rec.Interp
open Rec.Ds

(* Tests for SA1 *)
let tests_rec = [

  "letrec_even_odd" >:: (fun _ -> assert_equal
                            (* ~printer:string_of_expval *)
                            (Ok (NumVal 1))
                            (interp
                               "letrec
                                  even(x) = if zero?(x) then 1 else (odd (x-1))
                                  odd(x) = if zero?(x) then 0 else (even (x-1))
                                in (odd 99)"));

  "letrec_factorial" >:: (fun _ -> assert_equal
                             (* ~printer:string_of_expval *)
                             (Ok (NumVal 120))
                             (interp
                               "letrec
                                  fac(x) = if zero?(x) then 1 else x*(fac (x-1))
                                in (fac 5)"));

  "letrec_factorial2" >:: (fun _ -> assert_equal
                              (* ~printer:string_of_expval *)
                              (Ok (NumVal 120))
                              (interp
                                "letrec
                                    facc1(x) = x*(facc(x-1))
                                    facc(x) = if zero?(x) then 1 else (facc1 (x))
                                  in (facc 5)"));

  "letrec_sum" >:: (fun _ -> assert_equal
                              (* ~printer:string_of_expval *)
                              (Ok (NumVal 15))
                              (interp
                                "letrec
                                    mysum1(x) = x+(mysum(x-1))
                                    mysum(x) = if zero?(x) then 0 else (mysum1 (x))
                                  in (mysum 5)"));

  "letrec_fib" >:: (fun _ -> assert_equal
                      (* ~printer:string_of_expval *)
                      (Ok (NumVal 55))
                      (interp
                      "letrec
                      fib2(n) =  (fib (n-2)) 
                      fib1(n) =  (fib (n-1))
                      fib(n) = 
                      if zero?(n)
                      then 0
                      else (if zero?(n-1)
                      then 1
                        else 
                          (fib1 n) + (fib2 n))
                      in (fib 10)"));

 "letrec_list1" >:: (fun _ -> assert_equal
    (Ok (ListVal
    [NumVal 10; NumVal 9; NumVal 8; NumVal 7; NumVal 6; NumVal 5; NumVal 4;
     NumVal 3; NumVal 2; NumVal 1]))
    (interp 
    "letrec
      even(x) = if zero?(x) then emptylist else cons(x, (odd (x-1)))
      odd(x) = if zero?(x) then emptylist else cons(x, (even (x-1)))
    in (odd 10)")
  );

  "letrec_list2" >:: (fun _ -> assert_equal
    (Ok (ListVal
    [NumVal 1; NumVal 3; NumVal 4; NumVal 6; NumVal 7; NumVal 9; NumVal 10;
     NumVal 12; NumVal 13; NumVal 15]))
    (interp 
    "letrec
      f(x) = proc (y) { if zero?(x) then emptylist else cons(y, ((g (x-1)) (y+2))) }
      g(x) = proc (y) { if zero?(x) then emptylist else cons(y, ((f (x-1)) (y+1))) }
    in ((f 10) 1)")
  );

  "letrec_list3" >:: (fun _ -> assert_equal
    (Ok (ListVal [NumVal 0; NumVal 1; NumVal 2; NumVal 3]))
    (interp 
    "letrec
      f(x) = proc (y) { if zero?(x) then emptylist else cons(y, ((g (x/2)) (y+1))) }
      g(x) = proc (y) { if zero?(x) then emptylist else cons(y, ((f (x/3)) (y+1))) }
    in ((f 20) 0)")
  );

  "letrec_list4" >:: (fun _ -> assert_equal
    (Ok (NumVal 60))
    (interp 
    "letrec
      f(xs) = if empty?(xs) then 0 else (hd (xs)) + (g (tl (xs)))
      g(xs) = if empty?(xs) then 0 else (hd (xs)) + (f (tl (xs)))
    in (f (cons(4, cons(8, cons(16, cons(32, emptylist))))))")
  );

  "letrec_fa1" >:: (fun _ -> assert_equal
    (Ok (ListVal
    [NumVal 1; NumVal 2; NumVal 1; NumVal 2; NumVal 1; NumVal 2; NumVal 1;
     NumVal 2; NumVal 1; NumVal 2]))
    (interp 
    "letrec
      q0(x) = if zero?(x) then emptylist else cons(1, (q1 (x-1)))
      q1(x) = if zero?(x) then emptylist else cons(2, (q0 (x-1)))
    in (q0 10)")
  );

  "letrec_fa2" >:: (fun _ -> assert_equal
    (Ok (ListVal
    [NumVal 0; NumVal 1; NumVal 2; NumVal 0; NumVal 1; NumVal 2; NumVal 0;
     NumVal 1]))
    (interp 
    "letrec
      q0(x) = if zero?(x) then emptylist else cons(0, (q1 (x-1)))
      q1(x) = if zero?(x) then emptylist else cons(1, (q2 (x-1)))
      q2(x) = if zero?(x) then emptylist else cons(2, (q0 (x-1)))
    in (q0 8)")
  );

  "letrec_fa3" >:: (fun _ -> assert_equal
    (Ok (ListVal
    [NumVal 15; NumVal (-14); NumVal 13; NumVal (-12); NumVal 11; NumVal (-10);
    NumVal 9; NumVal (-8); NumVal 7; NumVal (-6); NumVal 5; NumVal (-4);
    NumVal 3; NumVal (-2); NumVal 1]))
    (interp 
    "letrec
      q0(x) = proc (y) { if zero?(x) then emptylist else cons(x*y, ((q1 (x-1)) (y*(-1)))) }
      q1(x) = proc (y) { if zero?(x) then emptylist else cons(x*y, ((q2 (x-1)) (y*(-1)))) }
      q2(x) = proc (y) { if zero?(x) then emptylist else cons(x*y, ((q0 (x-1)) (y*(-1)))) }
    in ((q0 15) 1)")
  );
   
]

let _ = run_test_tt_main ("suite" >::: (tests_rec))
