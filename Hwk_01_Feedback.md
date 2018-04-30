## Feedback for Homework 01

Run on February 13, 01:00:50 AM.

+ Pass: Change into directory "Hwk_01".

### Feedback for Part 1

+ Pass: Check that file "intro.ml" exists.

+ Pass: Check that an OCaml file "intro.ml" has no syntax or type errors.

    OCaml file "intro.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   even 4
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   even 5
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   euclid 6 9
   ```
   matches the pattern `3`.

   




+ Pass: 
Check that the result of evaluating
   ```
   euclid 5 9
   ```
   matches the pattern `1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (8,16)
   ```
   matches the pattern `(1,2)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (4,9)
   ```
   matches the pattern `(4,9)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (3,9)
   ```
   matches the pattern `(1,3)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   take 3 [1; 2; 3; 4; 5]
   ```
   matches the pattern `[1; 2; 3]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   take 0 ['a'; 'b'; 'c']
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   take (-2) ['a'; 'b'; 'c']
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   max [1; 2; 5; 3; 2]
   ```
   matches the pattern `5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   max [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-1`.

   




### Feedback for Part 2

+ Pass: Check that file "higher.ml" exists.

+ Fail: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file higher.ml has errors.

    Run "ocaml higher.ml" to see them.

    Make sure that you are using ocaml version 4.06.  Run "ocaml -version" to check the version number.

+ Skip: You are not allowed to use recursion.

   

  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   all_evens [1; 2; 3; 4; 5]
   ```
   matches the pattern `[2; 4]`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   all_evens [1; 3; 5]
   ```
   matches the pattern `[]`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   increment_all [1; 2; 3; 4; 5]
   ```
   matches the pattern `[2; 3; 4; 5; 6]`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   increment_all []
   ```
   matches the pattern `[]`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   max_fold [1; 2; 5; 3; 2]
   ```
   matches the pattern `5`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   max_fold [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-1`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3]
   ```
   matches the pattern `(6, 6)`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3; 4]
   ```
   matches the pattern `(10, 24)`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   sum_prod []
   ```
   matches the pattern `(0, 1)`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```
   matches the pattern `[[1; 2]; [4; 5]; [7; 8]; [10]]`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 3; 4; 5; 6; 6; 6; 7; 8; 9 ;10]
   ```
   matches the pattern `[[1; 2]; []; [4; 5]; []; []; [7; 8]; [10]]`.

   


  This test was not run because of an earlier failing test.

### Feedback for Part 3

+ Pass: Check that file "puzzle.ml" exists.

+ Pass: Check that an OCaml file "puzzle.ml" has no syntax or type errors.

    OCaml file "puzzle.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars

   



Note that if your output is in reverse order, it's technically still a correct answer. But to make grading simple, please make sure to match the pattern here.

+ Pass: 
Check that the result of evaluating
   ```
   answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt"
   ```
   matches the pattern `["planet"; "smooth"; "change"; "twenty"; "knight"; "street"; "prefix"; "abrade"; "rawest"; "wonton"; "scrawl"; "misled"; "presto"; "upland"; "safari"]`.

   




+ Fail: 
Check that the result of evaluating
   ```
   pretty_answers (answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt")
   ```
   matches the pattern `[("lane", "planet"); ("moot", "smooth"); ("hang", "change"); ("went", "twenty"); ("nigh", "knight"); ("tree", "street"); ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest"); ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled"); ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]`.

   


   Test failed. The following errors were reported:

   ```
 ;;
Characters 115-417:
  (pretty_answers (answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt")) = ([("lane", "planet"); ("moot", "smooth"); ("hang", "change"); ("went", "twenty"); ("nigh", "knight"); ("tree", "street"); ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest"); ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled"); ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]) ;; ;;
                                                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[1;31mError[0m: This expression has type 'a list
       but an expression was expected of type string list * string list

   ```


### Feedback for Part 4

+ Pass: Check that file "formatter.ml" exists.

+ Pass: Check that an OCaml file "formatter.ml" has no syntax or type errors.

    OCaml file "formatter.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars, explode, f

   



Note that you might not see the line breaks in the following patterns, because of the way feedback file is rendered. They are in the actual string.

+ Fail: 
Check that the result of evaluating
   ```
   format p1 12
   ```
   matches the pattern `"Hello world!
How are you
today? I
hope all is
well."`.

   


   Test failed. The following errors were reported:

   ```
 ;;        
Characters 17-72:
  .................("Hello world!
  How are you
  today? I
  hope all is
  well.")......
[1;31mError[0m: This expression has type string but an expression was expected of type
         unit

   ```


+ Fail: 
Check that the result of evaluating
   ```
   format p1 11
   ```
   matches the pattern `"Hello
world! How
are you
today? I
hope all is
well."`.

   


   Test failed. The following errors were reported:

   ```
 ;;          
Characters 17-72:
  .................("Hello
  world! How
  are you
  today? I
  hope all is
  well.")......
[1;31mError[0m: This expression has type string but an expression was expected of type
         unit

   ```


+ Fail: 
Check that the result of evaluating
   ```
   format p1 20
   ```
   matches the pattern `"Hello world! How are
you today? I hope
all is well."`.

   


   Test failed. The following errors were reported:

   ```
 ;;    
Characters 17-72:
  .................("Hello world! How are
  you today? I hope
  all is well.")......
[1;31mError[0m: This expression has type string but an expression was expected of type
         unit

   ```


+ Fail: 
Check that the result of evaluating
   ```
   format p1 4
   ```
   matches the pattern `"Hello
world!
How
are
you
today?
I
hope
all
is
well."`.

   


   Test failed. The following errors were reported:

   ```
 ;;                    
Characters 16-71:
  ................("Hello
  world!
  How
  are
  you
  today?
  I
  hope
  all
  is
  well.")......
[1;31mError[0m: This expression has type string but an expression was expected of type
         unit

   ```


