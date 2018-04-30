### Feedback for Lab 05

Run on February 16, 15:38:30 PM.

+ Pass: Change into directory "Lab_05".

+ Pass: Check that file "higher.ml" exists.

+ Fail: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file higher.ml has errors. Type checking does not terminate.

    Run "ocaml higher.ml" is unable to complete. Some error in the file is breaking Ocaml's type checking, causing it to not terminate.

    Make sure that you are using ocaml version 4.06.  Run "ocaml -version" to check the version number.

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

+ Skip: You should remove unnecessary ;;

  This test was not run because of an earlier failing test.

+ Pass: Check that file "puzzle.ml" exists.

+ Fail: Check that an OCaml file "puzzle.ml" has no syntax or type errors.

    OCaml file puzzle.ml has errors. Type checking does not terminate.

    Run "ocaml puzzle.ml" is unable to complete. Some error in the file is breaking Ocaml's type checking, causing it to not terminate.

    Make sure that you are using ocaml version 4.06.  Run "ocaml -version" to check the version number.

+ Skip: Make sure you are only using recursion in functions read_file, read_chars

   

  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt"
   ```
   matches the pattern `["planet"; "smooth"; "change"; "twenty"; "knight"; "street"; "prefix"; "abrade"; "rawest"; "wonton"; "scrawl"; "misled"; "presto"; "upland"; "safari"]`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   pretty_answers (answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt")
   ```
   matches the pattern `[("lane", "planet"); ("moot", "smooth"); ("hang", "change"); ("went", "twenty"); ("nigh", "knight"); ("tree", "street"); ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest"); ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled"); ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]`.

   


  This test was not run because of an earlier failing test.

+ Skip: You should remove unnecessary ;;

  This test was not run because of an earlier failing test.

+ Pass: Check that file "formatter.ml" exists.

+ Fail: Check that an OCaml file "formatter.ml" has no syntax or type errors.

    OCaml file formatter.ml has errors. Type checking does not terminate.

    Run "ocaml formatter.ml" is unable to complete. Some error in the file is breaking Ocaml's type checking, causing it to not terminate.

    Make sure that you are using ocaml version 4.06.  Run "ocaml -version" to check the version number.

+ Skip: Make sure you are only using recursion in functions read_file, read_chars, explode, f

   

  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   format p1 12
   ```
   matches the pattern `"Hello world!
How are you
today? I
hope all is
well."`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
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

   


  This test was not run because of an earlier failing test.

+ Skip: 
Check that the result of evaluating
   ```
   format p1 20
   ```
   matches the pattern `"Hello world! How are
you today? I hope
all is well."`.

   


  This test was not run because of an earlier failing test.

+ Skip: 
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

   


  This test was not run because of an earlier failing test.

+ Skip: You should remove unnecessary ;;

  This test was not run because of an earlier failing test.

