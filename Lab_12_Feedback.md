### Feedback for Lab 12

Run on April 13, 14:29:29 PM.

+ Pass: Change into directory "Lab_12".

+ Pass: Check that file "interpreter.ml" exists.

+ Pass: Check that an OCaml file "interpreter.ml" has no syntax or type errors.

    OCaml file "interpreter.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   lookup "sum_evens" (exec program_while_ifthenelse_test [])
   ```
   matches the pattern `Int 30`.

   




+ Pass: 
Check that the result of evaluating
   ```
   lookup "y" (exec program_ifthen [("x",Int 4)])
   ```
   matches the pattern `Int 6`.

   




+ Fail: 
Check that the result of evaluating
   ```
   lookup "sum" (exec program_sum_10 [])
   ```
   matches the pattern `Int 55`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value program_sum_10
`


