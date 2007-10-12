      * Sample COBOL program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adding.
       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       01 Num1     PIC 9(5).
       01 Num2     PIC 9(5).
       01 Result   PIC 9(5).
       PROCEDURE DIVISION.
       DISPLAY "Please type in a number".
       ACCEPT Num1.
       DISPLAY "Please type in a number".
       ACCEPT Num2.
       COMPUTE Result = Num1+Num2.
       DISPLAY Num1, "+", Num2, "=", Result.
       STOP RUN.
