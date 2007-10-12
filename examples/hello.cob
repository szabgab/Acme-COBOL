      * Sample COBOL program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       01 Name     PIC X(5).
       PROCEDURE DIVISION.
       DISPLAY "Please type in your name".
       ACCEPT Name.
       DISPLAY "Hello ", Name, " how are you?".
       STOP RUN.
