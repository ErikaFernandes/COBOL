      ******************************************************************
      * Author: Erika Tavares Fernandes
      * Date: 03/10/2018
      * Purpose: Study and practice Cobol
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. My-Diary.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MY-NAME PIC A(30).


       01 MY-DAY OCCURS 20 TIMES INDEXED BY NUMBER-DAY.
           05 DAY-ID          PIC 9(20).
           05 DAY-DATE.
               10 YYYY        PIC X(4).
               10   MM        PIC X(2).
               10   DD        PIC X(2).
           05 DAY-DESCRIPTION PIC X(150).

       01 WS-DD-MM-YYYY .
           05 DD               PIC X(2).
           05 FILLER           PIC X(1) VALUE '/'.
           05 MM               PIC X(2).
           05 FILLER           PIC X(1) VALUE '/'.
           05 YYYY             PIC X(4).

       01 MY-CONTACTS OCCURS 20 TIMES INDEXED BY NUMBER-CONTACT.
           05 CONTACT-ID           PIC 9(20).
           05 CONTACT-NAME         PIC X(15).
           05 CONTACT-AGE          PIC 9(2).
           05 CONTACT-PHONE-NUMBER PIC 9(11).
           05 CONTACT-HOUSE-NUMBER PIC 9(3).
           05 CONTACT-STREET       PIC X(15).
           05 CONTACT-CITY         PIC X(15).
           05 CONTACT-COUNTRY      PIC X(15).
           05 CONTACT-EMAIL        PIC X(50).
           05 CONTACT-INSTAGRAM    PIC X(30).


       01 MY-TASKS OCCURS 20 TIMES INDEXED BY NUMBER-TASK.
           05 TASK-ID                 PIC X(20).
           05 TASK-NAME               PIC X(30).
           05 TASK-DESCRIPTION        PIC X(150).
           05 TASK-STATUS             PIC 9(1).
           05 TASK-ST-DESCTIP         PIC X(15).


       01 NUMBER-FIRST-MENU    PIC 9(2)  VALUE 13.
       01 SEARCH-NAME-CONTACT  PIC X(15).
       01 SEARCH-NAME-TASK     PIC X(30).
       01 SEARCH-ID-REG      PIC X(10).

       01 NUMBER-CONTACT-AUX            PIC 9(2) VALUE 1.

       01 NUMBER-TASK-AUX               PIC 9(2) VALUE 1.
       01 TASK-ID-AUX                   PIC X(20).

       01 NUMBER-DAY-REGISTER-AUX       PIC 9(2) VALUE 1.

       01 RETURN-RESPONSE               PIC 9(1) VALUE 1.

       PROCEDURE DIVISION.

       SET NUMBER-CONTACT TO 1.
       SET NUMBER-DAY TO 1.
       SET NUMBER-TASK TO 1.

       ENTRY-MY-DIARY.
           DISPLAY "HELLO, I'M YOUR DIARY, PLEASE ENTRY YOUR NAME: ".
           ACCEPT MY-NAME.
           DISPLAY "HELLO "MY-NAME.
           PERFORM MENU WITH TEST AFTER UNTIL NUMBER-FIRST-MENU = 11.

           STOP RUN.
       MENU.

           PERFORM SPACE-BETWEEN-TEXT
           DISPLAY "NOW, WHAT DO YOU WANNA DO?"
           DISPLAY "1 - INSERT NEW CONTACT".
           DISPLAY "2 - REGISTER YOUR DAY".
           DISPLAY "3 - REGISTER A NEW TASK".
           DISPLAY "4 - SEARCH A CONTACT".
           DISPLAY "5 - SEARCH A TASK".
           DISPLAY "6 - CHANGE TASK STATUS".
           DISPLAY "7 - LIST CONTACTS".
           DISPLAY "8 - LIST TASKS".
           DISPLAY "9 - LIST DAY REGISTER".
           DISPLAY "10 - CLOSE THE DIARY".

           DISPLAY "PLEASE, TYPE A NUMBER: "
           ACCEPT NUMBER-FIRST-MENU.

           EVALUATE TRUE
                WHEN NUMBER-FIRST-MENU = 1
                 PERFORM CAD-NEW-CONTACT

                WHEN NUMBER-FIRST-MENU = 2
                 PERFORM CAD-NEW-REGISTER-DAY

                WHEN NUMBER-FIRST-MENU = 3
                 PERFORM CAD-NEW-TASK

                WHEN NUMBER-FIRST-MENU = 4
                 PERFORM SEARCH-A-CONTACT

                WHEN NUMBER-FIRST-MENU = 5
                 PERFORM SEARCH-A-TASK

                WHEN NUMBER-FIRST-MENU = 6
                 PERFORM CHANGE-TASK-STATUS

                WHEN NUMBER-FIRST-MENU = 7
                 PERFORM LIST-CONTATCTS

                WHEN NUMBER-FIRST-MENU = 8
                 PERFORM LIST-TASKS

                WHEN NUMBER-FIRST-MENU = 9
                 PERFORM LIST-DAY-REGISTER

                WHEN NUMBER-FIRST-MENU = 10
                 STOP RUN

                WHEN OTHER
                 PERFORM SPACE-BETWEEN-TEXT
                 DISPLAY "INVALID VALUE"

           END-EVALUATE.

       CAD-NEW-CONTACT.
                 PERFORM SPACE-BETWEEN-TEXT
                 DISPLAY "ENTER THE NAME CONTACT ".
                 ACCEPT CONTACT-NAME(NUMBER-CONTACT).
                 DISPLAY "ENTER THE AGE CONTACT ".
                 ACCEPT CONTACT-AGE(NUMBER-CONTACT).
                 DISPLAY "ENTER THE PHONE NUMBER ".
                 ACCEPT CONTACT-PHONE-NUMBER(NUMBER-CONTACT).
                 DISPLAY "ENTER THE HOUSE NUMBER ".
                 ACCEPT CONTACT-HOUSE-NUMBER(NUMBER-CONTACT).
                 DISPLAY "ENTER THE STREET ".
                 ACCEPT CONTACT-STREET(NUMBER-CONTACT).
                 DISPLAY "ENTER THE CITY ".
                 ACCEPT CONTACT-CITY(NUMBER-CONTACT).
                 DISPLAY "ENTER THE COUNTRY ".
                 ACCEPT CONTACT-COUNTRY(NUMBER-CONTACT).
                 DISPLAY "ENTER THE EMAIL ".
                 ACCEPT CONTACT-EMAIL(NUMBER-CONTACT).
                 DISPLAY "ENTER THE INSTAGRAM ".
                 ACCEPT CONTACT-INSTAGRAM(NUMBER-CONTACT).
                 MOVE NUMBER-CONTACT TO CONTACT-ID(NUMBER-CONTACT).
                 SET NUMBER-CONTACT UP BY 1.

       CAD-NEW-REGISTER-DAY.
                 PERFORM SPACE-BETWEEN-TEXT
                 DISPLAY  "ENTER THE DESCRIPTION ".
                 ACCEPT DAY-DESCRIPTION(NUMBER-DAY).
                 ACCEPT DAY-DATE(NUMBER-DAY) FROM DATE YYYYMMDD.
                 DISPLAY DAY-DATE(NUMBER-DAY).
                 MOVE NUMBER-DAY TO DAY-ID(NUMBER-DAY).
                 SET NUMBER-DAY UP BY 1.

       CAD-NEW-TASK.
                 PERFORM SPACE-BETWEEN-TEXT
                 DISPLAY "ENTER TASK NAME ".
                 ACCEPT TASK-NAME(NUMBER-TASK).
                 DISPLAY "ENTER TASK DESCRIPTION ".
                 ACCEPT TASK-DESCRIPTION(NUMBER-TASK).
                 DISPLAY "ENTER TASK STATUS 0 - OK, 1 - NOT OK ".
                 ACCEPT TASK-STATUS(NUMBER-TASK).
                 IF TASK-STATUS(NUMBER-TASK) IS EQUAL TO 0
                    MOVE "TASK- OK" TO TASK-ST-DESCTIP(NUMBER-TASK)
                 ELSE
                    MOVE "TASK- NOT OK" TO TASK-ST-DESCTIP(NUMBER-TASK).
                 MOVE NUMBER-TASK TO TASK-ID(NUMBER-TASK).
                 SET NUMBER-TASK UP BY 1.

       SEARCH-A-CONTACT.
             PERFORM SPACE-BETWEEN-TEXT
             DISPLAY "ENTER SEARCH BY NAME ".
             ACCEPT SEARCH-NAME-CONTACT.
             PERFORM ENG-SEARCH-CONTACT VARYING NUMBER-CONTACT-AUX
                FROM 1 BY 1 UNTIL NUMBER-CONTACT-AUX=NUMBER-CONTACT.
                IF RETURN-RESPONSE = 1
                    DISPLAY "CONTACT NOT FOUND"
                ELSE
                    MOVE 1 TO RETURN-RESPONSE.

       ENG-SEARCH-CONTACT.
             IF CONTACT-NAME(NUMBER-CONTACT-AUX) = (SEARCH-NAME-CONTACT)
                 MOVE 0 TO RETURN-RESPONSE
                 PERFORM CONTACT-INF-LIST
             ELSE
                 IF NUMBER-CONTACT-AUX = NUMBER-CONTACT
                 MOVE 1 TO RETURN-RESPONSE.

       SEARCH-A-TASK.
             PERFORM SPACE-BETWEEN-TEXT
             DISPLAY "ENTER SEARCH BY TASK NAME ".
             ACCEPT SEARCH-NAME-TASK.
             PERFORM ENG-SEARCH-TASK VARYING NUMBER-TASK-AUX
                FROM 1 BY 1 UNTIL NUMBER-TASK-AUX=NUMBER-TASK.
                IF RETURN-RESPONSE = 1
                    DISPLAY "TASK NOT FOUND"
                ELSE
                    MOVE 1 TO RETURN-RESPONSE.

       ENG-SEARCH-TASK.
             IF TASK-NAME(NUMBER-TASK-AUX) = (SEARCH-NAME-TASK)
                 MOVE 0 TO RETURN-RESPONSE
                 PERFORM TASK-INF-LIST
              ELSE
                 IF NUMBER-TASK-AUX = NUMBER-TASK
                 MOVE 1 TO RETURN-RESPONSE.


       CHANGE-TASK-STATUS.
             PERFORM SPACE-BETWEEN-TEXT
             DISPLAY "ENTER THE TASK ID: ".
             ACCEPT NUMBER-TASK-AUX.
             PERFORM TASK-INF-LIST.
             PERFORM SPACE-BETWEEN-TEXT.
             DISPLAY "ENTER TASK STATUS 0 - OK, 1 - NOT OK ".
              ACCEPT TASK-STATUS(NUMBER-TASK-AUX).
               IF TASK-STATUS(NUMBER-TASK-AUX) IS EQUAL TO 0
                MOVE "TASK- OK" TO TASK-ST-DESCTIP(NUMBER-TASK-AUX)
               ELSE
                MOVE "TASK- NOT OK" TO TASK-ST-DESCTIP(NUMBER-TASK-AUX).
             PERFORM TASK-INF-LIST.

       LIST-CONTATCTS.
             IF CONTACT-ID(1) IS EQUAL TO 0
               DISPLAY "YOU DONT HAVE CONCTACTS REGISTERED"
             ELSE
               PERFORM CONTACT-INF-LIST VARYING NUMBER-CONTACT-AUX
                FROM 1 BY 1 UNTIL NUMBER-CONTACT-AUX=NUMBER-CONTACT.


       CONTACT-INF-LIST.
           PERFORM SPACE-BETWEEN-TEXT.

           DISPLAY "ID: " CONTACT-ID(NUMBER-CONTACT-AUX).
           DISPLAY "NAME: " CONTACT-NAME(NUMBER-CONTACT-AUX).
           DISPLAY "AGE: " CONTACT-NAME(NUMBER-CONTACT-AUX).
           DISPLAY "PHONE NUMBER: "
           CONTACT-PHONE-NUMBER(NUMBER-CONTACT-AUX).
           DISPLAY "HOUSE NUMBER: "
           CONTACT-HOUSE-NUMBER(NUMBER-CONTACT-AUX).
           DISPLAY "STREET: " CONTACT-STREET(NUMBER-CONTACT-AUX).
           DISPLAY "CITY: " CONTACT-CITY(NUMBER-CONTACT-AUX).
           DISPLAY "COUNTRY: " CONTACT-COUNTRY(NUMBER-CONTACT-AUX).
           DISPLAY "EMAIL: " CONTACT-EMAIL(NUMBER-CONTACT-AUX).
           DISPLAY "INSTAGRAM: " CONTACT-INSTAGRAM(NUMBER-CONTACT-AUX).

       LIST-TASKS.
               PERFORM TASK-INF-LIST VARYING NUMBER-TASK-AUX
                FROM 1 BY 1 UNTIL NUMBER-TASK-AUX=NUMBER-TASK.


       LIST-DAY-REGISTER.
           IF DAY-ID(1) IS EQUAL TO 0
               DISPLAY "YOU DONT HAVE REGISTER "
             ELSE
               PERFORM DAY-REG-INF-LIST VARYING NUMBER-DAY-REGISTER-AUX
                  FROM 1 BY 1
                UNTIL NUMBER-DAY-REGISTER-AUX=NUMBER-DAY.

       TASK-INF-LIST.
               PERFORM SPACE-BETWEEN-TEXT.

               DISPLAY "TASK ID : " TASK-ID(NUMBER-TASK-AUX).
               DISPLAY "TASK NAME: " TASK-NAME(NUMBER-TASK-AUX).
               DISPLAY "TASK DESCRIPTION: "
               TASK-DESCRIPTION(NUMBER-TASK-AUX).
               DISPLAY "TASK-STATUS: " TASK-ST-DESCTIP(NUMBER-TASK-AUX).

       DAY-REG-INF-LIST.
               PERFORM SPACE-BETWEEN-TEXT.
               DISPLAY "DAY ID: " DAY-ID(NUMBER-DAY-REGISTER-AUX).
               MOVE CORRESPONDING
               DAY-DATE(NUMBER-DAY-REGISTER-AUX) TO WS-DD-MM-YYYY
               DISPLAY "DD/MM/YYYY: " WS-DD-MM-YYYY.
               DISPLAY "DAY DESCRIPTION: "
               DAY-DESCRIPTION(NUMBER-DAY-REGISTER-AUX).

       SPACE-BETWEEN-TEXT.
           DISPLAY " ".

       END PROGRAM My-Diary.
