

;; LOAD VL COMMANDS
(vl-load-com)

(defun C:FindMeXYZ                    ( / *error*   MtextEnt   vlaMtextObj   vlaLeaderObj   boxEnt
                                      originalElevation  flashEnt       justificationCharacters
                                      annotationStringNumber  justificationNumber  tailLocation
                                      drawBox  boxEnt  vlaBoxObj  continue  firstClick  grIn
                                      gr1  gr2  cursorPt  selectionPt  leaderPointsVariant
                                      textString  boxMinPt  MinPt  boxMaxPt  MaxPt  pt1  pt2
                                      pt3  pt4  arrowTailPt  tailLocation  MtextCoordinates
                                      leaderPointsSafeArray)

  ;; ERROR HANDLING
  (defun *error* ( msg / )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ "\nThis object is not yet supported\n") ;; (strcat "\nError: " msg))
    );_if

    ;; DELETE ANY ENTITIES THAT MAY HAVE BEEN CREATED
    (if MtextEnt (entdel MtextEnt))
    (if vlaLeaderObj (entdel (vlax-vla-object->ename vlaLeaderObj)))
    (if boxEnt (entdel boxEnt))

    ;; RETURN THE USER'S SETTINGS: ELEVATION and CMDECHO
    (setvar "ELEVATION" originalElevation)
    (setvar "CMDECHO" originalCMDECHO)

    ;; EXIT QUIETLY
    (princ "\n")
    (exit)
      
  );_defun *error*
    
  ;; SUBROUTINES

  ;; #1
  ;; FUNCTION TO SIMULATE ENTMOD WHEN ENTMOD DOESN'T SEEM TO DO ANYTHING:
  ;; MAKES A NEW ENTITY, DELETES THE OLD ONE, UPDATES PROGRAM GLOBAL MTEXTENT AND VLAMTEXTOBJ VALUES
  ;; IN elist WITH ENTITY NAME
  (defun flashEnt ( elist / )

    (entdel (cdr(assoc -1 elist)))
    (entmake (cdr elist))
    (setq MtextEnt    (entlast))
    (setq vlaMtextObj (vlax-ename->vla-object MtextEnt))
      
  );_defun flashEnt

  ;; PROGRAM START ----------------------------------------


  ;; RECORD USER'S SETTINGS AND RETURN THEM AT THE END OF THE FUNCTION
  (setq originalElevation (getvar "ELEVATION"))
  (setq originalCMDECHO (getvar "CMDECHO"))

  ;; TURN OFF CMDECHO
  (setvar "CMDECHO" 0)
    
  ;; SELECT THE LINE ENTITY
  (if (setq selection (entsel "\nSelect an Object:"))
    (progn
      (princ)
      (setq vlaCurveObj (vlax-ename->vla-object (car selection)))
      (setq selectionPt (cadr selection))
        
      ;; CREATE AN MTEXT AND LEADER OBJECT
      ;; MTEXT OBJECT:
      (entmake
        (list
          (cons 0 "MTEXT")
          (cons 5 "24F")
          (cons 100 "AcDbEntity")
          (cons 67 0)
          (cons 410 "Model")
          (cons 8 "0")
          (cons 100 "AcDbMText")
          (cons 10 selectionPt)
          (cons 40 2.5)
          (cons 41 20)
          (cons 46 0.0)
          (cons 71 1)
          (cons 72 5)
          (cons 1 (strcat "X = " (rtos (car   selectionPt)2 2)
                        "\nY = " (rtos (cadr  selectionPt)2 2)
                        "\nZ = " (rtos (caddr selectionPt)2 2)))
          (cons 7 "Standard")
          (cons 210 (list 0.0 0.0 1.0))
          (cons 11 (list 1.0 0.0 0.0))
          (cons 42 20)
          (cons 43 20)
          (cons 50 0.0)
          (cons 73 1)
          (cons 44 1.0)
        );_list
      );_entmake

      (setq MtextEnt (entlast))  
      (setq vlaMtextObj (vlax-ename->vla-object MtextEnt))

      ;; LEADER OBJECT:
      (entmake
        (list
          (cons 0 "LEADER")
          (cons 5 "253")
          (cons 100 "AcDbEntity")
          (cons 67 0)
          (cons 410 "Model")
          (cons 8 "0")
          (cons 100 "AcDbLeader")
          (cons 3 "ISO-25")
          (cons 71 1)
          (cons 72 0)
          (cons 73 3)
          (cons 74 1)
          (cons 75 0)
          (cons 40 0.0)
          (cons 41 0.0)
          (cons 76 2)
          (cons 10 selectionPt)
          (cons 10 (list (+ (car selectionPt) 1) (cadr selectionPt) (caddr selectionPt)))
          ;; (cons 340 <Entity name: 0>)
          (cons 211 (list 1.0 0.0 0.0))
          (cons 210 (list 0.0 0.0 1.0))
          (cons 212 (list 0.0 0.0 0.0))
          (cons 213 (list 0.0 0.0 0.0))
        );_list
      );_entmake

      (setq vlaLeaderObj (vlax-ename->vla-object (entlast)))

      ;; DEFINE JUSTIFICATION VALUES
      (setq justificationCharacters
        (list
          (cons 0 "TL")
          (cons 1 "TC")
          (cons 2 "TR")
          (cons 3 "ML")
          (cons 4 "MC")
          (cons 5 "MR")
          (cons 6 "BL")
          (cons 7 "BC")
          (cons 8 "BR")
        );_list
      );_setq

      ;; SET SOME DEFAULTS
      (setq annotationStringNumber 0)
      (setq justificationNumber 0)
      (setq tailLocation 0)
      (setq drawBox nil)
      (setq boxEnt nil)
      (setq vlaBoxObj nil)
        
      ;; MAIN LOGIC
      (setq continue T)
      (setq firstClick nil)

      ;; SHOW THE COMMANDS TO THE USER
      (princ "\nKEYBOARD INPUTS:
              \n-/+ : DECREASE/INCREASE text size
              \n[/] : DECREASE/INCREASE arrow head size
              \n</> : DECREASE/INCREASE text rotation
              \n 1  : TOGGLE text alignment
              \n 2  : TOGGLE box around text
              \n 3  : CYCLE arrow attachment point
              \n 4  : CYCLE text content")
        
      (while continue

        ;; FETCH THE CURSOR INPUT
        (setq grIn (grread t 15 0))
        (setq gr1  (car  grIn))
        (setq gr2  (cadr grIn))

        ;; INITIALLY A CODE 13 IS GIVEN USE THE SELECTION POINT
        (if (= gr1 13) (setq cursorPt selectionPt))
          
        ;; FETCH THE CURSOR COORDINATE IF A MOUSE MOVE OR CLICK WAS MADE
        (if (or (= gr1 3) (= gr1 5))
          (setq cursorPt (list (car gr2) (cadr gr2) (caddr gr2))))
          
        ;; DETERMINE THE CLOSEST POINT (UNTIL A FIRST CLICK HAS BEEN MADE)
        (if (not firstClick)
          (progn
            (setq closestPt (vlax-curve-getclosestpointto vlaCurveObj cursorPt))
            ;; ADJUST THE ELEVATION OF THE CURSOR
            (setvar "ELEVATION" (caddr closestPt))
          );_progn
        );_if
        
            
        ;; MOVE THE MTEXT OBJECT
        ;; USE THE CURSOR POINTS X AND Y COORDINATES BUT THE CLOSEST'S POINT Z COORDINATE
        (setq MtextCoordinates (list (car cursorPt) (cadr cursorPt) (caddr closestPt)))
        (vlax-put-property vlaMtextObj 'InsertionPoint (vlax-3d-point MtextCoordinates))


        ;; PREPARE BOUNDING BOX POINTS
        ;;(START WITH BOTTOM LEFT AND GO COUNTER CLOCKWISE)
        (vlax-invoke-method vlaMtextObj 'GetBoundingBox 'MinPt 'MaxPt)
        (setq boxMinPt (vlax-safearray->list MinPt))
        (setq boxMaxPt (vlax-safearray->list MaxPt))
        (setq pt1 (list (car boxMinPt) (cadr boxMinPt)))
        (setq pt2 (list (car boxMaxPt) (cadr boxMinPt)))
        (setq pt3 (list (car boxMaxPt) (cadr boxMaxPt)))
        (setq pt4 (list (car boxMinPt) (cadr boxMaxPt)))

        ;; MOVE THE BOX
        (if vlaBoxObj
          (progn
              
            ;; MAKE A SAFEARRAY OF THE POINTS
            (setq boxPointsSafeArray (vlax-make-safearray vlax-vbDouble '(0 . 7)))
          
            ;; POINTS
            (vlax-safearray-put-element boxPointsSafeArray 0 (car  pt1))
            (vlax-safearray-put-element boxPointsSafeArray 1 (cadr pt1))
            (vlax-safearray-put-element boxPointsSafeArray 2 (car  pt2))
            (vlax-safearray-put-element boxPointsSafeArray 3 (cadr pt2))
            (vlax-safearray-put-element boxPointsSafeArray 4 (car  pt3))
            (vlax-safearray-put-element boxPointsSafeArray 5 (cadr pt3))
            (vlax-safearray-put-element boxPointsSafeArray 6 (car  pt4))
            (vlax-safearray-put-element boxPointsSafeArray 7 (cadr pt4))

            ;; MAKE VARIANT
            (setq boxPointsVariant (vlax-make-variant boxPointsSafeArray))
            
            (vlax-put-property vlaBoxObj 'Coordinates boxPointsVariant)
            (vlax-put-property vlaBoxObj 'Elevation   (caddr closestPt))
            
          );_progn
        );_if
        
        ;; RELOCATE THE LEADER OBJECT
        (setq leaderPointsSafeArray (vlax-make-safearray vlax-vbDouble '(0 . 5)))
          
        ;; ARROWHEAD
        (vlax-safearray-put-element leaderPointsSafeArray 0 (car   closestPt))
        (vlax-safearray-put-element leaderPointsSafeArray 1 (cadr  closestPt))
        (vlax-safearray-put-element leaderPointsSafeArray 2 (caddr closestPt))

        ;; ARROWTAIL
        (setq arrowTailPt
          (cond

            ;; TOP LEFT
            ((= tailLocation 0) pt4)
            ;; TOP MIDDLE
            ((= tailLocation 1) (list (/ (+ (car pt3) (car pt4))2) (/ (+ (cadr pt3) (cadr pt4))2)))
            ;; TOP RIGHT
            ((= tailLocation 2) pt3)
            ;; RIGHT MIDDLE
            ((= tailLocation 3) (list (/ (+ (car pt2) (car pt3))2) (/ (+ (cadr pt2) (cadr pt3))2)))
            ;; BOTTOM RIGHT
            ((= tailLocation 4) pt2)
            ;; BOTTOM CENTER
            ((= tailLocation 5) (list (/ (+ (car pt1) (car pt2))2) (/ (+ (cadr pt1) (cadr pt2))2)))
            ;; BOTTOM LEFT
            ((= tailLocation 6) pt1)
            ;; BOTTOM MIDDLE
            ((= tailLocation 7) (list (/ (+ (car pt4) (car pt1))2) (/ (+ (cadr pt4) (cadr pt1))2)))

          );_cond
        );_setq arrowTailPt
        (vlax-safearray-put-element leaderPointsSafeArray 3 (car    arrowTailPt))
        (vlax-safearray-put-element leaderPointsSafeArray 4 (cadr   arrowTailPt))
        (vlax-safearray-put-element leaderPointsSafeArray 5 (caddr  closestPt))

        ;; MAKE VARIANT
        (setq leaderPointsVariant (vlax-make-variant leaderPointsSafeArray))

        ;; MOVE THE LEADER OBJECT
        (vlax-put-property vlaLeaderObj 'Coordinates leaderPointsVariant)

        ;; BUILD A TEXTSTRING
        (setq textString
          (cond
            ;; XYZ STACKED
            ((= annotationStringNumber 0) (strcat "X = " (makeCoordinateString (car   closestPt))
                                                "\nY = " (makeCoordinateString (cadr  closestPt))
                                                "\nZ = " (makeCoordinateString (caddr closestPt))))
            
            ;; XY STACKED
            ((= annotationStringNumber 1) (strcat "X = " (makeCoordinateString (car   closestPt))
                                                "\nY = " (makeCoordinateString (cadr  closestPt))))

            ;; (x,y,z)
            ((= annotationStringNumber 2) (strcat "(" (rtos (car   closestPt)2 2)
                                                  "," (rtos (cadr  closestPt)2 2)
                                                  "," (rtos (caddr closestPt)2 2) ")"))

            ;; (x,y)
            ((= annotationStringNumber 3) (strcat "(" (rtos (car  closestPt)2 2)
                                                  "," (rtos (cadr closestPt)2 2) ")"))

            ;; FROM START:
            ((= annotationStringNumber 4) (strcat "FROM"
                                                "\nSTART:"
                                                "\n" (rtos (vlax-curve-getDistAtPoint vlaCurveObj closestPt)2 2)))

            ;; FROM END:
            ((= annotationStringNumber 5) (strcat "FROM"
                                                "\nEND:"
                                                "\n"  (rtos (- (vlax-curve-getdistatpoint vlaCurveObj (vlax-curve-getendpoint vlaCurveObj))
                                                               (vlax-curve-getDistAtPoint vlaCurveObj closestPt))2 2)))
          );_cond
        );_setq textString
            
        ;; UPDATE THE MTEXT TEXT
        (vlax-put-property vlaMtextObj 'TextString textString)

        ;; PROCESS KEYBOARD INPUT
        (if (= 2 gr1)
          (cond

            ;; INCREASE TEXT SIZE WITH '+' AND '='
            ((member gr2 '(043 061))
              (progn
                  
                ;; INCREASE THE MASK SIZE
                (setq MtextEntList (entget MtextEnt))
                (setq maskSize (cdr (assoc 41 MtextEntList)))
                (flashEnt (subst (cons 41 (+ maskSize 10)) (assoc 41 MtextEntList) MtextEntList))
                         
                ;; INCREASE THE TEXT SIZE
                (vlax-put-property vlaMtextObj 'Height
                  (+ (vlax-get-property vlaMtextObj 'Height) 0.1))

                ;; TRIM THE MASK SIZE TO THE TEXT SIZE 42 IS WIDTH 43 IS HEIGHT
                (setq MtextEntList (entget MtextEnt))
                (setq textWidth (cdr (assoc 42 MtextEntList)))
                (flashEnt (subst (cons 41 (+ textWidth 0.8)) (assoc 41 MtextEntList) MtextEntList))
                
              );_progn
            );_(member gr2 '(043 061))

            ;; DECREASE TEXT SIZE WITH '-' AND '_'
            ((member gr2 '(045 095))
              (progn
                  
                ;; DECREASE THE TEXT SIZE
                (vlax-put-property vlaMtextObj 'Height
                  (- (vlax-get-property vlaMtextObj 'Height) 0.1))
                
                ;; TRIM THE MASK SIZE TO THE TEXT SIZE 42 IS WIDTH 43 IS HEIGHT
                (setq MtextEntList (entget MtextEnt))
                (setq textWidth (cdr (assoc 42 MtextEntList)))
                (flashEnt (subst (cons 41 textWidth) (assoc 41 MtextEntList) MtextEntList))
                
              );_progn
            );_(member gr2 '(045 095))

            ;; INCREASE ARROWHEAD SIZE WITH ']' and '}'
            ((member gr2 '(093 125))
              (progn
                (vlax-put-property vlaLeaderObj 'ArrowheadSize
                  (+ (vlax-get-property vlaLeaderObj 'ArrowheadSize) 0.1))
              );_progn
            );_(member gr2 '(093 125))

            ;; DECREASE ARROWHEAD SIZE WITH '[' and '{'
            ((member gr2 '(091 123))
              (progn
                (vlax-put-property vlaLeaderObj 'ArrowheadSize
                  (- (vlax-get-property vlaLeaderObj 'ArrowheadSize) 0.1))
              );_progn
            );_(member gr2 '(091 123))

            ;; INCREASE TEXT ROTATION WITH '.' and '>'
            ((member gr2 '(046 062))
              (progn
                (vlax-put-property vlaMtextObj 'Rotation
                  (+ (vlax-get-property vlaMtextObj 'Rotation) 0.01))
              );_progn
            );_(member gr2 '(046 062))

            ;; DECREASE TEXT ROTATION WITH ',' and '<'
            ((member gr2 '(044 060))
              (progn
                (vlax-put-property vlaMtextObj 'Rotation
                  (- (vlax-get-property vlaMtextObj 'Rotation) 0.01))
              );_progn
            );_(member gr2 '(044 060))
            
            ;; CHANGE TEXT JUSTIFICATION WITH '1'
            ((member gr2 '(049))
              (progn
                (setvar "CMDECHO" 0)
                (setq justificationNumber (rem (1+ justificationNumber) 9))
                (command "JUSTIFYTEXT" MtextEnt "" (cdr (assoc justificationNumber justificationCharacters)) )
                (setvar "CMDECHO" 1)
              );_progn
            );_(member gr2 '(049))

            ;; TOGGLE A BOX AROUND THE MTEXT WITH '2'
            ((member gr2 '(050))
              (progn
                   
                (if (setq drawBox (not drawBox))
                  ;; DRAW A NEW BOX
                  (progn

                    ;; TRIM THE MASK AS TIGHT AS POSSIBLE
                    (setq MtextEntList (entget MtextEnt))
                    (setq textWidth (cdr (assoc 42 MtextEntList)))
                    (flashEnt (subst (cons 41 textWidth) (assoc 41 MtextEntList) MtextEntList))

                    ;; DRAW THE BOX
                    (vlax-invoke-method vlaMtextObj 'GetBoundingBox 'MinPt 'MaxPt)
                    (setq boxMinPt (vlax-safearray->list MinPt))
                    (setq boxMaxPt (vlax-safearray->list MaxPt))
                    (command "RECTANGLE" boxMinPt boxMaxPt)

                    ;; ASSIGN THE BOX ENTITY
                    (setq boxEnt (entlast))
                    (setq vlaBoxObj (vlax-ename->vla-object boxEnt))
                     
                  );_progn
                  ;; DELETE THE OLD BOX
                  (progn
                    (entdel boxEnt)
                    (setq boxEnt nil)
                    (setq vlaBoxObj nil)
                  );_progn
                );_if
                   
              );_progn
            );_(member gr2 '(049))

            ;; CYCLE THROUGH THE LEADER TAIL POINT NUMBER WITH '3'
            ((member gr2 '(051))
              (progn
                (setq tailLocation (rem (1+ tailLocation) 8))
              );_progn
            );_(member gr2 '(051))

            ;; CYCLE THROUGH THE ANNOTATION STRING NUMBER WITH '4'
            ((member gr2 '(052))
              (progn
                (setq annotationStringNumber (rem (1+ annotationStringNumber) 6))
              );_progn
            );_(member gr2 '(052))

          );_cond
        );_if

        ;; PROCESS THE FIRST OR SECOND CLICK
        (if (= gr1 3)
          (if firstClick
            (setq continue nil)
            (setq firstClick T)
          );_if
        );_if
          
      );_while

    );_progn
    (princ "\nNothing Selected")
  );_if

  ;; RETURN THE USER'S ELEVATION
  (setvar "ELEVATION" originalElevation)
  (setvar "CMDECHO" originalCMDECHO)
    
  (princ)
  (princ)
    
);_ defun C:FindMeXYZ


(defun makeCoordinateString ( coordinate / decimalPosition)

  ;; TRANSFORM COORDINATE INTO A STRING
  (setq coordinateString (rtos coordinate 2 2))

  ;; IF THE FIRST CHARACTER IS NOT A '-' SIGN THEN ADD A SPACE
  (if (/= "-" (substr coordinateString 1 1))
    (setq coordinateString (strcat " " coordinateString)))

  ;; ENSURE TWO DECIMAL PLACES
  (if (setq decimalPosition (vl-string-search "." coordinateString 0))
    ;; ADD TRAILING ZEROS AS NEEDED
    (progn
      (setq additionalZeros (- 2(strlen (substr coordinateString (+ 2 (vl-string-search "." coordinateString 0))))))
      (while (> additionalZeros 0)
        (setq coordinateString (strcat coordinateString "0"))
        (setq additionalZeros (1- additionalZeros))
      );_while
    );_progn
      
    ;; FORCE A DECIMAL POINT AND TRAILING ZEROS
    (setq coordinateString (strcat coordinateString ".00"))
  );_if

  coordinateString
    
);_defun makeCoordinateString

(princ "\nFindMeXYZ v. 1.0 loaded")
(princ)