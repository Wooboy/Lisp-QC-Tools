    ;;----------------------------------------------------------------------;;
    ;; Quick check objects length/radius reasonable
    ;; only check ARC, DIMENSION, LINE, POLYLINE
    ;; 
    ;; Author : wooboy
    ;; version : 1.0.0  -  2015/11/18
    ;;----------------------------------------------------------------------;;
    
    ;;----------------------------------------------------------------------;;
    ;; Default settings
    ;;----------------------------------------------------------------------;;
(setq 
    layer_QC "QC-CHECK"                ; QC圖層
                                    ; after QC, generate result to which layer
    layer_QC_color "1"                ; QC圖層顏色 0-255索引色, 如為 nil 則改用 truecolor
                                    ; QC layer color, if set nil it whill use truecolor
    layer_QC_tcolor "255,0,0"        ; QC圖層顏色 truecolor, 需先設定 layer_QC_color 為 nil
                                    ; QC layer truecolor
    dimsty_QC "QC-CHECK"            ; QC DIM STYLE
    default_qc_len_type    0            ; QC長度預設檢核方式    0=0.5進位, 1=小數位數
                                    ; QC length check type, 0=round off 0.5, 1=percision
    default_qc_len_percision nil    ; QC長度預設小數位數, 如為nil 時取目前單位精度
                                    ; QC type with percision, if nil set as LUPREC
    default_qc_drawtext    1            ; 是否顯示原始長度文字    1=是, 0=否
                                    ; show object original length    1=yes, 0=no
    default_qc_textsize "10"        ; QC文字預設大小
                                    ; text size for show object original length
)

(vl-load-com)
(setq version (atof (getvar "ACADVER")))

    ;;----------------------------------------------------------------------;;
    ;;layer control
    ;;----------------------------------------------------------------------;;

(defun layer_on (layer / elist)
    (setq elist (entget (tblobjname "layer" layer)))
    (if (> 0 (cdr (assoc 62 elist)))
        (progn
            (setq elist
                (subst
                    (cons '62 (abs (cdr (assoc 62 elist))))
                    (assoc 62 elist)
                    elist
                )
            )
            (entmod elist)
            (entupd (tblobjname "layer" layer))
        )
    )
)
(defun layer_thaw (layer / elist)
    (setq elist (entget (tblobjname "layer" layer)))
    (if (= 1 (boole 1 1 (cdr (assoc 70 elist))))
        (progn
            (entmod
                (subst 
                    (cons 70 (boole 4 1 (cdr (assoc 70 elist))))
                    (assoc 70 elist)
                    elist
                )
            )
            (entupd (tblobjname "layer" layer))
        )
    )
)
(defun layer_unlock (layer / elist)
    (setq elist (entget (tblobjname "layer" layer)))
    (if (= 4 (boole 1 4 (cdr (assoc 70 elist))))
        (progn
            (entmod
                (subst 
                    (cons 70 (boole 4 4 (cdr (assoc 70 elist))))
                    (assoc 70 elist)
                    elist
                )
            )
            (entupd (tblobjname "layer" layer))
        )
    )
)
(defun create_layer (layer color truecolor / curlay)
    (setq curlay (getvar "clayer"))
    (if (null (tblsearch "layer" layer))
        (progn
            (if (null color)
                (command "-layer" "m" layer "c" "truecolor" truecolor "" "unlock" layer "THAW" layer "ON" layer "")
                (command "-layer" "m" layer "c" color "" "unlock" layer "THAW" layer "ON" layer "")
            )
        )
        (progn
            (layer_thaw layer)
            (layer_unlock layer)
            (layer_on layer)
        )
    )
    
    (setvar "clayer" curlay)
)

    ;;----------------------------------------------------------------------;;
    ;; DimStyle Create
    ;; check for style and create if missing
    ;;----------------------------------------------------------------------;;
(defun MakeDimStyle (DSTY)
    (if (not (tblsearch "DImstyle" DSTY))
        (progn
            (command
                "_DIMCLRD"   256            ;line color
                "_DIMLWD"    -1                ;line lineweight
                "_DIMDLE"    25.0000        ;line Extend Beyond Ticks
                "_DIMDLI"    0.0000            ;line baseline space
                "_DIMCLRE"   256            ;line ext color
                "_DIMLWE"    -1                ;line ext lineweight
                "_DIMEXE"    25.000            ;line ext beyond dim line
                "_DIMEXO"    25.000            ;line ext offset from original
                "_DIMSAH"    0                ;arrow use same arrow for start and end
                "_DIMBLK"    "_DOTSMALL"    ;arrow head
                "_DIMLDRBLK" "_DOTSMALL"    ;arrow leader
                "_DIMASZ"    30                ;arrow size
                "_DIMCEN"    10                ;arrow center mark
                "_DIMTXT"    30.000            ;txt size
                "_DIMTXSTY"  "standard"        ;txt style
                "_DIMCLRT"   256            ;txt color
                "_DIMGAP"    10.000            ;txt offset
                "_DIMLUNIT"  2                ;Primary Unit unit
                "_DIMDEC"    8                ;Primary Unit precision
                "_DIMDSEP"   "."            ;Primary Unit decimal seprator
                "_DIMADEC"   8                ;Primary Unit angular precision
                "_DIMTOLJ"   1                ;Tolerance Vertical position
                "_DIMFRAC"   0
                "_DIMJUST"   0
                "_DIMLFAC"   1.0000
                "_DIMLIM"    0
                "_DIMRND"    0.0000
                "_DIMSCALE"     1.0000
                "_DIMSD1"    0
                "_DIMSD2"    0
                "_DIMSE1"    0
                "_DIMSE2"    0
                "_DIMSOXD"   0
                "_DIMTIH"    0
                "_DIMTIX"    1
                "_DIMTM"     0.0000
                "_DIMTMOVE"  0
                "_DIMTOFL"   1
                "_DIMTOH"    0
                "_DIMTSZ"    0.0000
                "_DIMTVP"    0.0000
                "_DIMFIT"    5
                "_DIMLTYPE"  "BYLAYER"
                "_DIMLTEX1"  "BYLAYER"
                "_DIMLTEX2"  "BYLAYER"
                "_DIMJOGANG" 90
                "_dimstyle" "_save" DSTY    ;save dim sty
            )
        )
    )
)
    ;;----------------------------------------------------------------------;;
    ;; Start Undo - Lee Mac
    ;; Opens an Undo Group.
    ;;----------------------------------------------------------------------;;
(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)
    ;;----------------------------------------------------------------------;;
    ;; End Undo    - Lee Mac
    ;; Closes an Undo Group.
    ;;----------------------------------------------------------------------;; 
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)
    ;;----------------------------------------------------------------------;;
    ;; Active Document - Lee Mac
    ;; Returns the VLA Active Document Object
    ;;----------------------------------------------------------------------;;
(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

    ;;----------------------------------------------------------------------;;
    ;; Get Inside Angle  -  Lee Mac
    ;; Returns the smaller angle subtended by three points with vertex at p2
    ;;----------------------------------------------------------------------;;
(defun LM:GetInsideAngle ( p1 p2 p3 )
    (   (lambda ( a ) (min a (- (+ pi pi) a)))
        (rem (+ pi pi (- (angle p2 p1) (angle p2 p3))) (+ pi pi))
    )
)

    ;;----------------------------------------------------------------------;;
    ;; Round To
    ;;----------------------------------------------------------------------;;
(defun LM:Roundto ( n p )(/ (fix (+ (* n (setq p (expt 10. p))) (if (minusp n) -0.5 0.5))) p))

    ;;----------------------------------------------------------------------;;
    ;; Readable
    ;;----------------------------------------------------------------------;;
(defun LM:readable ( a )
    (    (lambda ( a )
            (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
                (LM:readable (+ a pi))
                a
            )
        )
        (rem (+ a pi pi) (+ pi pi))
    )
)

(defun mid2p (ps pe)
  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0)) ps pe)
)

    ;;----------------------------------------------------------------------;;
    ;; ssget  -  Lee Mac
    ;; A wrapper for the ssget function to permit the use of a custom selection prompt
    ;; msg - [str] selection prompt
    ;; arg - [lst] list of ssget arguments
    ;;----------------------------------------------------------------------;;
    
(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)
    ;;----------------------------------------------------------------------;;
    ;; LW Vertices  -  Lee Mac
    ;; Returns a list of lists in which each sublist describes
    ;; the position, starting width, ending width and bulge of the
    ;; vertex of a supplied LWPolyline
    ;;----------------------------------------------------------------------;;
    
(defun LM:lwvertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:lwvertices (cdr e))
        )
    )
)

    ;;----------------------------------------------------------------------;;
    ;; Bulge Radius  -  Lee Mac
    ;; p1 - start vertex
    ;; p2 - end vertex
    ;; b  - bulge
    ;; Returns the radius of the arc described by the given bulge and vertices
    ;;----------------------------------------------------------------------;;
(defun LM:bulgeradius ( p1 p2 b )
    (/ (* (distance p1 p2) (1+ (* b b))) 4 (abs b))
)

    ;;----------------------------------------------------------------------;;
    ;; Bulge Centre  -  Lee Mac
    ;; p1 - start vertex
    ;; p2 - end vertex
    ;; b  - bulge
    ;; Returns the centre of the arc described by the given bulge and vertices
    ;;----------------------------------------------------------------------;;
(defun LM:bulgecentre ( p1 p2 b )
    (polar p1
        (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b))))
        (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
    )
)

    ;;----------------------------------------------------------------------;;
    ;; Draw QC result
    ;;----------------------------------------------------------------------;;
(defun QC_DrawLine (p1 p2 / newobj)
    ;; Create new selection set for copied objects
    (setq newobj (ssadd))
    
    ;; Add new object to new selection set
    (ssadd
        (entmakex
            (list (cons 0 "LINE")
                (cons 10 p1)
                (cons 11 p2)
                (cons 8 layer_QC)
            )
        )
        newobj
    )
        
    ;; Highlight copied objects
    (sssetfirst nil newobj)
    ;; Send new object to front
    (if (> 18.2 version)
        (command "draworder" "f")
        (command-s "draworder" "f")
    )
)
(defun QC_DrawText(pos txt ang)
    (entmakex
        (list (cons 0 "TEXT")
            (cons 1 txt)
            (cons 8 layer_QC)
            '(10 0.0 0.0 0.0)
            (cons 11 pos)
            (cons 50 ang)    ; text rotation
            (cons 40 (atof default_qc_textsize))    ; text size
            (cons 72 1)        ; text alignment horizontal center
            (cons 73 2)        ; text alignment vertical center
        )
    )
)
(defun QC_DrawArc(cen rad sang eang)
    (entmakex
        (list (cons 0 "ARC")
            (cons 8 layer_QC)
            (cons 10 cen)
            (cons 40 rad)
            (cons 50 sang)
            (cons 51 eang)
        )
    )
)

    ;;----------------------------------------------------------------------;;
    ;; Check is reasonable
    ;;----------------------------------------------------------------------;;
(defun QC_MeasureReasonable (len / deltalen)
    (if (zerop default_qc_len_type)
        (progn
            (setq deltalen (abs (- len (LM:Roundto len 0))))
            (if (or (= deltalen 0) (= deltalen 0.5))
                t
                nil
            )
        )
        (progn
            (setq deltalen (abs (- len (LM:Roundto len default_qc_len_percision))))
            ;;(princ (rtos deltalen 2 8))
            (if (zerop deltalen)
                t
                nil
            )
        )
    )
)

    ;;----------------------------------------------------------------------;;
    ;; Check is the point inside polyline
    ;;     pt  - [point] ray start point
    ;;     rad - [float] rad of base line
    ;;     obj - [obj] Entities name
    ;;     mod - [int] acextendoption enum of intersectwith method
    ;;----------------------------------------------------------------------;;
(defun QC_pointInsidePL ( pt rad ob1 mod / lst rtn tmpray ob2)
    (setq tmpray
        (entmakex
            (list
                '(0 . "RAY")
                '(8 . "0")
                '(100 . "AcDbEntity")
                '(100 . "AcDbRay")
                '(60 . 1)
                (cons 10 pos)
                (cons 11 (list (cos (+ ang (/ pi -2))) (sin (+ ang (/ pi -2))) 0))
            )
        )
    )
    (setq ob1 (vlax-ename->vla-object ob1))
    (setq ob2 (vlax-ename->vla-object tmpray))
    (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
    (repeat (/ (length lst) 3)
        (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
              lst (cdddr lst)
        )
    )
    (entdel tmpray)
    (= (rem (length rtn) 2) 1)
)

    ;;----------------------------------------------------------------------;;
    ;; Clear result
    ;;----------------------------------------------------------------------;;

(defun c:QCClear(/ sel idx curlay en)
    (LM:startundo (LM:acdoc))
    (setvar "CMDECHO" 0)
    (setq sel (ssget "x" (list '(-4 . "<OR") (cons 8 layer_QC) '(-4 . "OR>"))))
    (if (not (null sel))
        (progn
            (setq curlay (getvar "clayer"))
            (create_layer layer_QC layer_QC_color layer_QC_tcolor)
            (setvar "clayer" curlay)
            (repeat (setq idx (sslength sel))
                (setq en (ssname sel (setq idx (1- idx))))
                (entdel en)
                (princ)
            )
        )
    )
    (setvar "CMDECHO" 1)
    (LM:endundo (LM:acdoc))
    (princ)
)

    ;;----------------------------------------------------------------------;;
    ;; Main lisp
    ;;----------------------------------------------------------------------;;
(defun c:QCZ0(/ *error*)
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    (LM:startundo (LM:acdoc))
    
    (setq sel
        (LM:ssget "\nSelect objects to be Check: "
            '(    "_:L"
                (
                    (0 . "LINE,LWPOLYLINE,CIRCLE,MLINE,ARC,ELLIPSE,*TEXT")
                )
            )
        )
    )
    (setq len (sslength sel))
    (setq index 0)
    (create_layer layer_QC layer_QC_color layer_QC_tcolor)
    
    (repeat len
        (setq a (entget (ssname sel index)))
        (setq b10 (assoc 10 a))
        (setq b11 (assoc 11 a))
        (setq b38 (assoc 38 a))
        
        (if (and (not (null b10)) (not (null (caddr (cdr b10)))))
            (setq z10 (LM:Roundto (caddr (cdr b10)) 8))
            ;(setq z10 (caddr (cdr b10)))
            (setq z10 0)
        )
        (if (not (null b11))
            (setq z11 (LM:Roundto (caddr (cdr b11)) 8))
            ;(setq z11 (caddr (cdr b11)))
            (setq z11 0)
        )
        (if (not (null b38))
            (setq z38 (LM:Roundto (cdr b38) 8))
            ;(setq z38 (cdr b38))
            (setq z38 0)
        )
        (if (not (zerop (+ z10 z11 z38)))
            (progn
                (setq newent (vla-copy (vlax-ename->vla-object (ssname sel index))))
                (vla-put-layer newent layer_QC)
                (vla-put-color newent 256)
            )
        )
        (setq index (+ index 1))
    );_repeat end
    
    (princ)
)
(defun c:QCLen(/ userclick)
    (setq sav (qc:savepath))
    (setq dcl (strcat sav "\\QC_Tools.dcl"))
    (qc:writedcl dcl)
    (cond
        (   (not (qc:writedcl dcl))
            (princ "\nDCL file could not be written.")
        )
        (   (<= (setq dch (load_dialog dcl)) 0)
            (princ "\nDCL file could not be loaded.")
        )
        (   (not (new_dialog "QCLen_DCL" dch))
            (princ "\nProgram dialog could not be loaded.")
        )
        (t
            (if (zerop default_qc_len_type)
                (progn
                    (set_tile "Len_Chk_05" "1")
                    (mode_tile "Len_Chk_DecNum" 1)
                )
                (progn
                    (set_tile "Len_Chk_Dec" "1")
                    (mode_tile "Len_Chk_DecNum" 0)
                )
            )
            (start_list "Len_Chk_DecNum")
                (mapcar 'add_list '("0" "1" "2" "3" "4" "5" "6" "7" "8"))
            (end_list)
            (if (= nil default_qc_len_percision)
                (setq default_qc_len_percision (getvar "LUPREC"))
            )
            (set_tile "Len_Chk_DecNum" (itoa default_qc_len_percision))
            (if (zerop default_qc_drawtext)
                (set_tile "Len_Chk_DrawText" "0")
                (set_tile "Len_Chk_DrawText" "1")
            )
            (set_tile "Len_Chk_TxtHeight" default_qc_textsize)
            (action_tile "Len_Chk_05"
                "(mode_tile \"Len_Chk_DecNum\" 1)(setq default_qc_len_type 0)(setq chktyp 0)"
            )
            (action_tile "Len_Chk_Dec"
                "(mode_tile \"Len_Chk_DecNum\" 0)(setq default_qc_len_type 1)(setq chktyp 1)"
            )
            (action_tile "Len_Chk_DrawText"
                "(setq default_qc_drawtext (atoi (get_tile \"Len_Chk_DrawText\")))"
            )
            ;default_qc_drawtext
            
            (action_tile "accept"
                "(setq default_qc_len_percision (atoi (get_tile \"Len_Chk_DecNum\")))
                (setq default_qc_textsize (get_tile \"Len_Chk_TxtHeight\"))
                (done_dialog)
                (setq userclick t)"
            );action_tile
            (action_tile "cancel"
                "(done_dialog)
                (setq userclick nil)"
            );action_tile
            (start_dialog)
            (if userclick
                (QCLen_Main)
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
 
(princ)
    
)

(defun QCLen_Main(/ *error* idx en nrm uxa cen rad ang1 ang2 pclose cnt)
    (setq nrm (trans '(0.0 0.0 1.0) 1 0 t))
    (setq uxa (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 nrm t)))
    
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (setvar "CMDECHO" 1)
        (princ)
    )
    (LM:startundo (LM:acdoc))
    (setvar "CMDECHO" 0)
    
    (setq sel
        (LM:ssget "\nSelect lines, plines, dimensions: "
            '(    "_:L"
                (
                    (0 . "LINE,LWPOLYLINE,DIMENSION,ARC")
                )
            )
        )
    )
    (create_layer layer_QC layer_QC_color layer_QC_tcolor)
    (MakeDimStyle dimsty_QC)
    
    (setq idx (sslength sel))
    (setq cnt 0)
    
    (repeat idx
        (setq en (ssname sel (setq idx (1- idx)))
                endata (entget en)
                objName (cdr (assoc 0 endata))
        )
        (cond
            ((= objName "DIMENSION")
                (if (QC_MeasureReasonable (LM:Roundto (cdr (assoc 42 endata)) 8))
                    (progn
                        (if (not (= (cdr (assoc 1 endata)) ""))
                            (progn
                                (setq newent (vla-copy (vlax-ename->vla-object en)))
                                (vla-put-layer newent layer_QC)
                                (vla-put-color newent 256)
                                (vla-put-scalefactor newent 1)
                                (setq enlist (entget (vlax-vla-object->ename newent)))
                                (if (= (rtos (cdr (assoc 42 endata)) 2 8) (cdr (assoc 1 endata)))
                                    (setq enlist (subst (cons 1 (strcat (rtos (cdr (assoc 42 endata)) 2 8) "(x)")) (assoc 1 enlist) enlist))
                                    (setq enlist (subst (cons 1 "") (assoc 1 enlist) enlist))
                                )
                                (setq enlist (subst (cons 3 dimsty_QC) (assoc 3 enlist) enlist))
                                (entmod enlist)
                                (setq cnt (1+ cnt))
                            )
                        )
                    )
                    (progn
                        (setq newent (vla-copy (vlax-ename->vla-object en)))
                        (vla-put-layer newent layer_QC)
                        (vla-put-color newent 256)
                        (vla-put-scalefactor newent 1)
                        (setq enlist (entget (vlax-vla-object->ename newent)))
                        (setq enlist (subst (cons 3 dimsty_QC) (assoc 3 enlist) enlist))
                        (entmod enlist)
                        (setq cnt (1+ cnt))
                    )
                )
            )
            ((= objName "ARC")
                (setq cen (cdr (assoc 10 endata))
                      dst (LM:Roundto (cdr (assoc 40 endata)) 8) 
                      ang1 (cdr (assoc 50 endata))
                      ang2 (cdr (assoc 51 endata))
                      p1 (polar cen ang1 dst)
                      p2 (polar cen ang2 dst)
                )
                (if (not (QC_MeasureReasonable dst))
                    (progn
                        (QC_DrawArc cen dst ang1 ang2)
                        (if (> ang1 ang2)
                            (setq pos (polar cen (+ (/ (- (max ang1 ang2) (min ang1 ang2)) 2) ang2 pi) (- dst (* (atof default_qc_textsize) 1.5))))
                            (setq pos (polar cen (+ (/ (- (max ang1 ang2) (min ang1 ang2)) 2) ang1) (- dst (* (atof default_qc_textsize) 1.5))))
                        )
                        (setq ang (+ uxa (LM:readable (angle (trans p1 0 1) (trans p2 0 1)))))
                        (QC_DrawText pos (strcat "R=" (rtos dst 2 8)) ang)
                        (setq cnt (1+ cnt))
                    )
                )
            )
            ((= objName "LINE")
                (setq p1 (cdr (assoc 10 endata))
                      p2 (cdr (assoc 11 endata))
                )
                (setq dst (LM:Roundto (distance p1 p2) 8))
                (if (not (QC_MeasureReasonable dst))
                    (progn
                        (QC_DrawLine p1 p2)
                        (setq ang (+ uxa (LM:readable (angle (trans p1 0 1) (trans p2 0 1)))))
                        (QC_DrawText (polar (mid2p p1 p2) (+ ang (/ pi 2)) (* (atof default_qc_textsize) 1.5)) (rtos (distance p1 p2) 2 8) ang)
                        (setq cnt (1+ cnt))
                    )
                )
            )
            ((= objName "LWPOLYLINE")
                (setq lst (LM:lwvertices endata))
                (if (= (cdr (assoc 10 lst)) (cdr (assoc 11 (reverse lst))))
                    (setq pclose t)
                    (setq pclose nil)
                )
                (mapcar
                    (function
                        (lambda ( l1 l2 / b p1 p2 dst blg)
                            (setq p1 (cdr (assoc 10 l1))
                                  p2 (cdr (assoc 10 l2))
                                  b (cdr (assoc 42 l1))
                            )
                            (if (zerop b)
                                (setq dst (LM:Roundto (distance p1 p2) 8)
                                      blg nil)
                                (setq dst (LM:Roundto (LM:bulgeradius p1 p2 b) 8)
                                      blg T)
                            )
                            (if (not (QC_MeasureReasonable dst))
                                (progn
                                    (if blg
                                        (progn    ; If this segment is bulge
                                            (setq cen (LM:bulgecentre p1 p2 b)
                                                  ang1 (angle cen p1)
                                                  ang2 (angle cen p2)
                                            )
                                            ; detect arc start and end angle
                                            (if (< 0 b)
                                                (progn
                                                    (QC_DrawArc cen dst ang1 ang2)
                                                    (if (> 1 b)
                                                        (setq pos (polar cen (+ (/ (LM:GetInsideAngle p1 cen p2) 2) ang1) (- dst (* (atof default_qc_textsize) 1.5))))
                                                        (setq pos (polar cen (+ (/ (- (* pi 2) (LM:GetInsideAngle p1 cen p2)) 2) ang1) (- dst (* (atof default_qc_textsize) 1.5))))
                                                    )
                                                )
                                                (progn
                                                    (QC_DrawArc cen dst ang2 ang1)
                                                    (if (< -1 b)
                                                        (setq pos (polar cen (+ (/ (LM:GetInsideAngle p1 cen p2) 2) ang2) (- dst (* (atof default_qc_textsize) 1.5))))
                                                        (setq pos (polar cen (+ (/ (- (* pi 2) (LM:GetInsideAngle p1 cen p2)) 2) ang2) (- dst (* (atof default_qc_textsize) 1.5))))
                                                    )
                                                    
                                                )
                                            )
                                            (setq ang (+ uxa (LM:readable (angle (trans p1 0 1) (trans p2 0 1)))))
                                            (QC_DrawText pos (strcat "R=" (rtos dst 2 8)) ang)
                                        )
                                        (progn    ; If this segment is straight
                                            (QC_DrawLine p1 p2)
                                            (setq mp (mid2p p1 p2))
                                            (setq ang (+ uxa (LM:readable (angle (trans p1 0 1) (trans p2 0 1)))))
                                            (setq pos (polar mp (+ ang (/ pi 2)) (* (atof default_qc_textsize) 1.5)))
                                            (if pclose    ; check is drawtext position inside closed polyline
                                                (if (not (QC_pointInsidePL pos ang en 0))
                                                    (setq pos (polar mp (+ ang (/ pi 2)) (* (atof default_qc_textsize) -1.5)))
                                                )
                                            )
                                            (QC_DrawText pos (rtos dst 2 8) ang)
                                        )
                                    )
                                    (setq cnt (1+ cnt))
                                )
                            )
                        )
                    )
                    lst
                    (if (= 1 (logand 1 (cdr (assoc 70 endata))))
                        (append (cdr lst) (list (car lst)))
                        (cdr lst)
                    )
                )
            )
        )
    );_repeat end
    (if (zerop cnt)
        (alert "全部正確")
        (alert (strcat "共有 " (itoa cnt) " 個錯誤"))
    )
    (*error* nil)
    (princ)
)

    ;;----------------------------------------------------------------------;;
    ;; create dcl file
    ;;----------------------------------------------------------------------;;
(defun qc:savepath ( / tmp )
    (if (setq tmp (getvar 'roamablerootprefix))
        (strcat (qc:fixdir tmp) "\\Support")
        (qc:fixdir (getvar 'tempprefix))
    )
)
(defun qc:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)
(defun qc:writedcl ( dcl / des )
    (cond
        (   (findfile dcl))
        (   (setq des (open dcl "w"))
            (foreach x
               '(
                    "QCLen_DCL : dialog {"
                    "    label = \"檢核線段長度 !!\";"
                    "    : column {"
                    "        : boxed_radio_row {"
                    "            label = \"檢核方法單位\";"
                    "            : radio_button {"
                    "                label = \"0.5 公分進位\";"
                    "                key = \"Len_Chk_05\";"
                    "            }"
                    "            : radio_button {"
                    "                label = \"小數點位數\";"
                    "                key = \"Len_Chk_Dec\";"
                    "            }"
                    "        }"
                    "        : popup_list {"
                    "            label = \"小數位數\";"
                    "            key   = \"Len_Chk_DecNum\";"
                    "            width = 10;"
                    "        }"
                    "        : toggle {"
                    "            key = \"Len_Chk_DrawText\";"
                    "            label = \"寫正確長度\";"
                    "        }"
                    "        : edit_box {"
                    "            label = \"字高\";"
                    "            key = \"Len_Chk_TxtHeight\";"
                    "        }"
                    "    }"
                    "    ok_cancel;"
                    "}"
                )
                (write-line x des)
            )
            (setq des (close des))
            (while (not (findfile dcl)))
            dcl
        )
    )
)

    ;;----------------------------------------------------------------------;;
    ;; promot use descript
    ;;----------------------------------------------------------------------;;
(princ "\n")
(princ "\n C:QCZ0   \tCheck Objects Z coordinate is equl 0")
(princ "\n C:QCLen  \tCheck Objects length is reasonable")
(princ "\n C:QCClear\tClear QC result")
(princ)

    ;;----------------------------------------------------------------------;;
    ;;                              End of File                             ;;
    ;;----------------------------------------------------------------------;;
