; Defineix la macro `defun-tco`, que transforma una funció recursiva
; en forma "tail-call" en una versió iterativa.
; Inspirat en https://github.com/Wilfred/tco.el
;
; * Factorial no tail-call
; (defun fact (n)
;    (cond ((= n 0) 1)
;          (t (* n (fact (- n 1))))))
;
; * Factorial tail-call
; (defun fact (n &optional (r 1))
;    (cond ((= n 0) r)
;          (t (fact (- n 1) (* r n)))))
;
; * Factorial tail-call amb la macro
; (defun-tco fact (n &optional (r 1))
;    (cond ((= n 0) r)
;          (t (fact (- n 1) (* r n)))))
;
; * Resultat de la macro expandida
; (defun fact
;       (n &optional (r 1))
;       (labels ((thunk (n &optional (r 1))
;                       (progn (cond ((= n 0) r)
;                                    (t (lambda nil (thunk (- n 1) (* r n))))))))
;
;               (do ((v (apply (function thunk) (list n r))
;                       (funcall v)))
;                   ((not (eq (type-of v) (quote closure)))
;                    v))))
;
; Idea:
; 1. Transformar la funció que volem definir en una altra funció, `thunk`, que,
;    en comptes de cridar-se recursivament avalua a una funció lambda (que sí
;    que acabarà cridant a `thunk`).
; 2. Definir la funció original com un bucle que crida a `thunk` fins que el
;    resultat no sigui una clausura.
;
; Així, tot i que és cert que es crida `thunk` la mateixa quantitat de vegades,
; no es van acumulant crides a la pila, ja que les crides a `thunk` es fan
; quan la crida anterior ja ha retornat.

; Definim la macro `defun-tco` amb tres arguments:
; * `function-name`: nom de la funció que volem definir
; * `args`: arguments de la funció (és una llista amb àtoms, paraules clau
;           com &optional i possiblement subllistes)
; * `body`: cos de la funció (és una llista d'expressions, possiblement només una)
(defmacro defun-tco (function-name args &rest body)
    (labels (
            ; Transforma el cos de la funció:
            ; En trobar una crida a `function-name`, la transforma en una crida a
            ; una funció lambda que crida a `thunk`.
            ; Per exemple:
            ; (fact (- n 1) (* r n)) -> (lambda () (thunk (- n 1) (* r n)))
            (transform-thunk (expr)
                (cond
                    ((atom expr) expr)
                    ((and (listp expr) (eq (car expr) function-name))
                        (list 'lambda '() (cons 'thunk (cdr expr))))
                    (t (mapcar #'transform expr))))
            ; Obté la llista d'arguments efectius a partir de la llista d'arguments
            ; formals de la funció.
            ; Si troba &optional o &rest, els ignora.
            ; Si troba una llista (valor per defecte), es quedda amb el primer element.
            (extract-parameters (args)
                (cond
                    ((null args) nil)
                    ((member (car args) '(&optional &rest)) (extract-parameters (cdr args)))
                    ((atom (car args)) (cons (car args) (extract-parameters (cdr args))))
                    ; Noméss hauria de poder ser una llista
                    (t (cons (caar args) (extract-parameters (cdr args)))))))
        (let (
                (progbody (transform-thunk (cons 'progn body)))
                (progargs (extract-parameters args)))
            `(defun ,function-name ,args ; Definim la funció original
                (labels ((thunk ,args ,progbody)) ; Definim la funció `thunk`
                    ; Iteram sobre `v`, amb valor inicial el resultat de la
                    ; crida a `thunk` amb els arguments de la funció original
                    ; i amb valors successius els resultats de cridar `v`,
                    ; que és una clausura que crida `thunk`.
                    ; Repetim fins que `v` no sigui una clausura.
                    ; En aquest cas retornam `v`, que és el resultat final
                    ; de la funció original.
                    (do ((v (apply #'thunk (list ,@progargs)) (funcall v)))
                        ((not (eq (type-of v) 'closure)) v)))))))

; (pprint (macroexpand '(defun-tco fact (n &optional (r 1))
;    (cond ((= n 0) r)
;          (t (fact (- n 1) (* r n)))))))
