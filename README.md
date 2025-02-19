# defun-tco
Defineix la macro `defun-tco`, que transforma una funció recursiva
en forma _tail-call_ en una versió iterativa.

Inspirat en [Wilfred/tco.el](https://github.com/Wilfred/tco.el).
## Per exemple
### Factorial no _tail-call_
```lisp
(defun fact (n)
   (cond ((= n 0) 1)
         (t (* n (fact (- n 1))))))
```
### Factorial _tail-call_
```lisp
(defun fact (n &optional (r 1))
   (cond ((= n 0) r)
         (t (fact (- n 1) (* r n)))))
```
### Factorial _tail-call_ amb la macro `defun-tco`
```lisp
(defun-tco fact (n &optional (r 1))
   (cond ((= n 0) r)
         (t (fact (- n 1) (* r n)))))
```
### Resultat de la macro expandida
```lisp
(defun fact
      (n &optional (r 1))
      (labels ((thunk (n &optional (r 1))
                      (progn (cond ((= n 0) r)
                                   (t (lambda nil (thunk (- n 1) (* r n))))))))
              (do ((v (apply (function thunk) (list n r))
                      (funcall v)))
                  ((not (eq (type-of v) (quote closure)))
                   v))))
```
## Idea:

1. Transformar la funció que volem definir en una altra funció, `thunk`, que,
   en comptes de cridar-se recursivament avalua a una funció lambda (que sí
   que acabarà cridant a `thunk`).
2. Definir la funció original com un bucle que crida a `thunk` fins que el
   resultat no sigui una clausura.

Així, tot i que és cert que es crida `thunk` la mateixa quantitat de vegades,
no es van acumulant crides a la pila, ja que les crides a `thunk` es fan
quan la crida anterior ja ha retornat.
