; Um elemento do tipo Objeto é (make-objeto x y bbox-x bbox-y macio) tal que:
;   pos: posn, posicao utilizando x e y
;   bbox-x: posn, metade do tamanho da bounding box no eixo x e y
;   macio: Booleano, se o objeto e macio ou nao
(define-struct objeto (pos bbox macio))

