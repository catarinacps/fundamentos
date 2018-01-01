; Um elemento do tipo Objeto é (make-objeto x y bbox-x bbox-y macio) tal que:
;   pos: posn, posicao utilizando x e y
;   bbox-x: Número, metade do tamanho da bounding box no eixo x
;   bbox-y: Número, metade do tamanho da bounding box no eixo y
;   macio: Booleano, se o objeto e macio ou nao
(define-struct objeto (pos bbox-x bbox-y macio))

