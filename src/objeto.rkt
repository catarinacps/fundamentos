(module objeto racket
    (provide (all-defined-out))

    ; Um elemento do tipo Objeto é (make-objeto x y bbox-x bbox-y macio) tal que:
    ;   pos: coord, posicao utilizando x e y
    ;   bbox-x: coord, metade do tamanho da bounding box no eixo x e y
    ;   macio: Booleano, se o objeto e macio ou nao
    ;   caminho: String, onde o bitmap esta guardado
    (define-struct objeto (pos bbox macio caminho))

    (define FATOR_RESISTENCIA_M 0.4)
    (define FATOR_RESISTENCIA_D 0.8)
)