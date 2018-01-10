;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

    (define-struct coord (x y))

    (define GRAV 9.8)
    (define VEL_TERM_Y -100)
    (define ALT 400)
    (define LARG 600)

    (define (delta-tempo anterior)
        (- (current-milliseconds) anterior)
    )

    (define (passo-pos posicao vel delta)
        (+ posicao (* vel delta))
    )

    (define (passo-gravidade-dy dy delta)
        (cond
            [(<= dy VEL_TERM_Y)
                VEL_TERM_Y
            ]
            [else
                (+ dy (* GRAV delta))
            ]
        )
    )


    ; Um elemento do tipo Objeto é (make-objeto x y bbox-x bbox-y macio) tal que:
    ;   pos: coord, posicao utilizando x e y
    ;   bbox-x: coord, metade do tamanho da bounding box no eixo x e y
    ;   macio: Booleano, se o objeto e macio ou nao
    ;   caminho: String, onde o bitmap esta guardado
    (define-struct objeto (pos bbox macio caminho))

    (define FATOR_RESISTENCIA_M 0.4)
    (define FATOR_RESISTENCIA_D 0.8)



    ; Um elemento do tipo Bolha é (make-bolha x y dx dy resistencia cor raio) tal que:
    ;   x: coord, posicao utilizando x e y
    ;   dx: Número, velocidade no eixo x
    ;   dy: Número, velocidade no eixo y
    ;   resistencia: Numero, resistencia da bolha
    ;   cor: Símbolo, cor da bolha
    ;   raio: Número, valor do raio da bolha
    (define-struct bolha (pos dx dy resistencia cor raio))

    ; move-bolha: bolha -> bolha
    ; Dada uma bolha, e cria uma outra, modificando a posição da bolha.
    ; Exemplo: (move-bolha B1) = B2
    (define (move-bolha uma-bolha delta objeto-colisao)
        (cond
            [(empty? objeto-colisao)
                (make-bolha
                    (make-coord
                        (passo-pos (coord-x (bolha-pos uma-bolha)) (bolha-dx uma-bolha) delta)
                        (passo-pos (coord-y (bolha-pos uma-bolha)) (bolha-dy uma-bolha) delta)
                    )
                    (bolha-dx uma-bolha)
                    (passo-gravidade-dy (bolha-dy uma-bolha) delta)
                    (bolha-resistencia uma-bolha)
                    (bolha-cor uma-bolha)
                    (bolha-raio uma-bolha)
                )
            ]
            [else
                (rebate-bolha uma-bolha (objeto-pos objeto-colisao) delta)
            ]
        )
    )

    (define (colisao-bolha? pos-b pos-o bbox raio)
        (local
            [
                (define delta-x (abs (- (coord-x pos-b) (coord-x pos-o))))
                (define distancia-ate-bolha-y (abs (- (coord-y pos-b) (coord-y pos-o))))
                (define distancia-ate-bolha-x (abs (- (coord-x pos-b) (coord-x pos-o))))
            ]
            (and
                (< distancia-ate-bolha-x (+ (coord-x bbox) raio))
                (< distancia-ate-bolha-y (+ (coord-y bbox) raio))
            )
        )
    )

    (define (bolha-fora-limites? pos-b raio)
        (or
            (>= 0 (- LARG (+ (coord-x pos-b) raio)))
            (>= 0 (- ALT (+ (coord-y pos-b) raio)))
            (> 0 (+ (coord-x pos-b) raio))
            (> 0 (+ (coord-y pos-b) raio))
        )
    )

    (define (quadrante-colisao pos-b pos-o)
        (local
            [
                (define delta-x (- (coord-x pos-b) (coord-x pos-o)))
                (define delta-y (- (coord-y pos-b) (coord-y pos-o)))
            ]
            (cond
                [(and (> 0 delta-x) (< 0 delta-y))
                    1
                ]
                [(and (< 0 delta-x) (< 0 delta-y))
                    2
                ]
                [(and (< 0 delta-x) (> 0 delta-y))
                    3
                ]
                [(and (> 0 delta-x) (> 0 delta-y))
                    4
                ]
                [else
                    0
                ]
            )
        )
    )

    (define (reflete-vel-x dx quadrante)
        (cond
            [(and (= quadrante 1) (< dx 0))
                (* dx -1)
            ]
            [(and (= quadrante 2) (> dx 0))
                (* dx -1)
            ]
            [(and (= quadrante 1) (> dx 0))
                (* dx -1)
            ]
            [(and (= quadrante 1) (< dx 0))
                (* dx -1)
            ]
            [else
                0
            ]
        )
    )

    (define (reflete-vel-y dy quadrante)
        (cond
            [(and (= quadrante 1) (< dy 0))
                (* dy -1)
            ]
            [(and (= quadrante 2) (< dy 0))
                (* dy -1)
            ]
            [(and (= quadrante 1) (> dy 0))
                (* dy -1)
            ]
            [(and (= quadrante 1) (> dy 0))
                (* dy -1)
            ]
            [else
                0
            ]
        )
    )

    (define (rebate-bolha uma-bolha pos-o delta)
        (local
            [
                (define quadrante (quadrante-colisao (bolha-pos uma-bolha) pos-o))
            ]
            (make-bolha
                (make-coord
                    (passo-pos (coord-x (bolha-pos uma-bolha)) (bolha-dx uma-bolha) delta)
                    (passo-pos (coord-y (bolha-pos uma-bolha)) (bolha-dy uma-bolha) delta)
                )
                (reflete-vel-x (bolha-dx uma-bolha) quadrante)
                (reflete-vel-y (bolha-dy uma-bolha) quadrante)
                (bolha-resistencia uma-bolha)
                (bolha-cor uma-bolha)
                (bolha-raio uma-bolha)
            )
        )
    )



(define canhao
    (make-objeto
        (make-coord 50 (- ALT 30))
        (make-coord 10 10)
        false
        "bin/cannon.png"
    )
)

(define l-obj
    (list
        canhao
        (make-objeto
            (make-coord 200 (- ALT 10))
            (make-coord 10 10)
            false
            "bin/rock.png"
        )
    )
)

(define l-bolhas
    (list
        (make-bolha
            (make-coord 50 50)
            20
            -2
            0.9
            'blue
            10
        )
    )
)

(define (desenha-uma-cena lista-bolhas lista-obj)
    (local
        [
            (define (cria-bolhas ldb)
                (cond
                    [(empty? ldb)
                        empty
                    ]
                    [else
                        (cons
                            (circle (bolha-raio (first ldb)) "outline" (bolha-cor (first ldb))) 
                            (cria-bolhas (rest ldb))
                        )
                    ]
                )
            )
            (define (cria-objetos ldo)
                (cond
                    [(empty? ldo)
                        empty
                    ]
                    [else
                        (cons
                            (bitmap "rock.png")
                            (cria-objetos (rest ldo))
                        )
                    ]
                )
            )
            (define (lista-posicoes ldo funcao-pos)
                (cond
                    [(empty? ldo)
                        empty
                    ]
                    [else
                        (cons
                            (make-posn
                                (coord-x (funcao-pos (first ldo)))
                                (coord-y (funcao-pos (first ldo)))
                            )
                            (lista-posicoes (rest ldo) funcao-pos)
                        )
                    ]
                )
            )
        ]
        (place-images
            (append (cria-bolhas lista-bolhas) (cria-objetos lista-obj))
            (append (lista-posicoes lista-bolhas bolha-pos) (lista-posicoes lista-obj objeto-pos))
            (empty-scene LARG ALT)
        )
    )
)

(define (constroi-animacao lista-bolhas lista-obj tempo)
    (local
        [
            (define (bolhas-em-cena ldb)
                (cond
                    [(or (empty? ldb) (empty? (first ldb)))
                        empty
                    ]
                    [else
                        (cond
                            [(bolha-fora-limites? (bolha-pos (first ldb)) (bolha-raio (first ldb)))
                                (bolhas-em-cena (rest ldb))
                            ]
                            [else
                                (cons (first ldb) (bolhas-em-cena (rest ldb)))
                            ]
                        )
                    ]
                )
            )
            (define (move-bolha-na-cena uma-bolha ldo delta)
                (cond
                    [(empty? ldo)
                        (move-bolha uma-bolha delta empty)
                    ]
                    [else
                        (cond
                            [(colisao-bolha? (bolha-pos uma-bolha) (objeto-pos (first ldo)) (objeto-bbox (first ldo)) (bolha-raio uma-bolha))
                                (cond
                                    [(objeto-macio (first ldo))
                                        (cond
                                            [(< (bolha-resistencia uma-bolha) FATOR_RESISTENCIA_M)
                                                empty
                                            ]
                                            [else
                                                (move-bolha uma-bolha delta (first ldo))
                                            ]
                                        )
                                    ]
                                    [else
                                        (cond
                                            [(< (bolha-resistencia uma-bolha) FATOR_RESISTENCIA_D)
                                                empty
                                            ]
                                            [else
                                                (move-bolha uma-bolha delta (first ldo))
                                            ]
                                        )
                                    ]
                                )
                            ]
                            [else
                                (move-bolha-na-cena uma-bolha (rest ldo) delta)
                            ]
                        )
                    ]
                )
            )
            (define (move-todas-bolhas ldb ldo delta)
                (cond
                    [(empty? ldb)
                        empty
                    ]
                    [else
                        (cons
                            (move-bolha-na-cena (first ldb) ldo delta)
                            (move-todas-bolhas (rest ldb) ldo delta)
                        )
                    ]
                )
            )
        ]
        (cond
            [(empty? (bolhas-em-cena lista-bolhas))
                empty
            ]
            [else
                (cons
                    (desenha-uma-cena lista-bolhas lista-obj)
                    (constroi-animacao (move-todas-bolhas lista-bolhas lista-obj 0.05) lista-obj 0.05)
                )
            ]
        )
    )
)

(run-movie 0.05 (constroi-animacao l-bolhas l-obj 0.2))