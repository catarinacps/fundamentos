(module bolha racket
    (require "fisica.rkt")
    (require "objeto.rkt")

    (provide (all-defined-out))

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
                        (passo-pos (bolha-pos uma-bolha) (bolha-dx uma-bolha) delta)
                        (passo-pos (bolha-pos uma-bolha) (bolha-dy uma-bolha) delta)
                    )
                    (bolha-dx uma-bolha)
                    (passo-gravidade-dy (bolha-dy uma-bolha) delta)
                    (bolha-resistencia uma-bolha)
                    (bolha-cor uma-bolha)
                    (bolha-raio uma-bolha)
                )
            ]
            [else
                (rebate-bolha uma-bolha (objeto-pos objeto-colisao))
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
            (< 0 (+ (coord-x pos-b) raio))
            (< 0 (+ (coord-y pos-b) raio))
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

    (define (rebate-bolha uma-bolha pos-o)
        (local
            [
                (define quadrante (quadrante-colisao (bolha-pos uma-bolha) pos-o))
            ]
            (make-bolha
                (passo-pos uma-bolha)
                (reflete-vel-x (bolha-dx uma-bolha) quadrante)
                (reflete-vel-y (bolha-dy uma-bolha) quadrante)
                (bolha-resistencia uma-bolha)
                (bolha-cor uma-bolha)
                (bolha-raio uma-bolha)
            )
        )
    )
)