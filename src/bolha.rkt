(require fisica)

; Um elemento do tipo Bolha é (make-bolha x y dx dy resistencia cor raio) tal que:
;   x: posn, posicao utilizando x e y
;   dx: Número, velocidade no eixo x
;   dy: Número, velocidade no eixo y
;   resistencia: Numero, resistencia da bolha
;   cor: Símbolo, cor da bolha
;   raio: Número, valor do raio da bolha
(define-struct bolha (pos dx dy resistencia cor raio))

; move-bolha: bolha -> bolha
; Dada uma bolha, e cria uma outra, modificando a posição da bolha.
; Exemplo: (move-bolha B1) = B2
(define (move-bolha uma-bolha delta)
    (make-bolha
        (passo-pos (bolha-pos uma-bolha) (bolha-dx uma-bolha) delta)
        (passo-pos (bolha-pos uma-bolha) (bolha-dy uma-bolha) delta)
        (bolha-dx uma-bolha)
        (passo-gravidade-dy (bolha-dy uma-bolha) delta)
        (bolha-resistencia uma-bolha)
        (bolha-cor uma-bolha)
        (bolha-raio uma-bolha)
    )
)

(define (colisao-bolha? pos-b pos-o bbox raio)
    (local
        (
            (define delta-x (abs (- (posn-x pos-b) (posn-x pos-o))))
            (define distancia-ate-bolha-y (abs (- (posn-y pos-b) (posn-y pos-o))))
        )
        (and
            (< distancia-ate-bolha-x (+ (posn-x bbox) raio))
            (< distancia-ate-bolha-y (+ (posn-y bbox) raio))
        )
    )
)

(define (bolha-fora-limites? pos-b raio)
    (and
        (<= 0 (+ (posn-x pos-b) raio) LARG)
        (<= 0 (+ (posn-y pos-b) raio) ALT)
    )
)

(define (quadrante-colisao pos-b pos-o)
    (local
        (
            (define delta-x (- (posn-x pos-b) (posn-x pos-o)))
            (define delta-y (- (posn-y pos-b) (posn-y pos-o)))
        )
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

(define (rebate-bolha uma-bolha pos-o)
    (local
        (
            (define quadrante (quadrante-colisao (bolha-pos uma-bolha) pos-o))
        )
        (make-bolha
            (passo-pos uma-bolha)
            (passo-pos uma-bolha)
            (reflete-vel-x (bolha-dx uma-bolha) quadrante)
            (reflete-vel-y (bolha-dy uma-bolha) quadrante)
            (bolha-resistencia uma-bolha)
            (bolha-cor uma-bolha)
            (bolha-raio uma-bolha)
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