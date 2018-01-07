(module fisica racket
    (provide (all-defined-out))

    (define-struct coord (x y))

    (define GRAV 9.8)
    (define VEL_TERM_Y -100)
    (define ALT 400)
    (define LARG 600)

    (define (delta-tempo anterior)
        (- (current-seconds) anterior)
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
)