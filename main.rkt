(require 2htdp/image)
(require "src/bolha.rkt")
(require "src/objeto.rkt") 
(require "src/fisica.rkt") 


(define canhao
    (make-objeto
        (make-coord 0 (- ALT 10))
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
            (objeto-pos canhao)
            10
            -20
            0.75
            'blue
            10
        )
    )
)

(define (desenha-uma-cena lista-bolhas lista-obj)
    (local
        (
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
                            (bitmap (objeto-caminho (first ldo))) 
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
                        (cons (funcao-pos (first ldo)) (lista-posicoes (rest ldo) funcao-pos))
                    ]
                )
            )
        )
        (place-images
            (append (cria-bolhas lista-bolhas) (cria-objetos lista-obj))
            (append (lista-posicoes lista-bolhas bolha-pos) (lista-posicoes lista-obj objeto-pos))
            (empty-scene LARG ALT)
        )
    )
)

(define (constroi-animacao lista-bolhas lista-obj tempo)
    (local
        (
            (define (bolhas-em-cena ldb)
                (cond
                    [(empty? ldb)
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
                                (move-bolha-na-cena uma-bolha (rest ldo))
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
        )
        (cond
            [(empty? (bolhas-em-cena lista-bolhas))
                empty
            ]
            [else
                (cons
                    (desenha-uma-cena lista-bolhas lista-obj)
                    (constroi-animacao (move-todas-bolhas lista-bolhas lista-obj (delta-tempo tempo)) lista-obj (current-seconds))
                )
            ]
        )
    )
)

(run-movie 0.05 (constroi-animacao l-bolhas l-obj (current-seconds)))