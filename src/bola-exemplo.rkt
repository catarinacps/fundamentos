(define-struct bola (x y incr-x incr-y cor raio))
; Um elemento do conjunto Bola é 
;    (make-bola x y dx dy c r), onde
;   x: Número, posição da bola no eixo x
;   y: Número, posição da bola no eixo y
;   dx: Número, deslocamento da bola no eixo x
;   dy: Número, deslocamento da bola no eixo y
;   c: Símbolo, cor da bola
;   r: Número, valor do raio da bola

(define B1 (make-bola 10 20 5 7 'red 10))
(define B2 (make-bola 15 27 5 7 'red 10))
(define B3 (make-bola 0 0 2 1 'blue 20))


(define ALT 200)
(define LARG 400)

; desenha-uma-cena: Bola -> Scene
; Desenha um disco sólido (bola) em uma cena.
; Exemplo: (desenha-uma-cena B1) =
(define (desenha-uma-cena uma-bola)
    (place-image
        (circle  (bola-raio uma-bola) "solid" (bola-cor uma-bola))
        (bola-x uma-bola)
        (bola-y uma-bola)
        (empty-scene LARG ALT)
    )
)

; move-bola: Bola -> Bola
; Dada uma bola, e cria uma outra, modificando a posição da bola.
; Exemplo: (move-bola B1) = B2
(define (move-bola uma-bola)
    (make-bola
        (+ (bola-x uma-bola) (bola-incr-x uma-bola))
        (+ (bola-y uma-bola) (bola-incr-y uma-bola))
        (bola-incr-x uma-bola)
        (bola-incr-y uma-bola)
        (bola-cor uma-bola)
        (bola-raio uma-bola)
    )
)


; fora-dos-limites? : Bola -> Boolean
; Dada uma bola, determina se ela está fora dos limites da tela.
; Exemplos:
;     (fora-dos-limites? B1) = false
;     (fora-dos-limites? (make-bola -10 -10 5 7 'red 5) ) = true
(define (fora-dos-limites? uma-bola)
    (not
        (and
            (<= 0 (bola-x uma-bola) LARG)
            (<= 0 (bola-y uma-bola) ALT)
        )
    )
)

; move-até-que-fora : Bola -> ListaDeScene
; Dada uma bola, gera todas as cenas da sua movimentação até que caia fora dos limites.
(define (move-até-que-fora uma-bola)
    (cond
        [(fora-dos-limites? uma-bola) 
            empty
        ]
        [else
            (cons (desenha-uma-cena uma-bola) (move-até-que-fora (move-bola uma-bola)))
        ]
    )
)



(run-movie .05 (move-até-que-fora B1))
(run-movie 0.05 (move-até-que-fora B3))