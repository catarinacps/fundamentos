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
        (+ (bolha-x uma-bolha) (bolha-dx uma-bolha))
        (* delta (+ (bolha-dy uma-bolha) (quotient 2 (* delta GRAV))))
        (bolha-dx uma-bolha)
        (bolha-dy (- (bolha-dy uma-bolha) (* delta GRAV)))
        (bolha-resistencia uma-bolha)
        (bolha-cor uma-bolha)
        (bolha-raio uma-bolha)
    )
)