;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname exemplo-velho-1) (read-case-sensitive #t) (teachpacks ((lib "hangman.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hangman.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")) #f)))
;nomes: LEONARDO EICH
;;EVANDRO LUIZ DA LUZ
;;VICTOR PEREIRA SANTOS


(define ALTURA 400)
(define LARGURA 400)

(define AGUARDA .3)



(start LARGURA ALTURA)


;;um objeto é uma estrutura do tipo
;(make-objeto um-posn uma-altura uma-largura)
;;onde:
;;um-posn: numero numero, é a posição onde o objeto é fixado na tela (devem que ser menor que as dimensões inicializadas da tela)
;;uma-altura: numero, é a altura do objeto
;;uma-largura: numero, é a altura do objeto
(draw-solid-rect (make-posn (/ ALTURA 2) (/ LARGURA 2)) (/ LARGURA 8) (/ ALTURA 8) 'black)


(define-struct objeto (x y altura largura))

;;uma Bolha é uma estrutura do tipo
;(make-bolha x-incial y-inicial incremento-x incremento-y)
;onde
;x-inicial: Numero
;y-inicial: Numero
;incremento-x: Numero
;incremento-y: Numero


(define-struct bolha  (x y incremento-x incremento-y))

;(define bolha1 (make-bolha 2 7 3 1 ))

;uma ldb é uma lista de bolhas e pode ser:
;1 - empty
;2 - (cons uma-bolha ldb)






;;fora-do-jogo: Bolha -> Booleano
;;dado uma-bolha, a função fora-do-jogo
;;verifica se a bolha passou dos limites do quarto ou bateu no objeto, devolvendo true para ambos os casos
;;senão, devolve false
(define (fora-do-jogo? uma-bolha)
    (cond
    ;;testar se a bolha passou de um dos limites do quarto, ou bateu no objeto
        [(or
            ;;caso a bola tenha passado dos limites do quarto, devolver true
            (not
                (and
                    (<= 0 (bolha-x uma-bolha) LARGURA)
                    (<= 0 (bolha-y uma-bolha) ALTURA)
                )
            )
                        
            ;;caso a bolha tenha batido no objeto, devolver true
            (and
                (and
                    ;;caso a bolha esteja no intervalo de largura do objeto
                    (> (bolha-x uma-bolha) (/ LARGURA 2))
                    (< (bolha-x uma-bolha) (/ LARGURA 8))
                )
            ;;caso a bolha esteja no intervalo de altura do objeto
                (and
                    (> (bolha-y uma-bolha) (/ ALTURA 2))
                    (< (bolha-y uma-bolha (/ ALTURA 8)))
                )
            )
          )
           ;;DEVOLVER TRUE
           true
        ]
        ;;senão,devolver false
        [else false]
    )
)





;;incrementa-bolha: Bolha -> Bolha
;;dada uma-bolha, que é uma estrutura do tipo Bolha
;;a função incrementa-bolha incrementa nas posições x e y da bolha
;;de acordo com os valores de incremento-x e incremento-y da propria bolha
;;e devolve a bolha com as posiçoes x e y atualizada

(define (incrementa-bolha uma-bolha)
  (make-bolha 
   ;;incrementa o X da bolha com o seu proprio incremento
   (+ (bolha-x uma-bolha) (bolha-incremento-x uma-bolha))
   ;;incrementa o Y da bolha com o seu proprio incremento
   (+ (bolha-y uma-bolha) (bolha-incremento-y uma-bolha))
   ;;manter os incrementos iguais
   (bolha-incremento-x uma-bolha)
   (bolha-incremento-y uma-bolha)))










;;desenha-e-apaga: Bolha Simbolo Numero-> True
;;desenha uma bolha, espera um tempo, e apaga a bolha
(define (desenha-e-apaga uma-bolha raio cor)
  (and
   (draw-solid-disk 
    (make-posn
     (bolha-x uma-bolha)
     (bolha-y uma-bolha))
    raio
    cor)
   ;;espera um tempo
   (sleep-for-a-while AGUARDA)
   
   ;;E apaga a bolha
   (clear-solid-disk 
    (make-posn
     (bolha-x uma-bolha)
     (bolha-y uma-bolha))
    raio
    cor)))



;;move-ate-sair: Bolha Simbolo Numero -> Booleano
;;dados uma Bolha,
;;uma-cor, que é a cor da bolha (simbolo), 
;;um-raio, que é o Raio da bolha (numero)
;;a função move-ate-sair movimenta a bolha dentro do quarto ate ela sair dos limites ou bater no objeto
(define (move-ate-sair uma-bolha raio cor)
  (cond
    ;;caso ja esteja fora dos limites ou tocou algum objeto, apagar a bolha corrente e retornar uma nova bolha
     [(fora-do-jogo? uma-bolha) true]
     ;;senão, movimentar a bolha dentro do quarto até ela sair dos limites ou bater no objeto
     [else (and
            ;;desenha a bolha numa nova posição incrementada, e apaga a ultima posição
            ;;ate a bolha sair dos limites
            (desenha-e-apaga uma-bolha raio cor)
               (move-ate-sair
                 (incrementa-bolha uma-bolha) raio cor))]))






;;move-bolhas: lista-de-bolhas simbolo numero -> true
;;dado uma estrutura do tipo bolha, uma-cor, que é um simbolo, e um-raio, que é um numero
;a função move-bolhas movimenta uma bolha por vez, dentro de um quarto respeitando as seguintes condiçoes:
;caso a posição inicial da bolha já esteja fora dos limites do quarto, a bolha não se movimenta e a função devolve True
;;senão, a bolha é movimentada dentro do quarto conforme seus incrementos de x e y, até tocar um objeto ou passar dos limites do quarto



(define (move-bolhas ldb raio cor)
  (cond
    ;;caso a lista-bolhas seja empty, devolver true
    [(empty? ldb) true]
    ;senão, movimentar a bolha corrente
    [else (and
           (move-ate-sair (first ldb) raio cor)
           (move-bolhas (rest ldb) raio cor))]))
                        






                  
                 

                         


(move-bolhas (cons (make-bolha 0 (+ (/ ALTURA 2)4) 6 0) (cons (make-bolha 40 40 10 9) empty)) 10 'blue)
(stop)


   