;;;                                                                                                                         
;;; DNA-M corresponde ao DNA marcação                                                                                       
;;; DNA-T corresponde ao DNA transformação                                                                                  
;;; DNA-E corresponde ao DNA estrutural                                                                                     
;;;                                                                                                                         
;;executa a função de acordo com o carater clicado no teclado e o grava na corrente de DNA                                  

(defun egc:click   ()   
                            (cond
		                   ((egc:l?)      (egc:Sele-DNA-E "L")              (egc:Grav-DNA-E *Bas-Estr*) (setq *existe* 0))
			           ((egc:p?)      (egc:Sele-DNA-E "P")              (egc:Grav-DNA-E *Bas-Estr*) (setq *existe* 0))
			           ((egc:v?)      (egc:Sele-DNA-E "V")              (egc:Grav-DNA-E *Bas-Estr*) (setq *existe* 0))
			           ((egc:2?)      (egc:T+90)                        (egc:Grav-DNA-T "2")        (setq *existe* 1))
               	                   ((egc:3?)      (egc:T-90)                        (egc:Grav-DNA-T "3")        (setq *existe* 1))
			           ((egc:4?)      (egc:T+00)                        (egc:Grav-DNA-T "4")        (setq *existe* 1))
                                   ((egc:5?)      (egc:T-00)                        (egc:Grav-DNA-T "5")        (setq *existe* 1))
			           ((egc:6?)      (egc:R+01)                        (egc:Grav-DNA-T "6")        (setq *existe* 1))
		                   ((egc:7?)      (egc:R-01)                        (egc:Grav-DNA-T "7")        (setq *existe* 1))
			           ((egc:8?)      (egc:R+90)                        (egc:Grav-DNA-T "8")        (setq *existe* 1))
		                   ((egc:9?)      (egc:R-90)                        (egc:Grav-DNA-T "9")        (setq *existe* 1))
			           ((egc:+?)      (egc:E+01)                        (egc:Grav-DNA-T "+")        (setq *existe* 1))
			           ((egc:-?)      (egc:E-01)                        (egc:Grav-DNA-T "-")        (setq *existe* 1))
			           ((egc:a?)      (egc:TR-01)                       (egc:Grav-DNA-T "a")        (setq *existe* 1))
			           ((egc:j?)      (egc:TR-02)                       (egc:Grav-DNA-T "j")        (setq *existe* 1))
			           ((egc:z?)      (egc:TR-03)                       (egc:Grav-DNA-T "z")        (setq *existe* 1))
			           ((egc:q?)      (egc:TR-04)                       (egc:Grav-DNA-T "q")        (setq *existe* 1))
		                   ((egc:w?)      (egc:TR-05)                       (egc:Grav-DNA-T "w")        (setq *existe* 1))
			           ((egc:m?)      (egc:M+00)                        (egc:Grav-DNA-T "m")        (setq *existe* 1))
			           ((egc:n?)      (egc:N-00)                        (egc:Grav-DNA-T "n")        (setq *existe* 1))
			           ((egc:b?)      (egc:B+90)                        (egc:Grav-DNA-T "b")        (setq *existe* 1))
		                   ((egc:c?)      (egc:C-90)                        (egc:Grav-DNA-T "c")        (setq *existe* 1))
			           ((egc:g?)      (egc:G+01)                        (egc:Grav-DNA-T "g")        (setq *existe* 1))
		                   ((egc:h?)      (egc:H-01)                        (egc:Grav-DNA-T "h")        (setq *existe* 1))
			           ((egc:clicou?) (setq *expresa* (* -1 *expresa*)) (egc:Grav-DNA-M *expresa*))                   
		                   ((egc:i?)      (egc:executa "egc:iniciar-" *reglas* nil))                                      
                                   ((egc:r?)      (redraw))                                                                       
 		             )                                                              
)

(defun egc:R+01  ()   (setq rota     (+ rota (* pi 0.10))  *orienta* (* prox *dimrad*)))
(defun egc:R-01  ()   (setq rota     (- rota (* pi 0.10))  *orienta* (* prox *dimrad*)))
(defun egc:R+90  ()   (setq rota     (+ rota (* pi 0.50))  *orienta* (* prox *dimrad*)))
(defun egc:R-90  ()   (setq rota     (- rota (* pi 0.50))  *orienta* (* prox *dimrad*)))

(defun egc:E+01  ()   (setq *dimrad* (* *dimrad* 1.15)))
(defun egc:E-01  ()   (setq *dimrad* (* *dimrad* 1.15)))

(defun egc:T+00  ()   (setq pcen-c   (mapcar '+ pcen-c (list *VetorT*   0           0))))
(defun egc:T-00  ()   (setq pcen-c   (mapcar '- pcen-c (list *VetorT*   0           0))))
(defun egc:T+90  ()   (setq pcen-c   (mapcar '+ pcen-c (list 0         *VetorT*     0))))
(defun egc:T-90  ()   (setq pcen-c   (mapcar '- pcen-c (list 0         *VetorT*     0))))
			
(defun egc:TR-01 ()   (setq prox     (+ prox 0.15)            *orienta* (* prox *dimrad*)))
(defun egc:TR-02 ()   (setq prox     (- prox 0.15)            *orienta* (* prox *dimrad*)))
(defun egc:TR-03 ()
                      (setq pcen-c   (mapcar '- pcen-c (list 0  *VetorT* 0)))
                      (setq prox     (+ prox 0.15)            *orienta* (* prox *dimrad*))
                      (setq *dimrad* (+ *dimrad*  (* *dimrad* 0.25)))
)
(defun egc:TR-04 ()   
                      (setq
;;;			    acontrol (+ acontrol (* pi 0.01))
			    *cuerda* (* *cuerda* (/ (+ 1 (sqrt 5)) 2.0))
		      )
)

(defun egc:TR-05 ()   (setq abase    (+ abase (* pi 0.0872665))))

;;para os conjugados                                                                                                  
(defun egc:M+00 ()    (setq control (mapcar '+ control  (list *dimrad*  0         0))))
(defun egc:N-00 ()    (setq control (mapcar '- control  (list *dimrad*  0         0))))
(defun egc:B+90 ()    (setq control (mapcar '+ control  (list    0     *dimrad*   0))))
(defun egc:C-90 ()    (setq control (mapcar '- control  (list    0     *dimrad*   0))))

(defun egc:G+01 ()    (egc:TR-04))
(defun egc:H-01 ()    (egc:TR-03))


(defun c:teclado ()

		                   (prompt "\n L P ou V Formato do perfil em L, Portico ou enviesado ")
                                   (prompt "\n Tecla 2 Translação para acima ")
                                   (prompt "\n Tecla 3 Translação para baixo ")
                                   (prompt "\n Tecla 4 Translação para direita ")
                                   (prompt "\n Tecla 5 Translação para esquerda ")
  
                                   (prompt "\n Tecla 6 Rotação positiva ")
                                   (prompt "\n Tecla 7 Rotação negativa ")
                                   (prompt "\n Tecla 8 Rotação 90° positiva ")
                                   (prompt "\n Tecla 9 Rotação 90° negativa ")

                                   (prompt "\n Tecla + Aumenta Escalar")
                                   (prompt "\n Tecla - Diminui Escalar")

                                   (prompt "\n Tecla a ")
                                   (prompt "\n Tecla j ")
                                   (prompt "\n Tecla z Aumenta raio")
                                   (prompt "\n Tecla q ")
                                   (prompt "\n Tecla w ")
  
                                   (prompt "\n Tecla m ")
                                   (prompt "\n Tecla n ")
                                   (prompt "\n Tecla b ")
                                   (prompt "\n Tecla c ")
                                   (prompt "\n Tecla g ")
                                   (prompt "\n Tecla h ")

)