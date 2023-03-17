;;;                                                                                                   
;;;                                                                                                   
;;;                                                                                                   
;;;                          funções de direcionamento da transformação                               
;;;                                                                                                   
;;;                                                                                                   
;;;                                                                                                   
;;;                                                                                                   
(defun egc:metrica (#fig1 #fig2)
                                      (egc:razon    #fig1 #fig2)
                                      (setq didia   (if (= *reglas* 1)
 						        (distance  egc-pt07   egc-pt09)
						        (distance  pnt        pd2) 
						    )
		 	                    a-alta  (* didia (/ 1.0  *razon*))
			                    a-medi  (* a-alta 0.5)
		                      )
                                      (setq *l-razones* (cons *razon*  *l-razones*))
                                      (egc:informa  a-alta  *razon*)
)

(defun egc:razon   (a b)
                                      (setq *razon* 
                                                     (cond
			                                  ((and (not (zerop a))
				                                (not (zerop b))
			                                  )
			                                                       (/ (min a b) (max a b))
							  )
			                                  (t                   1.0)
		 	                             )
		                      )
)

;;;                                                                                                                  
;;;Verifica se se debe dibujar el perfil de acordo com alguma restrição dimensional, modular etc.                    
;;;                                                                                                                  

(defun egc:dibuja? ()
                          (if (and
				   (/= *Bas-Estr*       "<E0>")
;;;				   (<= a-alta           (* *proporcao* didia))
				   (=  *existe*             1)
				   (=  *expresa*            1)
		              )
			      t
			      nil
			  )
)

(defun egc:portico (#gen# / lvert eixo perf tubo)
                                 (if (= *expresa* 1)
                                     (cond ((egc:dibuja?)
					                 (setq vertx (cons (car #gen#) vertx))	
                                                         (mapcar '(lambda (a)
								          (setq lvert (append a lvert))
				                                  )
				                                  (car #gen#)
		                                         )
					                 (setq eixo (egc:dibujaeje  *spac*   lvert)   
 			  		                       perf (egc:dibujaper  *spac*   (car (car *puntos-E*)) (egc:seccion  *contador*))
					                       tubo (egc:dibujatubo *spac*   perf  eixo)
				                         )
					                 (egc:perimetro   *puntos-E*   *nivelesanalise*  *alturaanalise*)
					                 (egc:Pone_datoX  (vlax-vla-object->ename eixo) (list *Bas-Estr*  **base**))
					                 (vla-put-layer   eixo (car *Lay-eixo*))
					                 (vla-put-layer   tubo (car *Lay-tubo*))
                                                         (vla-update      eixo)
					                 (vla-update      tubo)
			                  ) 
	                            )
		                )   
)

(defun egc:seccion (n)
                               (if (or  (= n 0)
				        (= (rem n *ritmo*) 0)
			           )
                                   *seccion2*
			           *seccion*
			       )
)
  
(defun egc:perimetro (l #niveis #altu / puntos index i1 i2 i3 i4 cont)

                               (setq
			 	      puntos  (car l)
				      index   (cdr l)
				      i1      (nth 0 index)
				      i2      (nth 1 index)
				      i3      (nth 2 index)
				      i4      (nth 3 index)
				      cont    0
		               )
                               (repeat #niveis
				       (set (read (strcat *nomecontorno-a* (itoa cont)))
					          (cons
						         (egc:pnt-perimetro (* cont #altu) (nth i1 puntos) (nth i2 puntos))
							 (eval (read (strcat *nomecontorno-a* (itoa cont))))
					          )
				       )
                                       (set (read (strcat *nomecontorno-b* (itoa cont)))
					          (cons
						         (egc:pnt-perimetro (* cont #altu) (nth i3 puntos) (nth i4 puntos))
							 (eval (read (strcat *nomecontorno-b* (itoa cont))))
					          )
				       )
                                       (setq cont (1+ cont))
		              )
)

(defun egc:dibuja-forma (#gen#)                                               
                                      (egc:metrica  alip acir)                
                                      (egc:portico (egc:executa "egc:lista-de-genes-" *reglas* (list #gen#)))
)

(defun egc:dibujaperimetro (#niveis / objcon)
 
                              (setq cont 0)
                              (repeat #niveis
				              (egc:ad-layers (list (list (strcat *lay-contorno* (itoa cont)) 8)))
				
				              (mapcar
						      '(lambda (a) (setq lperim (append a lperim)))
					               (eval (read (strcat *nomecontorno-a* (itoa cont))))
				              )
                                              (mapcar
						      '(lambda (a) (setq lperim (append a lperim)))
					               (reverse (eval (read (strcat *nomecontorno-b* (itoa cont)))))
				              )
				
				              (setq objcon (vla-add3dpoly *spac* (egc:lst->apts lperim)))
				              (vla-put-closed objcon :vlax-true)
				              (vla-put-layer  objcon (strcat *lay-contorno* (itoa cont)))
				
                                              (setq
						    lperim  nil
						    objcon  nil
					            cont    (1+ cont)
				              )
		              )
)


(defun egc:dibujamalha (l)
                                           (mapcar '(lambda (i)
	                                                       (mapcar '(lambda (n)
			                                                            (setq pts (append n pts))
			                                                )
                                                                        i
	                                                       )
	                                             )
	                                             l
                                          )
                                          (setq pele (vla-Add3Dmesh *spac* (length l) (length (car l))  (egc:lst->apts pts)))
                                          (vla-put-Nclose pele :vlax-false)
                                          (vla-put-Mclose pele :vlax-false)
                                          (vla-put-layer  pele (car *Lay-pele*))
)


(defun egc:dibujaper  (esp cen rai)
                                            (vla-addregion esp (egc:list->variant
						                                   vlax-vbobject
				                                                   (vla-addcircle esp (vlax-3D-point cen) rai)
					 	               )
			                    )
)

(defun egc:dibujaeje  (esp li1)
                                            (vla-add3dpoly esp (egc:lst->apts li1))
)

(defun egc:dibujatubo (esp obj path)
                                            (vla-AddExtrudedSolidAlongPath esp
						                           (vlax-safearray-get-element (vlax-variant-value obj) 0)
						                           path
					    )
)