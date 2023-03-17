(defun c:egc ()
                  (C:EGC1 1)
)

(defun C:EGC1 (s / objcir obelip obecir obeci1 obeci2 obebas l-afin malha vertx pts)

                 (egc:ambienteX)

                 (egc:executa  "egc:iniciar"   nil  (list s))
                 (egc:executa  "egc:iniciar-"  s    (list s))
  
                 (while (egc:acao)
                                  (cond
				        (
					  (listp pco)
						    (egc:executa  "egc:trazar-"       *reglas*     nil)
					            (egc:executa  "egc:ptos-"         *reglas*     nil)
  			                            (egc:executa  "egc:dibuja-forma"      nil     (list *Bas-Estr*))
					            (setq *contador* (1+ *contador*))
		                        )
			          )
		                  (egc:click)
		 )

                 (egc:Fecha-DNA-E)
                 (egc:dibujaperimetro *nivelesanalise*)
                 (egc:dibujamalha     (reverse vertx))
  
                 (mapcar 'egc:liberar (list objcir obelip obecir  obeci1 obeci2  obebas l-afin malha))
)

(defun egc:iniciar (r)
 
                       (setq
			     *reglas*          r			 
			     *contador*        0
			     *Lay-eixo*        (list    "Egc-Eixo"  250)
			     *Lay-tubo*        (list    "Egc-Tubo"  255)
			     *Lay-pele*        (list    "Egc-Pele"  133)

			     pcen-c            (getvar  "VIEWCTR")
			     
			     *dimrad*          10
			     *VetorT*          (* 0.05   *dimrad*)
			     *seccion*         (* 0.01   *dimrad*)
			     *seccion2*        (* 0.03   *dimrad*)
			     *cuerda*          (* *dimrad* 2)
                             *orienta*            *dimrad*

			     *l-razones*       '()
			     *proporcao*       10.00
			     *ritmo*            5
			     *razon*            1
			     *tempo*            ':W
			     *existe*           0
    			     *expresa*         -1
		             *Tag-E*            "Estrutura"
			     **mar**            "/"
			     *Gen-tipo*         "0"
 		             *Bas-Estr*         "<E0>"
			     *Base*             ""
			     
     			     *lay-contorno*     "contorno"
                             *nomecontorno-a*   "contorno-a"
                             *nomecontorno-b*   "contorno-b"
			     *nivelesanalise*   10
			     *alturaanalise*    1.5

			     acontrol           (* pi 0.25)
			     rota               (* pi 0.50)
			     
			     densidadx          0
			     densidady         -1
		             prox               1			     
			     abase              0
			     a-baja             0
			     a-alta             0
			     
                             control            (polar  pcen-c  acontrol  *cuerda*)
	                     angulo1            (angle  pcen-c  control)			     
		      )
                      
                      (setq cont 0)
                      (repeat *nivelesanalise*
 			                            (set (read (strcat *nomecontorno-a* (itoa cont))) '())
 			                            (set (read (strcat *nomecontorno-b* (itoa cont))) '())
                               (setq cont (1+ cont))
		      )

                      (egc:ad-layers (list *Lay-eixo* *Lay-tubo* *Lay-pele*))
                      (egc:Abre-DNA-E)
)


(defun egc:ambienteX ()
                                    (cond
	   		                 (*aobj*)
		                         (t  (princ "\nCarregando ambiente ActiveX.\n")
 			                     (vl-load-com)                                
                                             (setq *aobj* (vlax-get-acad-object)         
                                                   *adoc* (vla-get-activedocument *aobj*)  
                                                   *spac* (vla-get-modelspace     *adoc*)
				                   *lays* (vla-get-Layers         *adoc*)					   
                    )
		)                                        
           )
                                    (princ "\nAmbiente ActiveX ativo.\n")
)

(defun egc:liberar (o)
                               (cond ((and o
                                             (not (vlax-erased-p o))
		                      )
				      (vla-delete o) (vlax-release-object o) (setq o nil)))
)

(defun egc:executa (a b lp)
                               (eval
				     (append
				             (list
					           (read
						         (if (and a b)
							     (if (= (type b) 'INT)
							         (strcat a (itoa b))      
								 (strcat a       b)       
							     )
							     a
							 )                                  
					           )                                        
				             )                                              
					     (if lp lp)                                     
				     )                                                      
			       )                                                            
)

(defun egc:p->x     (p)        (vlax-3d-point p))

(defun egc:ponto    (p h)      (list (car p) (cadr p) h))

(defun egc:p-p      (b p o)    (mapcar '+ b (mapcar '- o p)))

(defun egc:divisor  (d)        (if (= d 0.0) 0.0001 d))

(defun egc:centroid (a)                                                                         
	                       (mapcar '(lambda (n)                                                   
			   	         (/ (apply '+                                         
						    (mapcar '(lambda (x)                    
								        (nth n x)           
							     )                              
						             a                              
					            )                                       
				            )                                                 
				            (length a)                                        
				         )                                                    
			         )                                                            
		                 '(0 1 2)                                                      
		                )                                                                      
)

(defun egc:tiraz (p)            (list (car p) (cadr p)))

(defun egc:lst->apts (ptlist)
                                (vlax-make-variant
				    (vlax-safearray-fill
				        (vlax-make-safearray                               
	                                                    vlax-vbdouble		      
	                                                    (cons 0 (- (length ptlist) 1)) 
	       )
				        ptlist
				    )
		 	        )
)

(defun egc:list->variant (vartype lst) 
                            ((lambda (lst)
                               (vlax-Make-Variant
                                     (vlax-SafeArray-Fill
                          	         (vlax-Make-SafeArray vartype (cons 0 (1- (length lst))))
                          	       lst
             )
      )
    )
                             (if (listp lst)
                                 lst
                                 (list lst)
    )
   )
)

(defun egc:mensaje (m)       (grtext -2 m))
(defun egc:informa (h r)
                             (egc:mensaje (strcat "Altura = "  (rtos h 2 2) " "
                                                  "Razão  = "  (rtos r 2 5) " "
						  "Portico = " *Bas-Estr*
				          )
			     )
)

(defun egc:ad-layers (#list#)

                             (foreach item
				      #list#
                                       (cond
	                                     ((not (tblsearch "LAYER" (nth 0 item)))

					                       (setq obj-layer (vla-add *lays* (nth 0 item)))
							       (vla-put-color obj-layer (nth 1 item))	          
								  
			 	             )
			               )
			     )
                             (princ)
)


(prompt "\n   Para executar (C:EGC 1) ou (C:EGC 0)  ")                                                                           
