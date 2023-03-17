;;;                                                                                                         
;;;                           Funciones geométricas                                                         
;;;                                                                                                         

(defun egc:pnt-perimetro (#h #p1 #p2 / prumada tangente distprum) ; p-projet p-alto p-bajo)

                         (cond
			       ((< (caddr #p1) (caddr #p2))  (setq p-bajo  #p1   p-alto  #p2))
			       (t                            (setq p-bajo  #p2   p-alto  #p1))
		         )
                         (setq entro 0)
                         (cond
			         ((not (zerop #h))
				                  (setq prumada  (list (car p-alto) (cadr p-alto) (caddr p-bajo)))
				  
                                                  (if (egc:vertical?  #p1  #p2)
						      (setq p-projet   p-bajo)
				                      (setq tangente
							             (/
					                                (distance prumada p-alto)
					                                (distance prumada p-bajo)
					                             )
				                            distprum (/ #h tangente)
			                       	            p-projet (polar p-bajo (angle p-bajo prumada) distprum)   
		                                      )
					          )
                                )
			        (t       (setq p-projet   p-bajo))
			 )
  
                         (list (car p-projet) (cadr p-projet) #h)
)

(defun egc:vertical? (a b)
                            (and 
                                   (= (car  a) (car  b))
				   (= (cadr a) (cadr b))
		            )
)