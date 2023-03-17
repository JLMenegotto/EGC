(defun egc:iniciar-1 (r)
                         (princ)
)

(defun egc:trazar-1 ()       
  		         (setq			   
			       radio1  (polar  pcen-c (+ angulo1 (* pi 0.0)) *dimrad*)
		               radio2  (polar  pcen-c (+ angulo1 (* pi 0.5)) *dimrad*)
			       radio3  (polar  pcen-c (+ angulo1 (* pi 1.0)) *dimrad*)
			       radio4  (polar  pcen-c (+ angulo1 (* pi 1.5)) *dimrad*)
			       
 			       pc1     (polar  pcen-c    rota   *orienta*) 		      
			       
                               e-afin1 (polar  pc1    (- rota    (* pi 0.5)) 1000000) 
			       e-afin2 (polar  pc1    (+ rota    (* pi 0.5)) 1000000) 
			       
                               p-afin1 (inters pcen-c  radio1  e-afin1  e-afin2  nil) 
		               p-afin2 (inters pcen-c  radio2  e-afin1  e-afin2  nil) 
                         )

                         (egc:ejeafin  e-afin1  e-afin2)
                         (egc:objciro  pcen-c   *dimrad*)

                         (cond ((and p-afin1 p-afin2)
		 	        (setq
				  
                                     d-afine  (distance p-afin1 p-afin2)
				     r-afine  (* d-afine 0.5)
                                     pcen-b   (polar    p-afin1 (+ abase (angle p-afin1  p-afin2))  r-afine) 
		                     pcen-e   (polar    pcen-b           (angle pcen-b   control)   r-afine) 
		                     anafin   (angle    pcen-c   pcen-e)

                                     eixo-1   (inters   radio1 (polar radio1 anafin 10000) pcen-e  p-afin1  nil) 
		                     eixo-2   (inters   radio2 (polar radio2 anafin 10000) pcen-e  p-afin2  nil)
			      
                                     dis-x1   (distance pcen-e eixo-1) 
		                     dis-x2   (distance pcen-e eixo-2)
				     
		                     eixo-3   (polar    eixo-1 (angle eixo-1 pcen-e) (* dis-x1 2))
		                     eixo-4   (polar    eixo-2 (angle eixo-2 pcen-e) (* dis-x2 2)) 
                                     razon    (/
				   	         (min dis-x1  dis-x2)
 					         (max dis-x1  dis-x2)
				              )
				     
			             eixo-e   (cond
			 		            ((>= dis-x1  dis-x2) (egc:p->x (mapcar '-  pcen-e  eixo-1)))
				 	            (t                   (egc:p->x (mapcar '-  pcen-e  eixo-2)))
			                      )
			         )

                                 (if (not obelip) (setq obelip (vla-addellipse *spac* (egc:p->x pcen-e) eixo-e  razon)))		                 
		                 (if (not obebas) (setq obebas (vla-addcircle  *spac* (egc:p->x pcen-b) r-afine)))

		                 (vla-put-center      obelip (egc:p->x pcen-e))
		                 (vla-put-radiusratio obelip  razon)
		                 (vla-put-majoraxis   obelip  eixo-e)
     
                                 (vla-put-center      obebas (egc:p->x pcen-b))
                                 (vla-put-radius      obebas  r-afine)
		                 (vla-put-color       obebas  8)

				 (egc:ejeafin  e-afin1  e-afin2)
                                 (egc:objciro  pcen-c   *dimrad*)
				
                                 (setq alip            (vla-get-area  obelip)
                                       acir            (vla-get-area  objcir)
				 )      
	                )			      
                 )
)

(defun egc:ejeafin (a b)
                          (if (not l-afin) (setq l-afin (vla-addline  *spac* (egc:p->x a) (egc:p->x b))))
                          (vla-put-startpoint l-afin  (egc:p->x  a))
		          (vla-put-endpoint   l-afin  (egc:p->x  b))
                          (vla-put-color      l-afin   5)  
)

(defun egc:objciro (c r)
                          (if (not objcir) (setq objcir (vla-addcircle  *spac* (egc:p->x c) r)))
                          (vla-put-radius        objcir   r)
                          (vla-put-center        objcir  (egc:p->x c))
)                                                                                                 

(defun egc:selectipo-1 (tp)
                          (setq *Bas-Estr* 
                                                 (cond
			                              ((= (substr tp 1 1) "L")
			                                             (cond 
			 	                                           ((= *Gen-tipo* "L")         "<L1>")
				                                           ((= *Gen-tipo* "LL")        "<L2>")
			     	                                           ((= *Gen-tipo* "LLL")       "<L3>")
				                                           ((= *Gen-tipo* "LLLL")      "<L4>")
				                                           ((= *Gen-tipo* "LLLLL")     "<L5>")
				                                           ((= *Gen-tipo* "LLLLLL")    "<L6>")
				                                           ((= *Gen-tipo* "LLLLLLL")   "<L7>")
				                                           ((= *Gen-tipo* "LLLLLLLL")  "<L8>")
				                                           (t                          "<L1>")
			                                              ))
			                              ((= (substr tp 1 1) "P")
			                                             (cond       
				                                           ((= *Gen-tipo*  "P")        "<P1>")
                                                                           ((= *Gen-tipo*  "PP")       "<P2>")
                                                                           ((= *Gen-tipo*  "PPP")      "<P3>")
								           ((= *Gen-tipo*  "PPPP")     "<P4>")
                                                                           (t                          "<P1>")
				                                     ))
			                              ((= (substr tp 1 1) "V")
			                                             (cond       
				                                           ((= *Gen-tipo*  "V")        "<V1>")
                                                                           ((= *Gen-tipo*  "VV")       "<V2>")
                                                                           ((= *Gen-tipo*  "VVV")      "<V3>")
									   ((= *Gen-tipo*  "VVVV")     "<V4>")
                                                                           (t                          "<V1>")
				                                     ))
		                                )
		                )
)

(defun egc:lista-de-genes-1 (#gen#)
                        (setq *puntos-E*
                                  (cond
                                       ((= #gen# "<L1>") (list
							     (list
					 		       (egc:ponto    egc-pt04 (- a-baja *seccion*))
						 	       (egc:ponto    egc-pt04    a-baja)
				                               (egc:ponto    egc-pt03    a-alta)
			                                     )
							     1 2 2 1
							 ))
				       ((= #gen# "<L2>") (list
							     (list
							       (egc:ponto    egc-pt04 (- a-baja *seccion*))
							       (egc:ponto    egc-pt04    a-baja)
					                       (egc:ponto    egc-pt02    a-alta)
			                                     )
							     1 2 2 1
							 ))
				       ((= #gen# "<L3>") (list
							     (list
							       (egc:ponto    egc-pt03  (+ a-alta *seccion*))
							       (egc:ponto    egc-pt03     a-alta)
					                       (egc:ponto    egc-pt02     a-baja)
			                                     )
							     1 2 2 1
							 ))
				       ((= #gen# "<L4>") (list
							     (list
							       (egc:ponto    egc-pt01  (- a-baja *seccion*)) 
							       (egc:ponto    egc-pt01     a-baja)
					                       (egc:ponto    egc-pt06     a-alta)
			                                     )
							     1 2 2 1
							 ))
				       ((= #gen# "<L5>") (list
							     (list
							       (egc:ponto    egc-pt04 (- a-baja *seccion*))
							       (egc:ponto    egc-pt04    a-baja)
					                       (egc:ponto    egc-pt05    a-alta)
			                                     )
							     1 2 2 1
							 ))
				       ((= #gen# "<L6>") (list
							     (list
						 	       (egc:ponto    egc-pt04 (- a-baja *seccion*))
							       (egc:ponto    egc-pt04    a-baja)
					                       (egc:ponto    egc-pt05    a-medi)
					                       (egc:ponto    egc-pt05    a-alta)
					                       (egc:ponto    egc-pt04    a-alta)
							       (egc:ponto    egc-pt04 (+ a-alta *seccion*))
			                                     )
							     1 2 4 3
							 ))
				       ((= #gen# "<L7>") (list
							     (list
							       (egc:ponto    egc-pt06 (+ a-alta *seccion*))
							       (egc:ponto    egc-pt06    a-alta)
				                               (egc:ponto    egc-pt02    a-baja)
			                             	       (egc:ponto    egc-pt07    a-alta)
			                                     )
							     1 2 3 2
							 ))
				       ((= #gen# "<L8>") (list
							     (list
							       (egc:ponto    egc-pt04 (- a-baja *seccion*))
							       (egc:ponto    egc-pt04    a-baja)
				                               (egc:ponto    egc-pt05    a-alta)
					                       (egc:ponto    egc-pt05    a-baja)
			                                     )
							     1 2 3 2
							 ))
				       ((= #gen# "<P1>") (list
							     (list
							       (egc:ponto    egc-pt01  (- a-baja *seccion*))
							       (egc:ponto    egc-pt01     a-baja)
					                       (egc:ponto    egc-pt01     a-alta)
					                       (egc:ponto    egc-pt02     a-alta)
							       (egc:ponto    egc-pt02     a-baja)
					                       (egc:ponto    egc-pt02  (- a-baja *seccion*))
			                                     )
							     1 2 4 3
							 ))
                                       ((= #gen# "<P2>") (list
							     (list
						 	       (egc:ponto    egc-pt06  (- a-baja *seccion*))
						 	       (egc:ponto    egc-pt06     a-baja)
					                       (egc:ponto    egc-pt06     a-alta)
					                       (egc:ponto    egc-pt08     a-alta)
					                       (egc:ponto    egc-pt08     a-baja)
							       (egc:ponto    egc-pt08  (- a-baja *seccion*))
			                                     )
							     1 2 4 3
							 ))
                                       ((= #gen# "<P3>") (list
							     (list
							       (egc:ponto    egc-pt00  (- a-baja *seccion*))
							       (egc:ponto    egc-pt08     a-alta)
							       (egc:ponto    egc-pt04     a-alta)
                                                               (egc:ponto    egc-pt05     a-baja)
							       (egc:ponto    egc-pt05  (- a-baja *seccion*))
			                                     )
							     1 2 3 2
							 ))
                                       ((= #gen# "<P4>") (list
							     (list
							       (egc:ponto    egc-pt04  (- a-baja *seccion*))
							       (egc:ponto    egc-pt04     a-baja)
					                       (egc:ponto    egc-pt04     a-alta)
					                       (egc:ponto    egc-pt05     a-alta)
					                       (egc:ponto    egc-pt05     a-baja)
							       (egc:ponto    egc-pt05  (- a-baja *seccion*))
			                                     )
							     1 2 4 3
							 ))				       
				       ((= #gen# "<V1>") (list
							     (list
                                                               (egc:ponto    egc-pt04  (- a-baja *seccion*))
							       (egc:ponto    egc-pt04     a-baja)
					                       (egc:ponto    egc-pt01     a-alta)
                                                               (egc:ponto    egc-pt02     a-alta)
					                       (egc:ponto    egc-pt05     a-baja)
			                                     )
							     1 2 4 3
							 ))
                                       ((= #gen# "<V2>") (list
							     (list
                                                               (egc:ponto    egc-pt04  (- a-baja *seccion*))
							       (egc:ponto    egc-pt04     a-baja)
					                       (egc:ponto    egc-pt01     a-alta)
                                                               (egc:ponto    egc-pt02     a-baja)
					                       (egc:ponto    egc-pt05     a-alta)
			                                     )
							     1 2 4 3
							 ))
                                       ((= #gen# "<V3>") (list
							     (list
                                                               (egc:ponto    egc-pt04  (- a-baja *seccion*))
							       (egc:ponto    egc-pt04     a-baja)
					                       (egc:ponto    egc-pt04     a-alta)
                                                               (egc:ponto    egc-pt02     a-alta)
					                       (egc:ponto    egc-pt05     a-baja)
			                                     )
							     1 2 4 3
							 ))
                                       ((= #gen# "<V4>") (list
							     (list
                                                               (egc:ponto    egc-pt09  (- a-baja *seccion*))
							       (egc:ponto    egc-pt09     a-baja)
					                       (egc:ponto    egc-pt13     a-medi)
                                                               (egc:ponto    egc-pt01     a-alta)
					                       (egc:ponto    egc-pt11     a-medi)
                                                               (egc:ponto    egc-pt07     a-baja)
							       (egc:ponto    egc-pt07  (- a-baja *seccion*))							       
			                                     )
							     1 2 5 4
							 ))				       
				)
		           )
)

(defun egc:ptos-1 ()
                              (setq
                                     egc-pt01   pcen-c
                                     egc-pt02   pcen-e
				     egc-pt03   pcen-b
                                     egc-pt04   p-afin1
                                     egc-pt05   p-afin2
                                     egc-pt06   eixo-1
                                     egc-pt07   eixo-2
                                     egc-pt08   eixo-3
                                     egc-pt09   eixo-4				          
                                     egc-pt10   radio1
                                     egc-pt11   radio2
                                     egc-pt12   radio3
				     egc-pt13   radio4
                              )
)