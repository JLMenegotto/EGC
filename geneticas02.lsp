(defun egc:iniciar-2 (s)
                         (setq
                                ple      (mapcar '- pcen-c  (list    *dimrad*         0       0)) 
                                pno      (mapcar '+ pcen-c  (list    0              *dimrad*  0))
                                pwo      (mapcar '+ pcen-c  (list    *dimrad*         0       0)) 
                                psu      (mapcar '- pcen-c  (list    0             *dimrad*   0))
		       
		                pc1      (mapcar '+ pcen-c  (list    *dimrad*      *dimrad*   0))
		                pc2      (mapcar '+ pcen-c  (list    *dimrad*   (- *dimrad*)  0))
		                pc3      (mapcar '+ pcen-c  (list (- *dimrad*)  (- *dimrad*)  0))
	   	                pc4      (mapcar '+ pcen-c  (list (- *dimrad*)     *dimrad*   0))
		                control  (polar     pcen-c  rota                (* *dimrad*   2))
			     
		                fat      0.05
                        ) 
) 

(defun egc:trazar-2 ()
                         (setq
			        ple     (mapcar '- pcen-c (list (+ *dimrad* fat) 0                0)) 
                                pno     (mapcar '+ pcen-c (list 0                (+ *dimrad* fat) 0))
                                pwo     (mapcar '+ pcen-c (list (+ *dimrad* fat) 0                0)) 
                                psu     (mapcar '- pcen-c (list 0                (+ *dimrad* fat) 0))
		      
		                pc1     (mapcar '+ pcen-c (list    (+ *dimrad* fat)     (+ *dimrad* fat)   0))
		                pc2     (mapcar '+ pcen-c (list    (+ *dimrad* fat)  (- (+ *dimrad* fat))  0))
		                pc3     (mapcar '+ pcen-c (list (- (+ *dimrad* fat)) (- (+ *dimrad* fat))  0))
		                pc4     (mapcar '+ pcen-c (list (- (+ *dimrad* fat))    (+ *dimrad* fat)   0))

  			        ang     (angle pno control)
                                dis     (/ (distance pno control) 5.0)
  
		                pp1     (polar pc1    ang     dis)
		                pp2     (polar pc2 (+ ang pi) dis)
 		                pp3     (polar pc3 (+ ang pi) dis)
		                pp4     (polar pc4    ang     dis)
                                pnt     (polar pno    ang     dis)
			        pst     (polar psu (+ ang pi) dis)
 
			        pme     (egc:centroid  (list pno pnt))
			        pd1     (polar pme (angle pnt pno) (distance pme pcen-c))
			        pd2     (polar pme (angle pno pnt) (distance pme pcen-c))
			        pema    (mapcar '- pcen-c (polar pcen-c (angle pcen-c pd2) (distance pno pd2)))
			        rati    (/ (distance pno pd1)        (egc:divisor (distance pno pd2)))
			        rai1    (distance pnt pd1)
			        rai2    (distance pnt pd2)
 
			       an12    (angle pd1 pd2)
			       ph1a    (polar pd1 (+ an12 (/ pi 2)) rai1)
			       ph1b    (polar pd2 (+ an12 (/ pi 2)) rai2)
			       ph2a    (polar pd1 (- an12 (/ pi 2)) rai1)
			       ph2b    (polar pd2 (- an12 (/ pi 2)) rai2)
    
  			       pho     (inters ph1a ph1b ph2a ph2b nil)
			       pho     (if pho pho ph1a)
 			     
			       dis1    (sqrt (abs (- (expt (distance pd1 pho) 2) (expt rai1 2))))
			       ang1    (if (> dis1 0)
			    	                     (atan (/ rai1 dis1))
				                     (atan 0)
				       )
 
			       pta1    (polar pho (+ (angle pho pd1) ang1) dis1)
		 	       pta2    (polar pho (- (angle pho pd1) ang1) dis1)
		        ) 

                        (if (not objcir) (setq objcir (vla-addcircle  *spac* (egc:p->X pcen-c)  *dimrad*))) 
                        (if (not obelip) (setq obelip (vla-addellipse *spac* (egc:p->X pcen-c)  (egc:p->X (mapcar '+ pcen-c (list 1 0 0))) 1))) 
                        (if (not obecir) (setq obecir (vla-addcircle  *spac* (egc:p->X pcen-c)  *dimrad*)))
                        (if (not obeci1) (setq obeci1 (vla-addcircle  *spac* (egc:p->X pcen-c)  *dimrad*))) 
	                (if (not obeci2) (setq obeci2 (vla-addcircle  *spac* (egc:p->X pcen-c)  *dimrad*)))

  
                        (vla-put-center      obelip (egc:p->x pcen-c))
                        (vla-put-majoraxis   obelip (egc:p->x pema))
                        (vla-put-radiusratio obelip  *razon*)
                        (vla-put-center      obecir (egc:p->x pme))
                        (vla-put-center      obeci1 (egc:p->x pd1)) 
	                (vla-put-center      obeci2 (egc:p->x pd2))
 
                        (vla-put-radius    objcir (egc:divisor *dimrad*))
		        (vla-put-radius    obeci1 (egc:divisor rai1))
		        (vla-put-radius    obeci2 (egc:divisor rai2))
                        (vla-put-radius    obecir (egc:divisor (distance pme pcen-c)))

                        (vla-put-color     obelip 5)
                        (vla-put-color     objcir 1)
		        (vla-put-color     obeci1 2)
		        (vla-put-color     obeci2 3)
                        (vla-put-color     obecir 4)

  
                        (setq
			      alip (vla-get-area  obelip)
                              acir (vla-get-area  obecir)
		        )
)

(defun egc:selectipo-2 (tp)
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
			                              ((= (substr tp 1 1) "P") (cond       
				                                                     ((= *Gen-tipo*  "P")      "<P1>")
                                                                                     ((= *Gen-tipo*  "PP")     "<P2>")
                                                                                     ((= *Gen-tipo*  "PPP")    "<P3>")
                                                                                     ((= *Gen-tipo*  "PPPP")   "<P4>")
								                     (t                        "<P1>")
										)
		                                       )
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

(defun egc:lista-de-genes-2 (#gen#)
                                 (setq *puntos-E*
                                                 (cond
                                                               ((= #gen# "<P1>")
                                                                               (list
							                         (list
					 		                           (egc:ponto   egc-pt02  (- a-baja *seccion*))
									           (egc:ponto   egc-pt02  0)
				                                                   (egc:ponto   egc-pt01  a-alta)
						                                   (egc:ponto   egc-pt04  0)
					                                           (egc:ponto   egc-pt04  (- a-baja *seccion*)))
							                            1 2 3 2 
							                ))
				                               ((= #gen# "<P2>")
                                                                               (list			  
							                         (list
							                           (egc:ponto   egc-pt05  (- a-baja *seccion*))
									           (egc:ponto   egc-pt05  0)
					                                           (egc:ponto   egc-pt05  a-alta)
					                                           (egc:ponto   egc-pt06  a-alta)
					                                           (egc:ponto   egc-pt06  0)
									           (egc:ponto   egc-pt06  (- a-baja *seccion*)))
                                                                                   1 2 4 3
									))
				                              ((= #gen# "<P3>")
							                       (list
							                         (list
							                           (egc:ponto   egc-pt02  (- a-baja *seccion*))
									           (egc:ponto   egc-pt02  0)
					                                           (egc:ponto   egc-pt01  a-alta)
									           (egc:ponto   egc-pt04  a-alta)
                                                                                   (egc:ponto   egc-pt03  0)
									           (egc:ponto   egc-pt03  (- a-baja *seccion*)))
                                                                                   1 2 4 3			      
								              ))
                                                             ((= #gen# "<P4>")
							                      (list
							                        (list
							                           (egc:ponto   egc-pt01  (- a-baja *seccion*))
							                           (egc:ponto   egc-pt01     a-baja)
					                                           (egc:ponto   egc-pt03     a-medi)
                                                                                   (egc:ponto   egc-pt02     a-alta)
                                                                                   (egc:ponto   egc-pt09     a-baja)
							                           (egc:ponto   egc-pt09  (- a-baja *seccion*)))
									           1 2 4 3
								             ))
                                                             ((= #gen# "<L1>")
							                      (list
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

(defun egc:ptos-2 ()
                              (setq
                                     egc-pt01   pcen-c
                                     egc-pt02   pc1
                                     egc-pt03   pta1
                                     egc-pt04   pta2
                                     egc-pt05   pd1
                                     egc-pt06   pd2
                                     egc-pt07   pho
                                     egc-pt08   pnt
                                     egc-pt09   pst
				     
                                     egc-pt10   pst
                                     egc-pt11   pst
                                     egc-pt12   pst
                                     egc-pt13   pst				     
				     
                              )
)

