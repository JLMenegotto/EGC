;;;                                                                                                                              
;;; Seleciona tipo de perfil                                                                                                     
;;;                                                                                                                              
(defun egc:Sele-DNA-E (p)                                                                             
                           (egc:executa "egc:selectipo-" *reglas* (list
							             (setq *Gen-tipo* (if (= p (substr *Gen-tipo* 1 1))          
					                                                  (strcat p *Gen-tipo*)                  
			                                                                          p                              
				                                                      )                                          
		                                                     )
						                   ))                                                          
)

;;;                                                                                                                              
;;; Grava DNA-T                                                                                                                  
;;;                                                                                                                              
(defun egc:Grav-DNA-T (#base#)
                                        (setq **DNA** (egc:Ins-base-T **DNA**  #base# *Bas-Estr*)
					      **base** #base#
					)
)
;;;                                                                                                                              
;;; Grava DNA-E                                                                                                                  
;;;                                                                                                                              
(defun egc:Grav-DNA-E (#gen#)  
                                        (setq **DNA**
					              (cond
					  	            ((= *existe* 1) (egc:Ins-trecho-E  **DNA**  #gen#))
					                    (t              (egc:Mud-trecho-E  **DNA**  #gen#))
					              )
					)
)
;;;                                                                                                                              
;;; Grava DNA-M marcadores de Introns ou Exons                                                                                   
;;;                                                                                                                              
(defun egc:Grav-DNA-M (#expresa#)                                                                                           
                                       (setq **DNA**  (egc:Ins-base-T **DNA** (if (= #expresa# 1)
										                 "1"          
		 	                                                                         "0"
									      )
							                      *Bas-Estr*
		                                      )                                                                   
                                       )                                                                                                                                                                  
)                                                                                                                                
;;;                                                                                                                              
;;; Arma os marcadores para um novo trecho de DNA-E  <Ln></Ln>                                                                   
;;;                                                                                                                              
(defun egc:trecho-E (#gen#)
                                        (strcat #gen# (egc:fecho-E #gen#))
)
(defun egc:fecho-E (#gen#)
                                        (strcat (substr #gen# 1 1) **mar** (substr #gen# (1+ (strlen **mar**))))
)
;;;                                                                                                                              
;;; Abre o DNA-E estrutural                                                                                                      
;;;                                                                                                                              
(defun egc:Abre-DNA-E ()
                                        (setq **DNA** (strcat "<" *Tag-E* "><E0></E0>"))               
)
;;;                                                                                                                              
;;; Fecha o DNA-E estrutural                                                                                                     
;;;                                                                                                                              
(defun egc:Fecha-DNA-E ()
                                        (setq **DNA** (strcat **DNA** "</" *Tag-E* ">"))               
)
;;;                                                                                                                              
;;; Insere os marcadores de trecho estrutural na cadena de DNA-E                                                                 
;;;                                                                                                                              
(defun egc:Ins-trecho-E (#dna# #gen#)
                                        (strcat #dna# (egc:trecho-E #gen#)) 
)
;;;                                                                                                                              
;;; Muda os marcadores de trecho estrutural na cadena de DNA-E                                                                   
;;;                                                                                                                              
(defun egc:Mud-trecho-E (#dna# #gen#)
                                        (strcat
					        (substr #dna#  1  (-
								    (egc:Cal-cadena-DNA  #dna#)
						 	 	    (egc:Cal-trecho-E    #gen#)
							          )
						)
 					        (egc:trecho-E #gen#)
				        )				       
)
;;;                                                                                                                              
;;; Insere as bases de transformação entre os marcadores de trecho estrutural                                                    
;;;                                                                                                                              
(defun egc:Ins-base-T (#dna# #base# #Bas-Estr#)
                                        (strcat (substr #dna# 1 (-
								   (egc:Cal-cadena-DNA  #dna#)
						 	           (egc:Cal-fecho-E     #Bas-Estr#)
							        ))
						#base#
						(egc:fecho-E #Bas-Estr#)	
				        )
)
;;;                                                                                                                              
;;; Calcula Tamanho do marcador do trecho estrutural <Ln></Ln>                                                                   
;;;                                                                                                                              
(defun egc:Cal-trecho-E   (#gen#)  (+ (strlen **mar**) (* 2 (strlen #gen#))))
;;;                                                                                                                              
;;; Calcula Tamanho da cadena de DNA                                                                                             
;;;                                                                                                                              
(defun egc:Cal-cadena-DNA (#dna#)  (strlen  #dna#))
;;;                                                                                                                              
;;; Calcula Tamanho do fecho do marcador estrutural </Ln>                                                                        
;;;                                                                                                                              
(defun egc:Cal-fecho-E    (#gen#)
                                   (+ (strlen **mar**)  (strlen #gen#))
)
