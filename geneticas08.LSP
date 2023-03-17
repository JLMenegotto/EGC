(defun egc:Pone_datoX (#enti  #ldato / listadato novaentid)
  
                          (if  (not (tblsearch "appid" "GENETICAS"))
                               (regapp "GENETICAS")
                          ) 
                          (setq   listadato  (list (list -3 (list "GENETICAS"
								              (cons 1000  (car  #ldato))
								              (cons 1000  (cadr #ldato))
						            )
					           )
				             )
                                  novaentid  (append  (entget #enti)  listadato)
		          )
                          (entmod novaentid)
)

(defun egc:perfil? (#enti)  (cdr (cadr  (egc:dato_X? #enti))))
(defun egc:transf? (#enti)  (cdr (caddr (egc:dato_X? #enti))))
(defun egc:dato_X? (#enti)
                            (car (Gen:dxf  -3 (entget #enti '("GENETICAS"))))
)

(defun egc:dxf     (#a #b)
                            (cdr (assoc #a
			                   (if (listp  #b)
				                       #b
				               (entget #b)
				           )
			         )
		            )
)