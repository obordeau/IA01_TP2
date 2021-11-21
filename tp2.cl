(setq *actions* '(
	(1 1)
	(1 2)
	(1 3)
	(0 1)
	(0 2)
	(0 3))
)

(defun prochain_joueur (etat)
	(cadr etat))

(defun nb_allumettes_total (etat)
	(car etat))

(defun joueur_action (action)
	(car action))

(defun allumettes_a_prelever (action)
	(cadr action))

(defun etat_gagnant (x)
	(cond 
		((= x 0) (return-from etat_gagnant '(0 1)))
		((= x 1) (return-from etat_gagnant '(0 0)))
		(T NIL)
		)
  )

(defun actions_possibles (etat)
	(let ((result NIL))
		(dolist (action *actions*)
			(if (eq (prochain_joueur etat) (joueur_action action))
				(if (>= (nb_allumettes_total etat) (allumettes_a_prelever action))
					(push action result)
				)
			)
		)
		result
	)
)

(defun jouer (etat action) ;;applique l'action voulue a un etat
	(if (eq (prochain_joueur etat) (joueur_action action)) ;; on verifie que c'est au bon joueur de jouer
		(if (>= (nb_allumettes_total etat) (allumettes_a_prelever action)) ;; et que le nombre d'allumettes total est suffisant
			(if (= (prochain_joueur etat) 1)
				(list (- (nb_allumettes_total etat) (allumettes_a_prelever action)) 0)
				(list (- (nb_allumettes_total etat) (allumettes_a_prelever action)) 1)
			)
			NIL
		)
		NIL
	)
  )

(defun successeurs_possibles(etat)
	(let ((result NIL) (actions (actions_possibles etat)))
		(dolist (action actions)
			(push (jouer etat action) result)
		)
		result
	)
)

(defun affichage (parcours)
	(let ((precedent (pop parcours)) (dernier))
		(setq parcours (reverse parcours))
		(setq dernier (pop parcours))
		(setq parcours (reverse parcours))
		(format t "Il y a ~S allumettes, c'est au joueur ~S de jouer ~%" (car precedent) (cadr precedent))
		(dolist (x parcours)
			(dotimes (i (car precedent))
				(format t "| ")
				)
			(format t "~%")
			(dotimes (k (car precedent))
				(format t "o ")
				)
			(format t "~%")
			(format t "Il a enleve ~S allumettes ~%" (- (car precedent) (car x)))
			(format t "~%Il y a ~S allumettes, c'est au joueur ~S de jouer ~%" (car x) (cadr x))
			(setq precedent x)
		)
		(dotimes (i (car precedent))
			(format t "| ")
			)
		(format t "~%")
		(dotimes (k (car precedent))
			(format t "o "))
		(format t "~% ~%")
		(format t "Le joueur ~S a enleve les dernieres allumettes, il a gagne" (cadr precedent))
	)
)

(defun parcours_profondeur_tous(etat parcours gagnant)
	(let ((g (etat_gagnant gagnant))) ;; on affecte a g l'etat final pour que le gagnant gagne
		(push etat parcours) ;; on push dans parcours l'etat actuel comme on vient de le visiter
		(cond 
			((EQUAL etat g)(print (reverse parcours))) ;; si l'etat actuel correspond a notre etat final souhaite on affiche le parcours
			(T
				(let ((succ (successeurs_possibles etat))) ;; dans tous les cas, on continue d'etudier les successeurs de l'etat
					(dolist (xx succ)
						(parcours_profondeur_tous xx parcours gagnant) ;; on appelle pour chaque successeur la fonction
					)
				)
			)
		)
	)
)

(defparameter sol 0) ;; on definit un parametre sol a initialiser des que l'on veut appeler la fonction parcours_profondeur_premier

(defun parcours_profondeur_premier(etat parcours gagnant)
	(let ((g (etat_gagnant gagnant))) ;; on affecte a g l'etat final pour que le gagnant gagne
		(push etat parcours) ;; on push dans parcours l'etat actuel comme on vient de le visiter
		(cond 
			((= sol 1) NIL) ;; si sol est a 1 on arrete car le premier parcours a deja ete trouve
                        ((EQUAL etat g) (affichage (reverse parcours)) (setq sol 1))
                        ;; si l'etat actuel correspond a notre etat final souhaite on affiche le parcours et on modifie le parametre sol a 1
			(T
				(let ((succ (successeurs_possibles etat))) ;; sinon on observe les successeurs de l'etat
					(dolist (xx succ)
						(parcours_profondeur_premier xx parcours gagnant) ;; et on appelle la fonction sur les successeurs
					)
				)
			)
		)
	)
)

(defun end_in_file (end file)
	(dolist (xx file) ;; pour chaque chemin
		(dolist (yy xx) ;; pour chaque etat
                     (if (equal yy end) (return-from end_in_file xx)) 
                     ;; si l'etat est egal a l'etat final, on retourne le chemin contenant cet etat final
		)
	)
)

(defun succ_list (chemin)
	(let ((result NIL) (succs (successeurs_possibles (car chemin)))) ;; on recupere les successeurs du premier etat du chemin
		(dolist (xx succs) ;; pour chaque successeurs
			(push (append (list xx) chemin) result) ;; on push un nouveau chemin auquel on ajoute le successeur
		)
		(reverse result) ;; on renvoie la liste des chemins avec les successeurs ajoutes mais en reverse pour correspondre a l'arbre 
	)
)

(defun parcours_largeur (start gagnant)
	(let ((file (list (list start))) (g (etat_gagnant gagnant))) ;; on affecte a g l'etat final pour que le gagnant gagne
		(loop while (null (END_IN_FILE g file)) ;; tant que l'etat final n'est pas dans la file
			do 
				(let ((new_file NIL)) ;; on cree une nouvelle file contenant tous les chemins decoulant des chemins de l'ancienne file
					(dolist (xx file) (setq new_file (append new_file (SUCC_LIST xx))))
					(setq file new_file)
				)
		)
		(affichage (reverse (END_IN_FILE g file))) ;; on affiche le chemin trouve contenant l'etat final
	)
)
