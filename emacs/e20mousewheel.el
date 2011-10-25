
 (defun up-slightly () (interactive) (scroll-up 5))
 (defun down-slightly () (interactive) (scroll-down 5))
 (global-set-key [mouse-4] 'down-slightly)
 (global-set-key [mouse-5] 'up-slightly)

 (defun up-one () (interactive) (scroll-up 1))
 (defun down-one () (interactive) (scroll-down 1))
 (global-set-key [(shift mouse-4)] 'down-one)
 (global-set-key [(shift mouse-5)] 'up-one)

 (defun up-a-lot () (interactive) (scroll-up))
 (defun down-a-lot () (interactive) (scroll-down))
 (global-set-key [(control mouse-4)] 'down-a-lot)
 (global-set-key [(control mouse-5)] 'up-a-lot)

