;; tabs.el
;;
;; Copyright (C)  2002  Zack Rusin <zackrat@att.net>

;; Author: Zack Rusin <zackrat@att.net>
;; Maintainer: Zack Rusin <zackrat@att.net>
;; Created: 2 Jul 2001
;; Version: 1.0
;; Keywords: environment convenience

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA

;;;Commentary:
;; 
;; This package is havily based on David's Ponce "ruler-mode.el".
;; 


(defgroup tabs-mode nil
  "Display tabs."
  :group 'environment)

(defface tabs-mode-default-face
  '((((type tty))
     (:inherit default
               :background "grey64"
               :foreground "grey50"
               ))
    (t
     (:inherit default
               :background "grey76"
               :foreground "grey64"
               :box (:color "grey76"
                            :line-width 1
                            :style released-button)
               )))
  "Default face used by the tabs."
  :group 'tabs-mode)

(defface tabs-face
  '((t
     (:inherit tabs-mode-default-face
	       :foreground "black"
	       )))
  "Face 2"
  :group 'tabs-mode)

(defface tabs-current
  '((t
     (:inherit tabs-face
	       :box (:color "grey76"
                            :line-width 1
                            :style pressed-button))))
  "Current buffer tab"
  :group 'tabs-mode)

(defvar tabs-buffers nil
  "Holds mode-buffers mappings.")

(defvar tabs-mode-header-line-format-old nil
  "Hold previous value of `header-line-format'.")
(make-variable-buffer-local 'tabs-mode-header-line-format-old)

(defconst tabs-mode-header-line-format
  '(:eval (tabs-mode-go))
  "`header-line-format' used in ruler mode.")

(defvar tabs-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line down-mouse-1]
      #'tabs-switch-to-buffer)
    (define-key km [header-line down-mouse-3]
      #'ignore)
    (define-key km [header-line down-mouse-2]
      #'tabs-switch-to-buffer)
    km)
  "Keymap for tabs minor mode.")

(defun tabs-init-buffers ()
  (if (not tabs-buffers)
      (let ((str) start end)
	(dolist (elt (buffer-list) nil)
	  (if (or
	       (not (string-match "^\\*\\|^[ \t]+\\*" (buffer-name elt)))
	       (string= (buffer-name elt) "*scratch*")
	       )
	      (progn 
		(setq start (length str))
		(setq end (+ start (length (buffer-name elt))))
		(setq str (concat str (buffer-name elt) " "))
		(setq tabs-buffers (append tabs-buffers (list (cons start end))))
		))
	  )
	(setq tabs-buffers (append (list str) tabs-buffers))
    )
    tabs-buffers
    ))

(defun tabs-on-buffer (col)
  (let ((str (car tabs-buffers))
	(li  (cdr tabs-buffers))
	start end (ret))
    (dolist (elt li ret)
      (if (and 
	   (>= col (car elt))
	   (< col (cdr elt)))
	  (setq ret (substring str (car elt) (cdr elt)))
	)
    )))

(defun tabs-switch-to-buffer (start-event)
  (interactive "e")
  (let* ((start (event-start start-event))
	 (end   (event-end start-event))
	 m col w lm rm hs fc)
    (if (eq start end) ;mouse click
	(save-selected-window
          (select-window (posn-window start))
          (setq m   (window-margins)
                lm  (or (car m) 0)
                rm  (or (cdr m) 0)
		col (- (car (posn-col-row start)) lm)
                w   (window-width)
		hs  (window-hscroll)
                fc  (+ col hs 2))
	  (switch-to-buffer (tabs-on-buffer fc))
	  (message "Fc = %d, tabs on = %s" 
		   fc (tabs-on-buffer fc))
	  )
      )
    ))

(defun tabs-add-buffer ()
  (let ((str (car tabs-buffers)) start end)
      (setq start (length str))
      (setq end (+ start (length (buffer-name))))
      (setq str (concat str (buffer-name) " "))
      (setq tabs-buffers (append tabs-buffers (list (cons start end))))
      (setcar tabs-buffers  str)
      )
  )

(define-minor-mode tabs-mode
  "Display tabs with buffers in the header line if ARG > 0."
  nil nil
  tabs-mode-map
  :group 'tabs-mode
  (if tabs-mode
      (progn
        ;; When `tabs-mode' is on save previous header line format
        ;; and install the tabs header line format.
	(tabs-init-buffers)
        (setq tabs-mode-header-line-format-old header-line-format
              header-line-format tabs-mode-header-line-format)
	(add-hook 'find-file-hooks 'tabs-add-buffer)
        (add-hook 'post-command-hook    ; add local hook
                  #'force-mode-line-update nil t))
    ;; When `tabs-mode' is off restore previous header line format if
    ;; the current one is the tabs header line format.
    (if (eq header-line-format tabs-mode-header-line-format)
        (setq header-line-format tabs-mode-header-line-format-old))
    (remove-hook 'post-command-hook     ; remove local hook
                 #'force-mode-line-update t)))

;; Add tabs-mode to the minor mode menu in the mode line
(define-key mode-line-mode-menu [tabs-mode]
  `(menu-item "Tabs" tabs-mode
	      :button (:toggle . tabs-mode)))

(defun tabs-mode-go ()
  "Return tabs."
  (if tabs-mode
      (let* ((buf (tabs-init-buffers))
	     (str (car buf))
	     (li  (cdr buf))
	     (ret str)
	     )
	(dolist (elt li ret)
	  (if (string= (buffer-name (current-buffer))
		       (substring str (car elt) (cdr elt)))
	      (put-text-property (car elt) (cdr elt)
				 'face 'tabs-current ret)
	    (put-text-property (car elt) (cdr elt)
			       'face 'tabs-face ret)
	    )
	  )
	(put-text-property 0 (length str)
                           'local-map tabs-mode-map
                           ret)
	ret
	)))

;;;###autoload
(easy-mmode-define-global-mode
 global-tabs-mode tabs-mode tabs-mode)
