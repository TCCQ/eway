;; something like exwm

(define-derived-mode eway-mode fundamental-mode "eway"
  "Mode for non-emacs 'windows' in the WM sense. Generalizes between emacs buffers and WM application windows"
  (read-only-mode 1)
  (add-hook 'kill-buffer-hook 'eway--buffer-kill 0 t)
  (add-hook 'window-size-change-functions 'eway--buffer-window-resize 0 t)
  (add-hook 'window-selection-change-functions 'eway--buffer-window-selection 0 t)
  (add-hook 'window-configuration-change-hook 'eway--buffer-window-change 0 nil)
  (add-hook 'window-state-change-hook 'eway--ensure-window-unqiueness 0 nil)
  (add-hook 'kill-emacs-hook 'eway--quit 0 nil))					

(defvar eway-WM-id nil "comp-supplied unique id for a WM-window")
(make-variable-buffer-local 'eway-WM-id)
(set-default 'eway-wm-id nil)

;; keybinds
(global-set-key (kbd "M-D") (lambda (cmd) (interactive "MCommand: ") (start-process cmd nil cmd)))

;; maintaining this plist requires org-plist-delete, otherwise we have
;; a memory leak. Not a big deal but something to remember (TODO write my own to avoid the dep)
(defvar eway--WM-window-plist nil
  "plist mapping comp-supplied WM window id numbers with emacs buffers that represent them")

(defvar eway-ipc-sock-path "/home/tmu/ewaySock" "Where to look for eway socket")

(defvar eway-default-buffer-name "*untitled-eway*" "base buffer name before a name is set by user or application")

(defvar eway--socket nil "socket used by an open eway process")

(defvar eway--unfinished-request "" "if a request is incomplete, it's stored here until it is compeleted")

(defun eway--eway-buffer-from-WM-id (id)
  "get the buffer associated with the id from the plist, or return nil"
  (plist-get eway--WM-window-plist id))

(defun eway--destroy (id)
  "respond to a destroy request from the socket"
  (let ((buf (eway--eway-buffer-from-WM-id id)))
    (when buf
      (kill-buffer buf))))

(defun eway--rename (id name)
  "set the user visible name of this buffer. Can be overwritten by the user if they care. For now it will just be the app_id. This isn't know at time of creation, see wayland standard of xdg_toplevel"
  (let ((buf (eway--eway-buffer-from-WM-id id)))
    (with-current-buffer buf
      (rename-buffer name))))

(defun eway--parse-request ()
  "parse and fufill request in `eway--unfinished-request'"
  (let ((parts (string-split eway--unfinished-request " " t  "[ \t\n]")))
    (cond ((string= (car parts) "NEW")
	   (let ((id (string-to-number (car (cdr parts)))))
	     (eway--make-WM-buffer id)))
	  ((string= (car parts) "DESTROY")
	   (let* ((id (string-to-number (car (cdr parts)))))
	     (eway--destroy id)))
	  ((string= (car parts) "FOCUS")
	   (let* ((id (string-to-number (car (cdr parts)))))
	     (eway--change-focus id)))
	  ((or (string= (car parts) "TITLE")
	       (string= (car parts) "APPID"))
	   (let* ((id (string-to-number (car (cdr parts))))
		  (name (car (cdr (cdr parts)))))
	     (eway--rename id name)))
	  (t
	   (message "Unrecognized ipc request: %S" parts)))))

(defun eway--socket-filter (process input)
  "divide socket input into individual requests and act on them as necessary"
  (when (eq process eway--socket)
    (let ((idx (string-match "\n" input)))
      (if (not idx)
	  (setq eway--unfinished-request (concat eway--unfinished-request input))
	(setq eway--unfinished-request (concat eway--unfinished-request (substring input 0 idx)))
	;; should have exactly one request in `eway--unfinished-request' without it's trailing newline
	(eway--parse-request)
	(setq eway--unfinished-request "")
	;; handle the rest
	(eway--socket-filter process (substring input (+ idx 1) (length input)))))))

(defun eway-init-socket ()
  "attempts to open the socker specified by `eway-ipc-sock-path' and fills `eway--socket' for internal use"
  (setq eway--socket
	(make-network-process
	 :name "eway-socket-process"
	 :buffer "*eway-socket*"
	 :family 'local
	 :remote eway-ipc-sock-path
	 :filter 'eway--socket-filter)))

;; internal functions and helpers

(defun eway--make-WM-buffer (id)
  "make a buffer to represent the WM-window identified by the id. ID and name are supplied by the comp process to start,"
  (let ((buffer (generate-new-buffer eway-default-buffer-name)))
    (with-current-buffer buffer
      ;; since eway-WM-id is always buffer local, we can just set it normally
      (eway-mode)
      (setq eway-WM-id id)
      (setq eway--WM-window-plist (plist-put eway--WM-window-plist id buffer)))
    buffer))

(defun eway--get-WM-id (&optional buffer)
  "get the WM-id associated with the passed (or current) buffer."
  (unless (and buffer (bufferp buffer))
    (setq buffer (current-buffer)))
  ;; (buffer-local-value eway-WM-id buffer) if I understand right I
  ;; can just talk about id as a reg variable and it is interpreted as
  ;; buffer local automatically
  eway-WM-id)

(defun eway--pass-message (msg)
  "wrapper for communicating with the compositor process"
  ;; (let ((minibuffer-message-timeout 0))
    ;; (message "Sent %s" msg))
  (process-send-string eway--socket msg))

(defun eway--release-focus ()
  "indicated that emacs should not recieve further input at this time. Should only be called in a eway buffer"
  (when (eq major-mode 'eway-mode)
    (let ((id (eway--get-WM-id)))
      (when id
	(eway--pass-message (format "RELEASE %d\n" id))))))

(defun eway--change-focus (id)
  "reaction to focus change from comp process. Seems like I specifically need inform resize"
  (let ((buf (eway--eway-buffer-from-WM-id id)))
    (switch-to-buffer buf)
    (eway--inform-resize-translate buf)))

;; shouldn't have to explictly deal with focus passing from the comp /
;; other applications back to emacs. emacs will just see new
;; inputs. When emacs is in focus, it will recieve input, and when it
;; wants to be done and pass focus elsewhere it will indicate that
;; with a release

(defun eway--get-window-box (&optional buffer)
  "give pixel coordiantes for the current eway buffer window. Returns (x y width height) in pixels."
  (unless buffer
    (setq buffer (current-buffer)))
  
  (with-current-buffer buffer 
    (when (eq major-mode 'eway-mode)
      (let* ((we (window-edges (get-buffer-window) nil t t))
	    (r-b (seq-drop we 2))
	    (right (car r-b))
	    (bottom (car (cdr r-b))))
	(setf (car (cdr (cdr we))) (- right (car we)))
	(setf (car (cdr (cdr (cdr we)))) (- bottom (car (cdr we)) (window-mode-line-height (get-buffer-window))))
	we))))

(defun eway--inform-hide (buffer)
  "tells the comp process to make the passed buffer/window invisible"
  (with-current-buffer buffer
    (when (eq major-mode 'eway-mode)
      (let ((id (eway--get-WM-id buffer)))
	(eway--pass-message (format "HIDE %d\n" id))))))

(defun eway--inform-resize-translate (buffer)
  "tells the comp process to update where the WM-window is and how big it is. This will show the given window. Hiding windows should be done with `eway--inform-hide' instead of setting things to zero here."
  (with-current-buffer buffer
    (when (eq major-mode 'eway-mode)
      (let ((id (eway--get-WM-id buffer)))
	(when id
	  (let* ((coors (eway--get-window-box))
		 (x (car coors))
		 (y (car (cdr coors)))
		 (w (car (cdr (cdr coors))))
		 (h (car (cdr (cdr (cdr coors))))))
	    (eway--pass-message (format "REBOX %d %d %d %d %d\n" id x y w h))))))))

(defun eway--inform-close (buffer)
  "singal to the comp process that the related window should be closed. Naturally this means that the comp process should be informed of a killed buffer *before* emacs removes it"
  (with-current-buffer buffer
    (when (eq major-mode 'eway-mode)
      (let ((id (eway--get-WM-id)))
	(eway--pass-message (format "CLOSE %d\n" id))))))

(defun eway--inform-quit ()
  "tell the compositor that it should quit"
  (eway--pass-message "QUIT\n"))

;; hooks and whatnot below

(defun eway--buffer-window-resize (window)
  "handle a eway window changing size (from emacs side) or eway buffer being swapped out of the window"
  ;; (message "resize-hook: resize: %S %S %S" window (window-buffer window) (eway--get-window-box (window-buffer window)))
  (eway--inform-resize-translate (current-buffer))
  
  ;; (message "resize-hook: swap-out: %S %S %S" window (current-buffer) (window-buffer window))
  ;; (eway--inform-hide (current-buffer))
    )

(defun eway--buffer-window-selection (window)
  "handle the selection and deselection of eway buffers"
  (if (eq (current-buffer) (window-buffer window))
      (let ((id (eway--get-WM-id)))
	;; we are switching INTO this eway buffer
	;; (message "switch-into-hook %S %S" window (window-buffer window))
	(eway--inform-resize-translate (current-buffer))
	(eway--release-focus))
    ;; we are switching OUT of this eway buffer, but the window still displays it
    ;; (message "selection-hook: switch-out: %S %S" window (window-buffer window))
    ;; I don't think we need to do anything here
))

(defun eway--buffer-window-change ()
  "handle the window displaying a different buffer. Called with the window / buffer as selected and current"
  (when (buffer-live-p (window-old-buffer))
    (unless (eq (current-buffer) (window-old-buffer))
      (with-current-buffer (window-old-buffer)
	(when (eq major-mode 'eway-mode)
	  ;; (message "buffer-window-change: out: %S %S" (current-buffer) (get-buffer-window))
	  (eway--inform-hide (current-buffer))))
      ;; switching in is handled be resize
      )))

(defun eway--buffer-kill ()
  "handle a eway buffer being killed"
  ;; (message "buffer-kill-hook: %S" (current-buffer))
  (let ((id (eway--get-WM-id (current-buffer))))
    (eway--inform-close (current-buffer))
    (setq eway--WM-window-plist (org-plist-delete eway--WM-window-plist id))))

(defun eway--quit ()
  "hook function for killing the whole emacs process"
  (eway--inform-quit))

(defun eway--ensure-window-unqiueness ()
  "make sure every eway buffer is only visible in one place"
  (let ((eway-bufs (seq-filter
		    (lambda (buf) (with-current-buffer buf (eq major-mode 'eway-mode)))
		    (buffer-list))))
    (seq-do
     (lambda (buf)
       (let* ((window-list (get-buffer-window-list buf nil t))
	      (sorted-list (seq-sort
			    (lambda (a b)
			      (let* ((buf-a (window-buffer a))
				     (time-a (time-convert (with-current-buffer buf-a buffer-display-time)
							   t))
				     (buf-b (window-buffer b))
				     (time-b (time-convert (with-current-buffer buf-b buffer-display-time)
							   t)))
				(if (= (cdr time-a) (cdr time-b))
				    (> (car time-a) (car time-b))
				  ;; can't compare, ticks are diff for some reason
				  t)))
			    window-list)))
	 ;; the above should hopefully sort the windows so that the
	 ;; first one is the one that most recently aquired the eway
	 ;; buffer. What we want to do make the other's display some
	 ;; other buffer. Ideally this would be the most recent one
	 ;; that isn't already spoken for, but for now they will just
	 ;; go to scratch
	 (seq-do
	  (lambda (win) (set-window-buffer win "*scratch*"))
	  (seq-drop sorted-list 1))))
     eway-bufs)))


