;; something like exwm

(define-derived-mode eway-mode fundamental-mode "eway"
  "Mode for non-emacs 'windows' in the WM sense. Generalizes between emacs buffers and WM application windows"
  nil)


;; open a standing IPC socket with "make-network-process"
;; then pass requests both ways

;; we need a consistent way to refer to other stuff running under the compositor.
;; we can't use pid because an application can have more than one windows
;; for now the compositor will supply a number that it will inform emacs of during the creation of said application WM window

(defvar eway-WM-id nil "comp-supplied unique id for a WM-window")
(make-variable-buffer-local eway-WM-id)
(set-default eway-wm-id nil)

(defvar eway--WM-window-plist nil "plist mapping comp-supplied WM window id numbers with emacs buffers that represent them")

(defun eway--make-WM-buffer (id name)
  "make a buffer to represent the WM-window identified by the id. ID and name are supplied by the comp process to start,"
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      ;; since eway-WM-id is always buffer local, we can just set it normally
      (setq eway-WM-id id)
      (setq eway--WM-window-plist (plist-put eway--WM-window-plist id buffer)))))

(defun eway--get-WM-id (&optional buffer)
  "get the WM-id associated with the passed (or current) buffer."
  (unless (and buffer (bufferp buffer))
    (setq buffer (current-buffer)))
  ;; (buffer-local-value eway-WM-id buffer) if I understand right I
  ;; can just talk about id as a reg variable and it is interpreted as
  ;; buffer local automatically
  eway-WM-id)

(defun eway--pass-message (message)
  "wrapper for commincating with the compositor process"
  ;; IPC stuff with the process
  )

(defun eway-release-focus ()
  "indicated that emacs should not recieve further input at this time. Should only be called in a eway buffer"
  (when (eq major-mode 'eway-mode)
    (let ((id (eway--getWM-id)))
      (when id
	(eway--pass-message (format "RELEASE %d\n" id))))))

;; shouldn't have to explictly deal with focus passing from the comp /
;; other applications back to emacs. emacs will just see new
;; inputs. When emacs is in focus, it will recieve input, and when it
;; wants to be done and pass focus elsewhere it will indicate that
;; with a release

(defun eway--get-window-box ()
  "give pixel coordiantes for the current eway buffer window. Returns (x y width height) in pixels."
  (when (eq major-mode 'eway-mode)
    (window-edges (get-buffer-window) nil t t)))

(defun eway--inform-resize-translate (buffer)
  "tells the comp process to update where the WM-window is and how big it is. This should also hide and show windows that are no longer visible as you would expect."
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

;; write the buffer/window hooks to release focus when moving into a eway window or size changes
