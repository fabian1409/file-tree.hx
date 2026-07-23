(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "helix/static.scm")
(require "helix/ext.scm")
(require "helix/configuration.scm")
(require (prefix-in helix. "helix/commands.scm"))

(define (file-tree-info msg)
  (set-status! msg))

(define (file-tree-error msg)
  (set-status! msg))

(define *file-tree-width* 32) ;
(define *file-tree-min-width* 16)
(define *file-tree-max-width* 60)
(define *file-tree-show-separator?* #t)

;; fixed icon set (no external glyph/nerd-font module dependency)
(define *file-tree-icon-dir-open* " ")
(define *file-tree-icon-dir-closed* " ")
(define *file-tree-icon-file* " ")
(define *file-tree-icon-arrow-down* " ")
(define *file-tree-icon-arrow-right* " ")

;; indent-guide connectors for nested entries (ported from render_file_tree in helix)
(define *file-tree-icon-vertical* "│")
(define *file-tree-icon-bottom-left* "└")

(define (file-tree-git-status-icon status)
  (cond
    [(equal? status 'untracked) "?"]
    [(equal? status 'added) "A"]
    [(equal? status 'deleted) "D"]
    [(equal? status 'renamed) "R"]
    [(equal? status 'modified) "M"]
    [else " "]))

;; reuses the same theme scopes as the gutter diff indicators, so git-status
;; colors in the tree match the ones already used for buffer diffs
(define (file-tree-git-status-scope status)
  (cond
    [(or (equal? status 'untracked) (equal? status 'added)) "diff.plus.gutter"]
    [(equal? status 'deleted) "diff.minus.gutter"]
    [(or (equal? status 'renamed) (equal? status 'modified)) "diff.delta.gutter"]
    [else #f]))

;; overrides just the foreground of base with whatever color the given theme
;; scope resolves to, leaving background/modifiers untouched; falls back to
;; base unchanged if the scope has no foreground defined
(define (file-tree-tinted-style base scope-name)
  (define c (style->fg (theme-scope-ref scope-name)))
  (if c (style-fg base c) base))

;; mirrors Helix's file-picker config so file-tree hides the same entries the
;; builtin file picker/explorer would
(define (file-tree-config-bool option default)
  (with-handler (lambda (_) default) (get-config-option-value option)))

;; dotfiles and git-ignored entries are hidden by default, following whatever
;; the file-picker.hidden/git-ignore/git-exclude/git-global options say
(define *file-tree-show-hidden* #f)
(define *file-tree-show-git-ignored* #f)
(define *file-tree-git-ignored-set* (hashset))

(define (file-tree-sync-config-defaults!)
  (set! *file-tree-show-hidden* (not (file-tree-config-bool "file-picker.hidden" #t)))
  (set! *file-tree-show-git-ignored*
        (not (or (file-tree-config-bool "file-picker.git-ignore" #t)
                 (file-tree-config-bool "file-picker.git-exclude" #t)
                 (file-tree-config-bool "file-picker.git-global" #t)))))

(define (file-tree-dotfile? name)
  (and (> (string-length name) 0) (char=? (string-ref name 0) #\.)))

(define (file-tree-git-repo? dir)
  (let ([proc (~> (command "git" (list "-C" dir "rev-parse" "--is-inside-work-tree"))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (and (Ok? proc)
         (string=? (trim (read-port-to-string (child-stdout (Ok->value proc)))) "true"))))

(define *file-tree-git-status-map* (hash))

;; classifies code it modifiles added deleted and renames
(define (file-tree-git-status-symbol code)
  (define x (string-ref code 0))
  (define y (string-ref code 1))
  (cond
    [(and (char=? x #\?) (char=? y #\?)) 'untracked]
    [(or (char=? x #\A) (char=? y #\A)) 'added]
    [(or (char=? x #\D) (char=? y #\D)) 'deleted]
    [(or (char=? x #\R) (char=? y #\R)) 'renamed]
    [(or (char=? x #\M) (char=? y #\M)) 'modified]
    [else #f]))

(define (file-tree-status-path rest)
  (define parts (split-many rest " -> "))
  (trim-end-matches (if (> (length parts) 1) (list-ref parts (- (length parts) 1)) rest)
                     (path-separator)))

(define (file-tree-parse-git-status-lines lines)
  (let loop ([ls lines] [ign (hashset)] [statuses (hash)])
    (if (null? ls)
        (cons ign statuses)
        (let ([line (car ls)])
          (if (< (string-length line) 3)
              (loop (cdr ls) ign statuses)
              (let* ([code (substring line 0 2)]
                     [path (file-tree-status-path (trim (substring line 3 (string-length line))))])
                (if (string=? code "!!")
                    (loop (cdr ls) (hashset-insert ign path) statuses)
                    (let ([sym (file-tree-git-status-symbol code)])
                      (loop (cdr ls) ign (if sym (hash-insert statuses path sym) statuses))))))))))

;; recomputes which workspace-relative paths git considers ignored
(define (file-tree-scan-git-ignored! root)
  (define parsed
    (with-handler
      (lambda (_) (cons (hashset) (hash)))
      (if (not (file-tree-git-repo? root))
          (cons (hashset) (hash))
          (let ([proc (~> (command "git" (list "-C" root "status" "--porcelain" "--ignored=matching"))
                          with-stdout-piped
                          with-stderr-piped
                          spawn-process)])
            (if (Ok? proc)
                (let* ([output (read-port-to-string (child-stdout (Ok->value proc)))]
                       [lines (filter (lambda (l) (> (string-length l) 0)) (split-many output "\n"))])
                  (file-tree-parse-git-status-lines lines))
                (cons (hashset) (hash)))))))
  (set! *file-tree-git-ignored-set* (car parsed))
  (set! *file-tree-git-status-map* (cdr parsed)))

(define *file-tree-active* #f)
(define *file-tree-focused* #f)
(define *file-tree-tree* '())
(define *file-tree-cursor* 0)
(define *file-tree-window-start* 0)
(define *file-tree-visible-height* 30)
(define *file-tree-directories* (hash))

(define *file-tree-default-keybinds*
  (hash 'down "j"
        'up "k"
        'create "n"
        'rename "r"
        'delete "d"
        'refresh "R"
        'toggle-hidden "."
        'toggle-git-ignored "i"
        'wider "+"
        'narrower "-"
        'quit "q"))

(define *file-tree-keybinds* *file-tree-default-keybinds*)

;; looks up which action (if any) a keypress is bound to
(define (file-tree-action-for-char ch)
  (define s (string ch))
  (let loop ([ks (hash-keys->list *file-tree-keybinds*)])
    (cond
      [(null? ks) #f]
      [(equal? (hash-try-get *file-tree-keybinds* (car ks)) s) (car ks)]
      [else (loop (cdr ks))])))

(provide file-tree-open)
(provide file-tree-close)
(provide file-tree-set-keybinds!)

;;@doc
;; Override any subset of file-tree's keybindings from init.scm
;; (file-tree-set-keybinds! (hash 'rename "R" 'refresh "r"))
(define (file-tree-set-keybinds! overrides)
  (set! *file-tree-keybinds*
        (let loop ([ks (hash-keys->list overrides)] [acc *file-tree-keybinds*])
          (if (null? ks)
              acc
              (loop (cdr ks) (hash-insert acc (car ks) (hash-try-get overrides (car ks))))))))

;; keep the panel off the rows moka's bars uses
(define *file-tree-reserved-top-fn* 'unresolved)
(define *file-tree-reserved-bottom-fn* 'unresolved)

(define (file-tree-resolve-reserved!)
  (when (equal? *file-tree-reserved-top-fn* 'unresolved)
    (set! *file-tree-reserved-top-fn* (with-handler (lambda (_) #f) (eval 'moka-reserved-top)))
    (set! *file-tree-reserved-bottom-fn* (with-handler (lambda (_) #f) (eval 'moka-reserved-bottom)))))

(define (file-tree-reserved-top)
  (file-tree-resolve-reserved!)
  (if *file-tree-reserved-top-fn* (with-handler (lambda (_) 0) (*file-tree-reserved-top-fn*)) 0))

(define (file-tree-reserved-bottom)
  (file-tree-resolve-reserved!)
  (if *file-tree-reserved-bottom-fn* (with-handler (lambda (_) 0) (*file-tree-reserved-bottom-fn*)) 0))

(define (file-tree-take lst n)
  (if (or (null? lst) (<= n 0)) '() (cons (car lst) (file-tree-take (cdr lst) (- n 1)))))

(define (file-tree-drop lst n)
  (if (or (null? lst) (<= n 0)) lst (file-tree-drop (cdr lst) (- n 1))))

(define (file-tree-truncate s max-w)
  (if (<= (string-length s) max-w)
      s
      (string-append (substring s 0 (max 0 (- max-w 1))) "…")))

(define (file-tree-repeat-str s n)
  (if (<= n 0) "" (string-append s (file-tree-repeat-str s (- n 1)))))

;; strips the workspace prefix so prompts show a short path instead of the full one
(define (file-tree-relpath path)
  (define prefix (string-append (helix-find-workspace) (path-separator)))
  (if (and (>= (string-length path) (string-length prefix))
           (equal? (substring path 0 (string-length prefix)) prefix))
      (substring path (string-length prefix) (string-length path))
      path))

;; cached lazily, same pattern as *file-tree-reserved-top-fn*
(define *file-tree-home-dir* 'unresolved)

(define (file-tree-home-dir)
  (when (equal? *file-tree-home-dir* 'unresolved)
    (set! *file-tree-home-dir*
          (with-handler
            (lambda (_) #f)
            (let ([proc (~> (command "sh" (list "-c" "printf %s \"$HOME\""))
                            with-stdout-piped
                            with-stderr-piped
                            spawn-process)])
              (and (Ok? proc)
                   (let ([home (trim (read-port-to-string (child-stdout (Ok->value proc))))])
                     (and (> (string-length home) 0) home)))))))
  *file-tree-home-dir*)

;; collapses the home dir prefix into `~`, e.g. /home/foo/bar -> ~/bar
(define (file-tree-fold-home path)
  (define home (file-tree-home-dir))
  (if (and home
           (>= (string-length path) (string-length home))
           (equal? (substring path 0 (string-length home)) home))
      (string-append "~" (substring path (string-length home) (string-length path)))
      path))

(define (file-tree-git-ignored? path)
  (hashset-contains? *file-tree-git-ignored-set* (file-tree-relpath path)))

(define (file-tree-git-status path)
  (hash-try-get *file-tree-git-status-map* (file-tree-relpath path)))

;; dirs before files, alphabetic oder
(define (file-tree-sort-entries lst)
  (define dirs (sort (filter is-dir? lst) string<?))
  (define files (sort (filter (lambda (p) (not (is-dir? p))) lst) string<?))
  (append dirs files))

(define (file-tree-dir-marker path)
  (if (hash-contains? *file-tree-directories* path)
      (if (hash-try-get *file-tree-directories* path)
          *file-tree-icon-arrow-right*
          *file-tree-icon-arrow-down*)
      *file-tree-icon-arrow-right*))

;; which folder icon to show: closed (collapsed) vs open (expanded)
(define (file-tree-dir-icon-for path)
  (if (hash-try-get *file-tree-directories* path)
      *file-tree-icon-dir-closed*
      *file-tree-icon-dir-open*))

;; builds each row's indent-guide prefix, using the same depth numbering as
;; helix's render_file_tree: depth 0 is the workspace root itself (rendered
;; as a plain heading, no guide/icon at all). every other depth adds a fixed
;; 2-column step: depth 1 is just the dir arrow (or a blank placeholder for
;; files), and depth 2+ additionally prepends one vertical continuation per
;; ancestor level above the immediate parent, then this row's own connector
;; - the dir arrow again, or (for files) a vertical/corner depending on
;; whether another entry follows at the same depth. both parts are additive,
;; not exclusive, so indentation grows by one full step per depth level
(define (file-tree-guide-for path depth next-depth)
  (define dir? (is-dir? path))
  (if (= depth 0)
      ""
      (string-append
       (if (and dir? (= depth 1)) (file-tree-dir-marker path) "  ")
       (if (> depth 1)
           (string-append
            (file-tree-repeat-str (string-append *file-tree-icon-vertical* " ") (- depth 2))
            (if dir?
                (file-tree-dir-marker path)
                (string-append
                 (if (and next-depth (< next-depth depth))
                     *file-tree-icon-bottom-left*
                     *file-tree-icon-vertical*)
                 " ")))
           ""))))

;; second pass over the flat (path depth name) list: needs to see each
;; entry's successor to know whether its own branch has ended. depth is
;; carried through so the renderer can spot the root row (depth 0) and
;; style it differently
(define (file-tree-compute-guides items)
  (let loop ([items items] [acc '()])
    (if (null? items)
        (reverse acc)
        (let* ([cur (car items)]
               [rest (cdr items)]
               [path (list-ref cur 0)]
               [depth (list-ref cur 1)]
               [name (list-ref cur 2)]
               [next-depth (and (not (null? rest)) (list-ref (car rest) 1))]
               [guide (file-tree-guide-for path depth next-depth)])
          (loop rest (cons (list path guide name depth) acc))))))

(define (file-tree-build-tree!)
  (define result '())
  (define (walk path depth)
    (define name (file-name path))
    (unless (or (and (not *file-tree-show-hidden*) (file-tree-dotfile? name))
                (and (not *file-tree-show-git-ignored*) (file-tree-git-ignored? path)))
      (define display-name (if (= depth 0) (file-tree-fold-home path) name))
      (set! result (cons (list path depth display-name) result))
      (when (is-dir? path)
        ;; the workspace root (depth 0) starts expanded; every other
        ;; directory collapses by default
        (unless (hash-contains? *file-tree-directories* path)
          (set! *file-tree-directories* (hash-insert *file-tree-directories* path (> depth 0))))
        (unless (hash-try-get *file-tree-directories* path)
          (for-each (lambda (child) (walk child (+ depth 1)))
                    (file-tree-sort-entries (read-dir path)))))))
  (walk (helix-find-workspace) 0)
  (set! *file-tree-tree* (file-tree-compute-guides (reverse result))))

(define (file-tree-active-count) (length *file-tree-tree*))

(define (file-tree-cursor-down!)
  (define n (file-tree-active-count))
  (when (< *file-tree-cursor* (- n 1))
    (set! *file-tree-cursor* (+ *file-tree-cursor* 1))
    (when (> *file-tree-cursor* (+ *file-tree-window-start* (- *file-tree-visible-height* 1)))
      (set! *file-tree-window-start* (+ *file-tree-window-start* 1)))))

(define (file-tree-cursor-up!)
  (when (> *file-tree-cursor* 0)
    (set! *file-tree-cursor* (- *file-tree-cursor* 1))
    (when (< *file-tree-cursor* *file-tree-window-start*)
      (set! *file-tree-window-start* (- *file-tree-window-start* 1)))))

(define (file-tree-current-entry)
  (and (not (null? *file-tree-tree*))
       (list-ref *file-tree-tree* *file-tree-cursor*)))

;; refreshes the view after an eaction like deletion
(define (file-tree-refresh-all!)
  (define old *file-tree-cursor*)
  (file-tree-build-tree!)
  (set! *file-tree-cursor* (min old (max 0 (- (file-tree-active-count) 1)))))

(define (file-tree-toggle-hidden!)
  (set! *file-tree-show-hidden* (not *file-tree-show-hidden*))
  (file-tree-info (if *file-tree-show-hidden* "file-tree: showing dotfiles" "file-tree: hiding dotfiles"))
  (file-tree-refresh-all!))

(define (file-tree-toggle-git-ignored!)
  (set! *file-tree-show-git-ignored* (not *file-tree-show-git-ignored*))
  (file-tree-info (if *file-tree-show-git-ignored* "file-tree: showing git-ignored" "file-tree: hiding git-ignored"))
  (file-tree-refresh-all!))

(define (file-tree-toggle-dir! path)
  (set! *file-tree-directories*
        (hash-insert *file-tree-directories* path (not (hash-try-get *file-tree-directories* path))))
  (define old *file-tree-cursor*)
  (file-tree-build-tree!)
  (set! *file-tree-cursor* (min old (max 0 (- (length *file-tree-tree*) 1)))))

;; `:open` (and so `helix.open`) always resets the cursor to line 0 after
;; switching, even for an already-open document - it's meant for the
;; `:open path[:line:col]` command line, which defaults to the top when no
;; position is given. to reopen an already-open file at wherever it was
;; left, switch to its existing document directly instead, which leaves its
;; selection untouched
(define (file-tree-doc-for-path path)
  (let loop ([docs (editor-all-documents)])
    (cond
      [(null? docs) #f]
      [(equal? (with-handler (lambda (_) #f) (editor-document->path (car docs))) path) (car docs)]
      [else (loop (cdr docs))])))

(define (file-tree-open-file! path)
  (define existing (file-tree-doc-for-path path))
  (if existing
      (editor-switch-action! existing (Action/Replace))
      (helix.open path)))

(define (file-tree-activate!)
  (define entry (file-tree-current-entry))
  (cond
    [(not entry) event-result/consume]
    [(is-file? (car entry))
     (define path (car entry))
     ;; hand focus to the buffer about to open
     (set! *file-tree-focused* #f)
     (enqueue-thread-local-callback (lambda () (file-tree-open-file! path)))
     event-result/close]
    [(is-dir? (car entry))
     (file-tree-toggle-dir! (car entry))
     event-result/consume]))

(define (file-tree-unfocus!)
  (set! *file-tree-focused* #f))

;; leaves the tree focused but pops it off the stack, so the editor gets input again
;; this has to be reachable from inside file-tree-handle-event-fg directly: while focused,
;; the fg component owns every keypress, so a global leader keymap like space+e never
;; reaches Helix's keymap layer to re-invoke file-tree-open
(define (file-tree-switch-to-editor!)
  (pop-last-component-by-name! "file-tree-fg")
  (file-tree-unfocus!))

(define (file-tree-close!)
  (set! *file-tree-active* #f)
  (set! *file-tree-focused* #f)
  (pop-last-component-by-name! "file-tree-fg")
  (pop-last-component-by-name! "file-tree-bg")
  (enqueue-thread-local-callback
   (lambda ()
     (set-editor-clip-left! 0))))

(define (file-tree-wider!)
  (set! *file-tree-width* (min *file-tree-max-width* (+ *file-tree-width* 2)))
  (helix.redraw '()))

(define (file-tree-narrower!)
  (set! *file-tree-width* (max *file-tree-min-width* (- *file-tree-width* 2)))
  (helix.redraw '()))

(define *file-tree-modal-open?* #f)
(define *file-tree-modal-mode* 'input)
(define *file-tree-modal-label* "")
(define *file-tree-modal-buffer* "")
(define *file-tree-modal-callback* #f)

(struct file-tree-modal-state ())

(define (file-tree-modal-width rect)
  (define content-len (+ (string-length *file-tree-modal-label*) (string-length *file-tree-modal-buffer*)))
  (min (- (area-width rect) 4) (max 40 (+ content-len 4))))

(define (file-tree-modal-origin rect)
  (define w (file-tree-modal-width rect))
  (define x (quotient (- (area-width rect) w) 2))
  (define y (quotient (- (area-height rect) 3) 2))
  (list x y w))

(define (file-tree-modal-render state rect frame)
  (define origin (file-tree-modal-origin rect))
  (define x (list-ref origin 0))
  (define y (list-ref origin 1))
  (define w (list-ref origin 2))
  (define bg-style (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define modal-area (area x y w 3))
  (buffer/clear-with frame modal-area bg-style)
  (block/render frame modal-area (make-block bg-style bg-style "all" "rounded"))
  (define text (string-append *file-tree-modal-label* *file-tree-modal-buffer*))
  (frame-set-string! frame (+ x 1) (+ y 1) (file-tree-truncate text (- w 2)) text-style))

(define (file-tree-modal-cursor-fn state rect)
  (if (equal? *file-tree-modal-mode* 'confirm)
      #f ; single keypress, no caret needed
      (let* ([origin (file-tree-modal-origin rect)]
             [x (list-ref origin 0)]
             [y (list-ref origin 1)])
        (position (+ y 1) (+ x 1 (string-length *file-tree-modal-label*) (string-length *file-tree-modal-buffer*))))))

(define (file-tree-modal-handle-event state event)
  (define ch (key-event-char event))
  (cond
    [(equal? *file-tree-modal-mode* 'confirm)
     (define cb *file-tree-modal-callback*)
     (set! *file-tree-modal-callback* #f)
     (set! *file-tree-modal-open?* #f)
     (when cb (enqueue-thread-local-callback (lambda () (cb (and (char? ch) (equal? ch #\y))))))
     event-result/close]
    [(key-event-enter? event)
     (define result *file-tree-modal-buffer*)
     (define cb *file-tree-modal-callback*)
     (set! *file-tree-modal-callback* #f)
     (set! *file-tree-modal-open?* #f)
     (when cb (enqueue-thread-local-callback (lambda () (cb result))))
     event-result/close]
    [(key-event-escape? event)
     (set! *file-tree-modal-callback* #f)
     (set! *file-tree-modal-open?* #f)
     event-result/close]
    [(key-event-backspace? event)
     (define len (string-length *file-tree-modal-buffer*))
     (when (> len 0)
       (set! *file-tree-modal-buffer* (substring *file-tree-modal-buffer* 0 (- len 1))))
     event-result/consume]
    [(char? ch)
     (set! *file-tree-modal-buffer* (string-append *file-tree-modal-buffer* (string ch)))
     event-result/consume]
    [else event-result/consume]))

(define (file-tree-show-modal! mode label initial-value callback)
  (set! *file-tree-modal-open?* #t)
  (set! *file-tree-modal-mode* mode)
  (set! *file-tree-modal-label* label)
  (set! *file-tree-modal-buffer* initial-value)
  (set! *file-tree-modal-callback* callback)
  (push-component!
   (new-component! "file-tree-modal"
                   (file-tree-modal-state)
                   file-tree-modal-render
                   (hash "handle_event" file-tree-modal-handle-event
                         "cursor" file-tree-modal-cursor-fn))))

;; shells out to mv mkdir since steel has no rename builtin
(define (file-tree-run-mv! from-path to-path)
  (let ([proc (~> (command "mv" (list from-path to-path))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (if (Ok? proc)
        (let ([stderr (read-port-to-string (child-stderr (Ok->value proc)))])
          (when (not (string=? (trim stderr) ""))
            (error (trim stderr))))
        (error "mv: could not spawn process"))))

(define (file-tree-run-mkdir-p! path)
  (let ([proc (~> (command "mkdir" (list "-p" path))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (if (Ok? proc)
        (let ([stderr (read-port-to-string (child-stderr (Ok->value proc)))])
          (when (not (string=? (trim stderr) ""))
            (error (trim stderr))))
        (error "mkdir: could not spawn process"))))

(define (file-tree-prompt-create!)
  (define entry (file-tree-current-entry))
  (when entry
    (define path (car entry))
    (define base (if (is-dir? path)
                      (string-append path (path-separator))
                      (trim-end-matches path (file-name path))))
    (enqueue-thread-local-callback
     (lambda ()
       (file-tree-show-modal!
        'input
        (string-append "New (end with " (path-separator) " for dir): ")
        (file-tree-relpath base)
        (lambda (name)
          (define full (string-append (helix-find-workspace) (path-separator) name))
          (with-handler
            (lambda (err) (file-tree-error (string-append "create failed: " (error-object-message err))))
            (begin
              (if (ends-with? name (path-separator))
                  (file-tree-run-mkdir-p! full)
                  (begin
                    (helix.vsplit-new)
                    (helix.open full)
                    (helix.write full)
                    (helix.quit)))
              (file-tree-info (string-append "created " name))))
          (enqueue-thread-local-callback file-tree-refresh-all!)))))))

(define (file-tree-prompt-rename!)
  (define entry (file-tree-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define dir (trim-end-matches path (string-append (path-separator) name)))
    (enqueue-thread-local-callback
     (lambda ()
       (file-tree-show-modal!
        'input
        "Rename: "
        name
        (lambda (new-name)
          (when (and (not (equal? new-name "")) (not (equal? new-name name)))
            (with-handler
              (lambda (err) (file-tree-error (string-append "rename failed: " (error-object-message err))))
              (begin
                (file-tree-run-mv! path (string-append dir (path-separator) new-name))
                (file-tree-info (string-append "renamed " name " -> " new-name))))
            (enqueue-thread-local-callback file-tree-refresh-all!))))))))

(define (file-tree-prompt-delete!)
  (define entry (file-tree-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define kind (if (is-dir? path) "directory" "file"))
    (enqueue-thread-local-callback
     (lambda ()
       (file-tree-show-modal!
        'confirm
        (string-append "Delete " kind " '" name "'? (y/N) ")
        ""
        (lambda (confirmed?)
          (when confirmed?
            (with-handler
              (lambda (err) (file-tree-error (string-append "delete failed: " (error-object-message err))))
              (begin
                (if (is-dir? path)
                    (delete-directory! path) ; only works if empty
                    (delete-file! path))
                (file-tree-info (string-append "deleted " name))))
            (enqueue-thread-local-callback file-tree-refresh-all!))))))))

(struct file-tree-bg-state ())

(define (file-tree-render-bg state rect frame)
  (define w (min *file-tree-width* (area-width rect)))
  (define h (area-height rect))
  (define x0 0)
  ;; panel spans only the rows not reserved by the bars
  (define y0 (file-tree-reserved-top))
  (define panel-h (max 1 (- h y0 (file-tree-reserved-bottom))))
  (set! *file-tree-visible-height* (max 1 panel-h))
  (set-editor-clip-left! w)

  ;; theme components
  (define bg-style (theme-scope-ref "ui.background"))
  (define dir-text-style (theme-scope-ref "ui.file-tree.dir-text"))
  (define dir-icon-style (theme-scope-ref "ui.file-tree.dir-icon"))
  (define file-text-style (theme-scope-ref "ui.file-tree.file-text"))
  (define file-icon-style (theme-scope-ref "ui.file-tree.file-icon"))
  (define indent-guide-style (theme-scope-ref "ui.file-tree.indent-guide"))
  (define root-text-style (theme-scope-ref "ui.file-tree.file-text"))
  (define hl-style (theme-scope-ref "ui.selection"))
  (define border-style (theme-scope-ref "ui.window"))

  ;; no border for cleaner look
  (define panel-area (area x0 y0 w panel-h))
  (buffer/clear-with frame panel-area bg-style)

  ;; line marking the boundary with the text buffer, spanning the panel row
  (when *file-tree-show-separator?*
    (define sep-x (- w 1))
    (when (and (>= sep-x 0) (< sep-x (area-width rect)))
      (let loop ([y y0])
        (when (< y (+ y0 panel-h))
          (frame-set-string! frame sep-x y "│" border-style)
          (loop (+ y 1))))))

  (define list-y0 y0)
  (define max-text-w (- w 2))

  (let ([visible (file-tree-take (file-tree-drop *file-tree-tree* *file-tree-window-start*)
                               *file-tree-visible-height*)])
    (let loop ([items visible] [row 0])
      (unless (or (null? items) (>= row *file-tree-visible-height*))
        (define entry (car items))
        (define abs-idx (+ *file-tree-window-start* row))
        (define path (list-ref entry 0))
        (define prefix (list-ref entry 1))
        (define name (list-ref entry 2))
        (define depth (list-ref entry 3))
        (define root? (= depth 0))
        (define dir? (is-dir? path))
        (define icon (cond [root? ""] [dir? (file-tree-dir-icon-for path)] [else *file-tree-icon-file*]))
        (define git-status (and (not dir?) (file-tree-git-status path)))
        (define git-icon (if git-status (file-tree-git-status-icon git-status) " "))
        (define y (+ list-y0 row))
        (define hl? (and *file-tree-focused* (= abs-idx *file-tree-cursor*)))
        (define row-style (cond [root? root-text-style] [dir? dir-text-style] [else file-text-style]))
        (define icon-style (cond [dir? dir-icon-style] [else file-icon-style]))
        (define git-style
          (let ([scope (and git-status (file-tree-git-status-scope git-status))])
            (if scope (file-tree-tinted-style row-style scope) row-style)))
        (define prefix-w (string-length prefix))
        (define icon-w (string-length icon))
        (define name-x (+ x0 prefix-w icon-w))
        (define git-col-x (- w 2)) ; rightmost content column, right next to the border
        (define avail (max 0 (- git-col-x name-x 1)))
        (when hl?
          (frame-set-string! frame x0 y (make-string (- w 1) #\space) hl-style))
        (frame-set-string! frame x0 y prefix indent-guide-style)
        (frame-set-string! frame (+ x0 prefix-w) y icon icon-style)
        (frame-set-string! frame name-x y (file-tree-truncate name avail) row-style)
        (unless dir?
          (frame-set-string! frame git-col-x y git-icon git-style))
        (loop (cdr items) (+ row 1))))))

(define (file-tree-handle-event-bg state event)
  ;; makes the editor receive events while the panel is unfocused
  event-result/ignore)

(struct file-tree-fg-state ())

(define (file-tree-render-fg state rect frame) void) ; bg handles all drawing

(define (file-tree-command-action! action)
  (cond
    [(equal? action 'down) (file-tree-cursor-down!) event-result/consume]
    [(equal? action 'up) (file-tree-cursor-up!) event-result/consume]
    [(equal? action 'create) (file-tree-prompt-create!) event-result/consume]
    [(equal? action 'rename) (file-tree-prompt-rename!) event-result/consume]
    [(equal? action 'delete) (file-tree-prompt-delete!) event-result/consume]
    [(equal? action 'refresh) (file-tree-refresh-all!) event-result/consume]
    [(equal? action 'toggle-hidden) (file-tree-toggle-hidden!) event-result/consume]
    [(equal? action 'toggle-git-ignored) (file-tree-toggle-git-ignored!) event-result/consume]
    [(equal? action 'wider) (file-tree-wider!) event-result/consume]
    [(equal? action 'narrower) (file-tree-narrower!) event-result/consume]
    [(equal? action 'quit) (file-tree-close!) event-result/close]
    [else event-result/consume]))

(define (file-tree-handle-event-command state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-down? event) (file-tree-cursor-down!) event-result/consume]
    [(key-event-up? event) (file-tree-cursor-up!) event-result/consume]
    [(key-event-enter? event) (file-tree-activate!)]
    [(key-event-tab? event)
     (define entry (file-tree-current-entry))
     (when (and entry (is-dir? (car entry))) (file-tree-toggle-dir! (car entry)))
     event-result/consume]

    [(key-event-escape? event)
     (file-tree-switch-to-editor!)
     event-result/close] ; pops fg only; bg stays visible

    [(and (char? ch) (equal? ch #\=)) (file-tree-wider!) event-result/consume] ; old alias not remappable

    [(char? ch)
     (define action (file-tree-action-for-char ch))
     (if action (file-tree-command-action! action) event-result/consume)]

    [else event-result/consume])) ; block unknown keys from editor while focused

(define (file-tree-handle-event-fg state event)
  (if *file-tree-modal-open?*
      event-result/ignore
      (file-tree-handle-event-command state event)))

(define (file-tree-make-bg-component)
  (new-component! "file-tree-bg"
                  (file-tree-bg-state)
                  file-tree-render-bg
                  (hash "handle_event" file-tree-handle-event-bg)))

(define (file-tree-make-fg-component)
  (new-component! "file-tree-fg"
                  (file-tree-fg-state)
                  file-tree-render-fg
                  (hash "handle_event" file-tree-handle-event-fg)))

(define (file-tree-open!)
  (cond
    [(not *file-tree-active*)
     (set! *file-tree-active* #t)
     (set! *file-tree-focused* #t)
     (set! *file-tree-cursor* 0)
     (set! *file-tree-window-start* 0)
     (file-tree-sync-config-defaults!)
     (file-tree-scan-git-ignored! (helix-find-workspace))
     (file-tree-build-tree!)
     (push-component! (file-tree-make-bg-component))
     (push-component! (file-tree-make-fg-component))]

    [*file-tree-focused*
     (file-tree-switch-to-editor!)]

    [else
     (set! *file-tree-focused* #t)
     (file-tree-scan-git-ignored! (helix-find-workspace))
     (file-tree-build-tree!)
     (push-component! (file-tree-make-fg-component))]))

;;@doc
;; Open the file tree
(define (file-tree-open)
  (file-tree-open!))

;;@doc
;; Close the file tree
(define (file-tree-close)
  (when *file-tree-active*
    (file-tree-close!)))

