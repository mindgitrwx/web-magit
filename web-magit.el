(defun my-get-github-repo-url ()
  (let* ((git-dir-p (vc-git-root (buffer-file-name)))
         (git-url (vc-git--run-command-string
                   git-dir-p "config" "--get" "remote.origin.url"))
         (git-url (replace-regexp-in-string "\n$" "" git-url))
         (web-url (replace-regexp-in-string
                   "\\.git$" "" (replace-regexp-in-string
                                 "^git@github.com:" "https://github.com/" git-url))))
    web-url))

(defun my-open-github-repo ()
  (interactive)
  (browse-url (my-get-github-repo-url)))

(defun my-open-github-repo-issues ()
  (interactive)
  (browse-url (concat (my-get-github-repo-url) "/issues")))

(defun my-open-github-repo-pulls ()
  (interactive)
  (browse-url (concat (my-get-github-repo-url) "/pulls")))

(defun my-open-github-repo-actions ()
  (interactive)
  (browse-url (concat (my-get-github-repo-url) "/actions")))

(defun my-open-github-repo-actions ()
  (interactive)
  (browse-url (concat (my-get-github-repo-url) "/actions")))

(defun my-open-github-repo-file ()
  (interactive)
  (browse-url (concat (my-get-github-repo-url) "/blob/master/" ( file-name-nondirectory(buffer-file-name) ))))

(defun my-create-github-repo (repo-name &optional username token)
  "Create a new repository on GitHub with the given REPO-NAME.

Optionally, specify a USERNAME and TOKEN to use for authentication. If not
provided, the user will be prompted to enter them."
  (interactive "sRepository name: ")
  (unless username
    (setq username (read-string "GitHub username: ")))
  (unless token
    (setq token (read-passwd "GitHub personal access token: ")))
  (let ((url (format "https://api.github.com/user/repos -u %s:%s" username token)))
    (call-process-shell-command (format "curl -d '{\"name\":\"%s\"}' %s" repo-name url))))

