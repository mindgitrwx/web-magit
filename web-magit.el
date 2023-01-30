(defun get-github-repo-url ()
  "Get the URL of the current GitHub repository in the current buffer."
  (interactive)
  (let* ((git-url (shell-command-to-string "git remote -v | grep origin | awk '{print $2}' | head -n1"))
         (git-url (string-trim git-url))
         (message (format "git-url: %s" git-url))
         (web-url (replace-regexp-in-string
                   "\\.git$" "" (replace-regexp-in-string
                                 "^git@github.com:" "https://github.com/" git-url)))
         (message (format "web-url: %s" web-url)))
    web-url))

(defun open-github-repo ()
  "Open the current Github repository on browser"
  (interactive)
  (browse-url (get-github-repo-url)))

(defun open-github-repo-issues ()
  "Open the current Github issue on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/issues")))

(defun open-github-repo-pull-requests ()
  "Open the current Github pull-requests on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/pulls")))

(defun open-github-repo-actions ()
  "Open the current Github actions on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/actions")))

(defun open-github-repo-file ()
  "Open the current Github files on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/blob/master/" (file-name-nondirectory(buffer-file-name)))))

(defun create-github-repo (repo-name &optional username token)
  "Create a new repository on GitHub with the given REPO-NAME.

Optionally, specify a USERNAME and TOKEN to use for authentication. If not
provided, the user will be prompted to enter them."
  (interactive "sRepository name: ")
  (unless username
    (setq username (read-string "GitHub username: ")))
  (unless token
    (setq token (read-passwd "GitHub personal access token: ")))
  (let ((url (format "https://api.github.com/user/repos -u %s:%s" username token)))
    (call-process-shell-command (format "curl -d '{\"name\":\"%s\"}' %s" repo-name url)))

  (sleep-for 1)
  (call-process-shell-command (format "take %s" repo-name))
  (call-process-shell-command (format "echo \"# %s\" >> README.md" repo-name))
  (call-process-shell-command "git init")
  (call-process-shell-command "git add README.md")
  (call-process-shell-command (format "git commit -m \"Initial commit\""))
  (call-process-shell-command "git branch -M main")
  (call-process-shell-command (format "git remote add origin https://github.com/%s/%s.git" username repo-name))
  (call-process-shell-command "git push -u origin main"))

